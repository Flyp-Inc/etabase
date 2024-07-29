module Table exposing
    ( Table, init
    , Row, Id
    , Config, define
    , Spec, Index, Predicate, toSpec, withIndex
    , cons, consBatch, update, delete
    , getById, where_, filter, select
    )

{-| Represents a "table" semantic, where a "table" is a a dictionary of values that have a unique identity, and a "created at" timestamp.

In a traditional relational database management system, the database engine provides a unique identity and can generate timestamps;
the `Table` type takes responsibility for mediating those operations.


# Types

@docs Table, init

@docs Row, Id


# Configuration

@docs Config, define

@docs Spec, Index, Predicate, toSpec, withIndex


# Commands

@docs cons, consBatch, update, delete


# Queries

@docs getById, where_, filter, select

-}

import Dict
import Murmur3
import Random
import Set
import Task
import Time
import UUID


{-| Opaque type representing a "table".
-}
type Table a
    = Table
        { dict : Dict.Dict String { value : a, createdAt : Time.Posix, indexKeys : Set.Set Int }
        , seed : Random.Seed
        , deleted : Dict.Dict String { value : a, deletedAt : Time.Posix }
        , index : Dict.Dict Int (Set.Set String)
        }


{-| Represents a row created in a `Table a`.
-}
type alias Row a =
    { id : Id a, value : a, createdAt : Time.Posix }


{-| Represents a unique ID for a `Row` in a `Table a`.
-}
type Id a
    = Id String


{-| Create a new `Table`.
-}
init : Table a
init =
    Table
        { dict = Dict.empty
        , seed = Random.initialSeed 0
        , deleted = Dict.empty
        , index = Dict.empty
        }


{-| Configuration details for a table.
-}
type Config a
    = Config (List (Index a))


{-| Define a table's configuration.

This value returns an empty `Config a`, and can be "built" with other functions in this module whose type signature ends in `Config a -> Config a`.

-}
define : Config a
define =
    Config []


{-| A `Spec a b` is a record that contains:

  - An `Index a`, which makes lookups faster for a given column
  - A `Predicate a b`, which allows searching for a `Row a` in a `Table a` by some value `b`, which has the same type as the value indexed

-}
type alias Spec a b =
    { index : Index a
    , predicate : Predicate a b
    }


{-| Represent an index for a table.

Internally, an index is:

  - A unique identifier (i.e., the index name)
  - A map between a property on an `a` and an `Int`

When you _query_ a table, you query it by its indexes. This doesn't mean that you need to create an index for every column! The columns
that you should create indexes for are the columns that will help you identify a record, or identify a group of records by some column that
is a member of a smaller set compared to other columns.

Consider the following:

    module User exposing (..)

    import Table

    type alias Record =
        { emailAddress : String
        , catchphrase : String
        , role : Role
        }

    type Role
        = Admin
        | Member

If we need to run queries against `User.Record` to find all users that are `Admin` users, it would make sense to create an index on the `.role`
column, because the `Role` type is a member of a set that only has two values - `Admin` or `Member`.

It wouldn't make much sense to create an index on the `.catchphrase` column - since that column is likely going to be unique for each user, anyway - so if we really needed to do a query against
the `.catchphrase` column, we would have to consider every record separately, no matter what.

-}
type Index a
    = Index (a -> Int)


{-| An opaque function that acts as an interface between the `Index a` values within a table's `Config a`,
and the type of a column that has an `Index a`.

See usage at [`where_`](#where_) for more information.

-}
type Predicate a b
    = Predicate (b -> Int)


{-| Create an `Index a` and `Predicate a b` by providing a unique name for the index, and a string representation of the indexed value.
-}
toSpec : String -> (a -> b) -> (b -> String) -> Spec a b
toSpec name accessor toString =
    let
        hashSeed : Int
        hashSeed =
            Murmur3.hashString 0 name

        toHashedValue : b -> Int
        toHashedValue col =
            Murmur3.hashString hashSeed <|
                toString col
    in
    { index = Index (\value -> accessor value |> toHashedValue)
    , predicate = Predicate toHashedValue
    }


{-| Add an `Index a` to a `Table a`.
-}
withIndex : Index a -> Config a -> Config a
withIndex idx (Config indexes) =
    Config <| idx :: indexes


{-| Insert a record into a `Table`.
-}
cons : Config a -> Time.Posix -> a -> Table a -> ( Row a, Table a )
cons (Config indexes) timestamp value (Table table) =
    let
        ( uuid, newSeed ) =
            Random.step UUID.generator table.seed

        internalId : String
        internalId =
            UUID.toString uuid

        indexKeys : Set.Set Int
        indexKeys =
            toIndexKeys indexes value

        index : Dict.Dict Int (Set.Set String)
        index =
            addIdToIndexByKeys internalId indexKeys table.index
    in
    ( { id = Id internalId
      , value = value
      , createdAt = timestamp
      }
    , Table
        { table
            | dict =
                Dict.insert internalId
                    { value = value
                    , createdAt = timestamp
                    , indexKeys = indexKeys
                    }
                    table.dict
            , seed = newSeed
            , index = index
        }
    )


{-| Insert many records into a `Table`.
-}
consBatch : Config a -> Time.Posix -> List a -> Table a -> ( List (Row a), Table a )
consBatch config timestamp values table =
    List.foldl
        (\step ( accList, accTable ) ->
            cons config timestamp step accTable
                |> Tuple.mapFirst (\new -> new :: accList)
        )
        ( [], table )
        values


{-| Update a record in a `Table`.

The `Table a` type doesn't track "updated at", since it's assumed that if you care about that, you will wrap each column whose "updated at" time is meaningful, in a `Col a`.

`update` doesn't return a tuple of the updated record and the updated table; `cons` only returns a tuple because `cons` is responsible for creating the `Id a` value.

`update` is an expensive operation, because it rebuilds the indexes for a `Table a`; prefer data structures such that "things that change often" are defined as calls to `cons`,
and "things that only change once in awhile" are handled by calls to `update`.

-}
update : Config a -> Id a -> Time.Posix -> a -> Table a -> Table a
update (Config indexes) (Id internalId) timestamp value (Table table) =
    let
        oldIndexKeys : Set.Set Int
        oldIndexKeys =
            Dict.get internalId table.dict
                |> Maybe.map .indexKeys
                |> Maybe.withDefault Set.empty

        newIndexKeys : Set.Set Int
        newIndexKeys =
            toIndexKeys indexes value

        updatedIndex : Dict.Dict Int (Set.Set String)
        updatedIndex =
            dropIdFromIndexByKeys internalId oldIndexKeys table.index
                |> addIdToIndexByKeys internalId newIndexKeys
    in
    Table
        { table
            | dict = Dict.insert internalId { value = value, createdAt = timestamp, indexKeys = newIndexKeys } table.dict
            , index = updatedIndex
        }


{-| Delete a record from a `Table`.
-}
delete : Config a -> Time.Posix -> Id a -> Table a -> Table a
delete (Config indexes) timestamp (Id internalId) ((Table table) as table_) =
    let
        toUpdatedIndex : Set.Set Int -> Dict.Dict Int (Set.Set String)
        toUpdatedIndex indexKeys =
            dropIdFromIndexByKeys internalId indexKeys table.index
    in
    case Dict.get internalId table.dict of
        Just { value, indexKeys } ->
            Table
                { table
                    | dict = Dict.remove internalId table.dict
                    , index = toUpdatedIndex indexKeys
                    , deleted = Dict.insert internalId { value = value, deletedAt = timestamp } table.deleted
                }

        Nothing ->
            table_


{-| Get a value by its `id`.
-}
getById : Id a -> Table a -> Maybe (Row a)
getById ((Id internalId) as id) (Table table) =
    Dict.get internalId table.dict
        |> Maybe.map
            (\record ->
                { id = id
                , value = record.value
                , createdAt = record.createdAt
                }
            )


{-| Query values in a table based on a `Predicate a b`.
-}
where_ : Predicate a b -> b -> Table a -> Table a
where_ (Predicate toIndexKey) term (Table table) =
    let
        maybeInternalIds : Maybe (Set.Set String)
        maybeInternalIds =
            Dict.get (toIndexKey term) table.index
    in
    case maybeInternalIds of
        Nothing ->
            Table { table | dict = Dict.empty }

        Just internalIds ->
            Table
                { table
                    | dict =
                        Dict.filter
                            (\internalId _ ->
                                Set.member internalId internalIds
                            )
                            table.dict
                }


{-| Query values in a table based on a predicate derived from any column value.

This does _not_ use any of a table's indexes. If you need to use `filter`, then consider either:

  - Creating an `Index a` so that you can have a `Predicate a b` to use with `where_`, or
  - Filtering the result set down as much as possible by `where_` first, before applying `filter`

-}
filter : (a -> Bool) -> Table a -> Table a
filter toPredicate (Table table) =
    Table
        { table
            | dict =
                Dict.filter
                    (\_ { value } ->
                        toPredicate value
                    )
                    table.dict
        }



-- internals


{-| Convert a value to its "index keys" - i.e., unique values within a table's index
-}
toIndexKeys : List (Index a) -> a -> Set.Set Int
toIndexKeys indexes value =
    List.map (\(Index idx) -> idx value) indexes
        |> Set.fromList


{-| Add a record's internal ID to a table's index; the index is represented as a dictionary:

  - the key is a unique hash of the value of a column
  - the value is a set of internal ID values that represent "records where this column's value hashes to the value of the key"

-}
addIdToIndexByKeys : String -> Set.Set Int -> Dict.Dict Int (Set.Set String) -> Dict.Dict Int (Set.Set String)
addIdToIndexByKeys internalId indexKeys index =
    Set.foldl
        (\key ->
            Dict.update key
                (\maybeRecord ->
                    Just <|
                        Set.insert internalId <|
                            case maybeRecord of
                                Nothing ->
                                    Set.empty

                                Just idSet ->
                                    idSet
                )
        )
        index
        indexKeys


{-| Select rows from a `Table a`.

This will operate on _all_ rows in a given `Table a`, so it may be wise to use `where_` and `filter` to narrow the `Table` to _what you actually want_ before calling `select`.

-}
select : (Row a -> b) -> Table a -> List b
select func (Table { dict }) =
    List.map
        (\( id, { value, createdAt } ) ->
            func { id = Id id, value = value, createdAt = createdAt }
        )
        (Dict.toList dict)


{-| Remove a record's internal ID from a table's index
-}
dropIdFromIndexByKeys : String -> Set.Set Int -> Dict.Dict Int (Set.Set String) -> Dict.Dict Int (Set.Set String)
dropIdFromIndexByKeys internalId indexKeys index =
    Set.foldl
        (\key ->
            Dict.update key
                (\maybeRecord ->
                    case maybeRecord of
                        Nothing ->
                            Nothing

                        Just idSet ->
                            let
                                updatedIdSet : Set.Set String
                                updatedIdSet =
                                    Set.remove internalId idSet
                            in
                            if Set.isEmpty updatedIdSet then
                                Nothing

                            else
                                Just updatedIdSet
                )
        )
        index
        indexKeys
