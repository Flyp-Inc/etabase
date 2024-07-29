module Table exposing
    ( Table, init
    , Config, define
    , Spec, Index, Predicate, toSpec, withIndex
    , cons, update, delete
    , getById, where_, filter
    )

{-| Represents a "table" semantic, where a "table" is a a dictionary of values that have a unique identity, and a "created at" timestamp.

In a traditional relational database management system, the database engine provides a unique identity and can generate timestamps;
the `Table` type takes responsibility for mediating those operations.


# Type

@docs Table, init


# Configuration

@docs Config, define

@docs Spec, Index, Predicate, toSpec, withIndex


# Commands

@docs cons, update, delete


# Queries

@docs getById, where_, filter

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


{-| Create a new `Table`.

Each table contains a `Random.Seed` that is stepped every time you insert a new record, to generate that record's unique identifier.
How you choose to create that `Random.Seed` value is up to you; in a tranditional relational database, it is not uncommon for identities
to be unique at the _table_ level; so if you don't need your record identities to be _globally_ unique, it would be OK to simply use `Random.initialSeed 0`
as the identity for every instance of `Table` in your application.

-}
init : Table a
init =
    Table
        { dict = Dict.empty
        , seed = Random.initialSeed 0
        , deleted = Dict.empty
        , index = Dict.empty
        }


{-| Represent configuration details for a table.

Internally, configuration is:

    - A mapping between a `String` and the ID type for your table
    - A representation of any indexes

-}
type Config id a
    = Config (String -> id) (id -> String) (List (Index a))


{-| Define a table's configuration.

The minimum-required configuration for a table is "a mapping between a `String` and an ID type". ID values are represented interally as a `String`, so
the path-of-least-resistance here is to just use the `identity` function; but for a tiny bit more effort, you can have an `Id` type that is unique to your table -
and that makes your code quite a bit easier to read, and it gives the compiler more information so that it can keep you from accidentally using the `String`-value of
a row's ID for the wrong thing!

Here's what this could look like:

    ```
    module User exposing (..)

    import Table

    type alias Record =
        { emailAddress : String }

    type Id
        = Id String

    config : Table.Config Id Record
    config =
        Table.define Id
    ```

-}
define : (String -> id) -> (id -> String) -> Config id a
define toId fromId =
    Config toId fromId []


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

    ```
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
    ```

If we need to run queries against `User.Record` to find all users that are `Admin` users, it would make sense to create an index on the `.role`
column, because the `Role` type is a member of a set that only has two values - `Admin` or `Member`. It wouldn't make much sense to create an index
on the `.catchphrase` column - since that column is likely going to be unique for each user, anyway - so if we really needed to do a query against
the `.catchphrase` column, we would have to consider every record separately, no matter what.

-}
type Index a
    = Index (a -> Int)


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
withIndex : Index a -> Config id a -> Config id a
withIndex idx (Config toId fromId indexes) =
    Config toId fromId <| idx :: indexes


{-| Insert a record into a `Table`.

It is assumed that you will use this function in your own code to create a `cons` function for each type `a` that is going to be stored in a `Table a`,
by partially applying the `(String -> id)` function. For instance, in a module `User`, here is what that would look like:

    ```
    module User exposing (Id, Record, cons)

    import Table
    import Time

    type Id
        = Id String


    type alias Table =
        Table.Table Record

    type alias Record =
        { emailAddress : String
        }


    cons : Time.Posix -> Record -> Table -> ( { id : Id, value : Record, createdAt : Time.Posix }, Table )
    cons =
        Table.cons Id
    ```

The nice thing about using `lamdera-extra` is that since there is always a `Time.Posix` value representing the current timestamp available
in the `update` function in `Backend`, this doesn't have to be wrapped in a `Task` or a `Cmd`.

-}
cons : Config id a -> Time.Posix -> a -> Table a -> ( { id : id, value : a, createdAt : Time.Posix }, Table a )
cons (Config toId _ indexes) timestamp value (Table table) =
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
    ( { id = toId internalId
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


{-| Update a record in a `Table`.

It is assumed that you will use this function in your own code to create an `update` function for each type `a` that is going to be stored in a `Table a`,
by partially applying the `String` value. For instance, in a module `User`, here is what that would look like:

    ```
    module User exposing (Id, Record, cons)

    import Table
    import Time

    type Id
        = Id String


    type alias Table =
        Table.Table Record


    type alias Record =
        { emailAddress : String
        }


    update : Id -> Time.Posix -> Record -> Table -> Table
    update (Id id) =
        Table.update id
    ```

The `Table a` type doesn't track "updated at", since it's assumed that if you care about that, you will wrap each column whose "updated at" time is meaningful in a `Col a`.

`update` doesn't return a tuple of the updated record and the updated table; `cons` only returns a tuple because `cons` is responsible for creating the `id` value.

`update` is an expensive operation, because it rebuilds the indexes for a `Table a`; prefer data structures such that "things that change often" are defined as calls to `cons`,
and "things that only change once in awhile" are handled by calls to `update`.

-}
update : Config id a -> id -> Time.Posix -> a -> Table a -> Table a
update (Config toId fromId indexes) id timestamp value (Table table) =
    let
        internalId : String
        internalId =
            fromId id

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
delete : Config id a -> Time.Posix -> id -> Table a -> Table a
delete (Config toId fromId indexes) timestamp id ((Table table) as table_) =
    let
        internalId : String
        internalId =
            fromId id

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
getById : Config id a -> id -> Table a -> Maybe { value : a, createdAt : Time.Posix }
getById (Config _ fromId _) id (Table table) =
    Dict.get (fromId id) table.dict
        |> Maybe.map
            (\record ->
                { value = record.value
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

    - Creating an `Index a` so that you can have a `Predicate b` to use with `where_`, or
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
