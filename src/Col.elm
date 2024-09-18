module Col exposing (..)

{-| HACK: currently exposing `(..)` to make Lamdera shut the fuck up, since we're vendoring this at the moment

here's the correct header:

    module Col exposing
        ( Col
        , cons
        , history
        , init
        , toUpdatedAt
        , touch
        , updatedAt
        , value
        )

END HACK
Represents a "column" semantic, with support for tracking changes over time.


# Type

@docs Col


# Create, update

@docs init, cons, touch


# Access values

@docs value, updatedAt, history


# Aggregation

@docs toUpdatedAt

-}

import Dict
import Task
import Time


{-| Opaque type representing a "column".
-}
type Col a
    = Col ( Time.Posix, a, Dict.Dict Int a )


{-| Create a new `Col a`.
-}
init : Time.Posix -> a -> Col a
init time value_ =
    Col ( time, value_, Dict.empty )


{-| Update a value in a `Col a`, tracking changes.
-}
cons : Time.Posix -> a -> Col a -> Col a
cons t v ((Col ( time, value_, dict )) as col) =
    if v == value_ then
        col

    else
        Col ( t, v, Dict.insert (Time.posixToMillis time) value_ dict )


{-| Update a `Col a`'s timestamp without adding a record to its history.
-}
touch : Time.Posix -> Col a -> Col a
touch t (Col ( _, value_, dict )) =
    Col ( t, value_, dict )


{-| Get a value from a `Col a`.
-}
value : Col a -> a
value (Col ( _, a, _ )) =
    a


{-| Get a `Col a`'s timestamp\`.
-}
updatedAt : Col a -> Time.Posix
updatedAt (Col ( time, _, _ )) =
    time


{-| Get all changes on a `Col a`, in order from newest to oldest.
-}
history : Col a -> List ( Time.Posix, a )
history (Col ( time, value_, dict )) =
    ( time, value_ )
        :: (Dict.toList dict
                |> List.reverse
                |> List.map (Tuple.mapFirst Time.millisToPosix)
           )


{-| For a list of `Col` values on a record `a`, calculate when the record was last updated.
-}
toUpdatedAt : List (a -> Time.Posix) -> Time.Posix -> a -> Time.Posix
toUpdatedAt getters createdAt record =
    Time.millisToPosix <|
        List.foldl Basics.max
            (Time.posixToMillis createdAt)
            (List.map ((|>) record >> Time.posixToMillis) getters)
