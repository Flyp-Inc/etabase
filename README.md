# etabase: A relational database semantic for the Elm programming language

One of Lamdera's biggest value propositions is:

> Elm on the frontend. Elm on the backend. Elm in-between. Elm in the shower! Elm at the store! Everything is Elm all the time always.

And that's really great, because you can store your user input from the frontend on the backend without having to deal with any pesky de/serialization.

But guess what: Your data... it forms a hierarchy, doesn't it? A _relational_ hierarchy. You need a relational database. You need support for relational-database-type manoeuvres, such as "updated at" and "change data control" and "automatic identity".

You dust off `Dict` (or `AssocList` if you're feeling fancy) and you just sort of... fight through it. At the end, you have a half-baked, bug-ridden implementation of the worst parts of SQL.

Well, now, you don't have to deal with that anymore. You can just use `flyp-inc/etabase`.

## What can it do?

- It can track timestamps automatically
- It can assign a unique identity to each row on insert
- It can track column history
- It can add indexes to columns for super-fast, typed lookups on non-primary-key values

## What's the catch?

Timestamps come from the runtime, so you'll need to do some `Task.map`ping to get the current timestamp. If you're already doing Lamdera anyway and you want a more holistic solution, check out [flyp-inc/lamdera-extra](https://github.com/Flyp-Inc/lamdera-extra).

## What does it look like to use?

Glad you asked!

```
module User exposing (..)

import Col
import Table
import Time



-- friendly type aliases


type alias Table =
    Table.Table Record


type alias Row =
    Table.Row Record



-- domain types


type alias Record =
    { emailAddress : String
    , role : Col.Col Role
    }


type Role
    = Admin
    | Member



-- table configuration


config : Table.Config Record
config =
    Table.define
        |> Table.withIndex specRole.index
        |> Table.withIndex specEmailAddress.index


specRole : Table.Spec Record Role
specRole =
    Table.toSpec "Role" (Col.value << .role) <|
        \role ->
            case role of
                Admin ->
                    "Admin"

                Member ->
                    "Member"


specEmailAddress : Table.Spec Record String
specEmailAddress =
    Table.toSpec "EmailAddress" .emailAddress identity



-- creation, insertion


new : Time.Posix -> { emailAddress : String, role : Role } -> Record
new timestamp { emailAddress, role } =
    { emailAddress = emailAddress
    , role = Col.init timestamp role
    }


init : Table
init =
    let
        timeZero : Time.Posix
        timeZero =
            Time.millisToPosix 0

        toRecord : String -> Record
        toRecord emailAddress =
            new timeZero { emailAddress = emailAddress, role = Admin }
    in
    Tuple.second <|
        Table.consBatch config timeZero (List.map toRecord [ "ceo@flypcard.com", "cto@flypcard.com" ]) Table.init



-- queries


findByEmailAddress : { emailAddress : String } -> Table -> Maybe Row
findByEmailAddress { emailAddress } table =
    Table.where_ specEmailAddress.predicate emailAddress table
        |> Table.select identity
        |> List.head


getByRole : Role -> Table -> Table
getByRole role =
    Table.where_ specRole.predicate role

```