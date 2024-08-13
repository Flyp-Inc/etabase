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
