module DemoUser exposing (Record, Role(..), Row, Table, config, new, specRole)

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



-- queries
