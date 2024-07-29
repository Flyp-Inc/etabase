module User exposing (..)

import Col
import Table
import Time


type alias Record =
    { emailAddress : String
    , role : Col.Col Role
    }


type Role
    = Admin
    | Member


specRole : Table.Spec Record Role
specRole =
    Table.toSpec "Role" (Col.value << .role) <|
        \role ->
            case role of
                Admin ->
                    "Admin"

                Member ->
                    "Member"


config : Table.Config Record
config =
    Table.define
        |> Table.withIndex specRole.index


type alias Table =
    Table.Table Record


new : Time.Posix -> { emailAddress : String, role : Role } -> Record
new timestamp { emailAddress, role } =
    { emailAddress = emailAddress
    , role = Col.init timestamp role
    }
