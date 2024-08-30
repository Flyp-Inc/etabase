module Tests exposing (..)

import Col
import Expect
import Set
import Table
import Test
import Time
import User


suite : Test.Test
suite =
    Test.describe "etabase tests"
        [ Test.test "should create a record in an empty table" <|
            let
                id : Table.Id User.Record
                id =
                    withRecord |> Tuple.first |> .id
            in
            \() ->
                Table.getById id (Tuple.second withRecord)
                    |> Result.fromMaybe ()
                    |> Expect.ok
        , Test.test "IDs should all be different" <|
            let
                ids : List String
                ids =
                    Tuple.first (withRecords |> Debug.log "withRecords")
                        |> List.map (.id >> Debug.toString)

                setIds : Set.Set String
                setIds =
                    Set.fromList ids
            in
            \() ->
                Expect.equal (Set.size setIds) (List.length ids)
        , Test.test "Querying by an index should work" <|
            let
                table : Table.Table User.Record
                table =
                    Tuple.second withRecords
            in
            \() ->
                Expect.equal 2 <|
                    (Table.where_ User.specRole.is User.Member table
                        |> Table.select identity
                        |> List.length
                    )
        ]



-- setups


init : Table.Table User.Record
init =
    Table.init


timeZero : Time.Posix
timeZero =
    Time.millisToPosix 0


withRecord : ( Table.Row User.Record, Table.Table User.Record )
withRecord =
    Table.init
        |> Table.insert User.config timeZero (User.new timeZero { emailAddress = "john@pavlick.dev", role = User.Admin })


withRecords : ( List (Table.Row User.Record), Table.Table User.Record )
withRecords =
    Table.init
        |> (Table.insertMany User.config timeZero <|
                List.map (User.new timeZero)
                    [ { emailAddress = "ceo@flypcard.com"
                      , role = User.Admin
                      }
                    , { emailAddress = "person@web.site"
                      , role = User.Member
                      }
                    , { emailAddress = "otherPerson@web.site"
                      , role = User.Member
                      }
                    ]
           )
