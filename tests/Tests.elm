module Tests exposing (..)

import Col
import DemoUser
import Expect
import Set
import Table
import Test
import Time


suite : Test.Test
suite =
    Test.describe "etabase tests"
        [ Test.test "should create a record in an empty table" <|
            let
                id : Table.Id DemoUser.Record
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
                table : Table.Table DemoUser.Record
                table =
                    Tuple.second withRecords
            in
            \() ->
                Expect.equal 2 <|
                    (Table.where_ DemoUser.specRole.is DemoUser.Member table
                        |> Table.select identity
                        |> List.length
                    )
        ]



-- setups


init : Table.Table DemoUser.Record
init =
    Table.init


timeZero : Time.Posix
timeZero =
    Time.millisToPosix 0


withRecord : ( Table.Row DemoUser.Record, Table.Table DemoUser.Record )
withRecord =
    Table.init
        |> Table.insert DemoUser.config timeZero (DemoUser.new timeZero { emailAddress = "john@pavlick.dev", role = DemoUser.Admin })


withRecords : ( List (Table.Row DemoUser.Record), Table.Table DemoUser.Record )
withRecords =
    Table.init
        |> (Table.insertMany DemoUser.config timeZero <|
                List.map (DemoUser.new timeZero)
                    [ { emailAddress = "ceo@flypcard.com"
                      , role = DemoUser.Admin
                      }
                    , { emailAddress = "person@web.site"
                      , role = DemoUser.Member
                      }
                    , { emailAddress = "otherPerson@web.site"
                      , role = DemoUser.Member
                      }
                    ]
           )
