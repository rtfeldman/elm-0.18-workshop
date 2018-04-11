module SearchTermTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import SearchTerm exposing (SearchTerm(..))
import Test exposing (..)


all : Test
all =
    describe "parsing search terms"
        [ test "it finds included and excluded terms" <|
            \() ->
                "beginner tutorial -todo"
                    |> SearchTerm.fromQuery
                    -- TODO change this list being passed to Expect.equal here,
                    -- such that the list contains the corrrect values.
                    --
                    -- HINT: Running `elm test` from the command line will
                    -- give you a test failure message that may be useful!
                    |> Expect.equal [ Include "foo", Exclude "bar" ]
        , test "it makes lists with the same length as the inputs" <|
            \() ->
                let
                    -- TODO convert this to a fuzz test that generates a random
                    -- list of ids instead of this hardcoded list of three ids.
                    --
                    -- fuzz test docs: http://package.elm-lang.org/packages/elm-community/elm-test/latest/Test#fuzz
                    -- Fuzzer docs: http://package.elm-lang.org/packages/elm-community/elm-test/latest
                    floats =
                        [ 1.1, 2.2, 3.3 ]
                in
                floats
                    |> List.map toString
                    |> String.join " "
                    |> SearchTerm.fromQuery
                    -- TODO change the rest of this pipeline to expect that
                    -- the length of the list returned by SearchTerm.fromQuery
                    -- is the same as the length of the floats
                    --
                    -- HINT: the List.length function will help here!
                    -- http://package.elm-lang.org/packages/elm-lang/core/latest#List-length
                    |> Expect.equal []
        ]
