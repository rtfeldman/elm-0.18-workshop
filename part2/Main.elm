module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


searchResults =
    [ "TheSeamau5/elm-checkerboardgrid-tutorial"
    , "grzegorzbalcerek/elm-by-example"
    , "sporto/elm-tutorial-app"
    , "jvoigtlaender/Elm-Tutorium"
    , "sporto/elm-tutorial-assets"
    ]


viewSearchResult searchResult =
    -- This function will receive `searchResult`, one of the strings
    -- in the list above.
    li []
        [-- TODO use `searchResult` to put a link here that points to
         -- something like this:
         --
         -- https://github.com/TheSeamau5/elm-checkerboardgrid-tutorial
         --
         -- by prepending "https://github.com/" to the searchResult string
         --
         -- HINT: This will also involve using parentheses!
        ]


main =
    let
        elmHubHeader =
            header []
                [ h1 [] [ text "ElmHub" ]
                , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
                ]
    in
    div [ class "content" ]
        [ text "TODO put the contents of elmHubHeader here instead of this text!"
        , ul [ class "results" ]
            -- TODO replace this [] with a `List.map` which uses the
            -- `viewSearchResult` function to turn `searchResults` into some Html!
            --
            -- HINT: You'll need some parentheses to do this!
            []
        ]
