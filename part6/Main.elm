module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, defaultValue, href, property, target)
import Html.Events exposing (..)
import Parser exposing (..)


type SearchTerm
    = Include String
    | Exclude String


isSpace : Char -> Bool
isSpace char =
    char == ' '


excludeTerm : Parser SearchTerm
excludeTerm =
    Parser.succeed Exclude
        |. ignore zeroOrMore isSpace
        |. symbol "-"
        |= keep oneOrMore (\char -> char /= ' ')
        |. ignore zeroOrMore isSpace


includeTerm : Parser SearchTerm
includeTerm =
    Parser.fail "TODO: implement this parser!"


searchTerm : Parser SearchTerm
searchTerm =
    Parser.oneOf
        [ excludeTerm
        , includeTerm
        ]


searchTerms : Parser (List SearchTerm)
searchTerms =
    repeat zeroOrMore searchTerm
        |. end


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , update = update
        , model = initialModel
        }


type alias Model =
    { query : String
    , results : List SearchResult
    , terms : List SearchTerm
    }


type alias SearchResult =
    { id : Int
    , name : String
    , stars : Int
    }


initialModel : Model
initialModel =
    { query = "tutorial"
    , results = []
    , terms = termsFromQuery "tutorial"
    }


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "ElmHub" ]
            , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
            ]
        , input [ class "search-query", onInput SetQuery, defaultValue model.query ] []
        , button [ class "search-button", onClick Search ] [ text "Search" ]
        , div []
            [ span [ class "search-terms" ] [ text "Showing results for:" ]
            , span [] (List.map viewSearchTerm model.terms)
            ]
        , ul [ class "results" ]
            (List.map viewSearchResult model.results)
        ]


viewSearchTerm : SearchTerm -> Html Msg
viewSearchTerm term =
    case term of
        Include str ->
            span [ class "search-term included" ] [ text str ]

        Exclude str ->
            span [ class "search-term excluded" ] [ text str ]


viewSearchResult : SearchResult -> Html Msg
viewSearchResult result =
    li []
        [ span [ class "star-count" ] [ text (toString result.stars) ]
        , a [ href ("https://github.com/" ++ result.name), target "_blank" ]
            [ text result.name ]
        , button [ class "hide-result", onClick (DeleteById result.id) ]
            [ text "X" ]
        ]


type Msg
    = SetQuery String
    | DeleteById Int
    | Search


termsFromQuery : String -> List SearchTerm
termsFromQuery query =
    case Parser.run searchTerms query of
        _ ->
            -- TODO if Parser.run returned an Ok containing the List of
            -- SearchTerm values we want, then return that list!
            --
            -- If it returned an Err instead, return [].
            []


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetQuery query ->
            { model | query = query }

        DeleteById idToHide ->
            let
                newResults =
                    List.filter (\{ id } -> id /= idToHide) model.results
            in
            { model | results = newResults }

        Search ->
            -- TODO when the user clicks Search, use `termsFromQuery` and
            -- the model's current `query` to update the model's `terms`.
            { model | terms = [] }
