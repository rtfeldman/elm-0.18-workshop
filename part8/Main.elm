module Main exposing (..)

import Auth
import Html exposing (..)
import Html.Attributes exposing (class, defaultValue, href, property, target)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Parser exposing ((|.), (|=), Parser, end, ignore, keep, oneOrMore, repeat, symbol, zeroOrMore)


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


includeTerm : Parser SearchTerm
includeTerm =
    Parser.succeed Include
        |. ignore zeroOrMore isSpace
        |= keep oneOrMore (\char -> char /= ' ')


searchTerm : Parser SearchTerm
searchTerm =
    Parser.oneOf
        [ excludeTerm
        , includeTerm
        ]


searchTerms : Parser (List SearchTerm)
searchTerms =
    repeat zeroOrMore searchTerm
        |. ignore zeroOrMore isSpace
        |. end


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = ( initialModel, searchFeed initialModel.terms )
        , subscriptions = \_ -> Sub.none
        }


searchFeed : List SearchTerm -> Cmd Msg
searchFeed terms =
    let
        includes =
            List.filterMap onlyInclude terms

        url =
            "https://api.github.com/search/repositories?access_token="
                ++ Auth.token
                ++ "&q="
                ++ String.join "+" includes
                ++ "+language:elm&sort=stars&order=desc"

        -- HINT: responseDecoder may be useful here.
        request =
            "TODO replace this String with a Request built using http://package.elm-lang.org/packages/elm-lang/http/latest/Http#get"
    in
    -- TODO replace this Cmd.none with a call to Http.send
    -- http://package.elm-lang.org/packages/elm-lang/http/latest/Http#send
    --
    -- HINT: request and HandleSearchResponse may be useful here.
    Cmd.none


onlyInclude : SearchTerm -> Maybe String
onlyInclude term =
    case term of
        Include str ->
            Just str

        Exclude _ ->
            Nothing


onlyExclude : SearchTerm -> Maybe String
onlyExclude term =
    case term of
        Include _ ->
            Nothing

        Exclude str ->
            Just str


responseDecoder : Decoder (List SearchResult)
responseDecoder =
    Json.Decode.at [ "items" ] (Json.Decode.list searchResultDecoder)


searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    Json.Decode.succeed SearchResult
        |> required "id" Json.Decode.int
        |> required "full_name" Json.Decode.string
        |> required "stargazers_count" Json.Decode.int


type alias Model =
    { query : String
    , results : List SearchResult
    , terms : List SearchTerm
    , errorMessage : Maybe String
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
    , errorMessage = Nothing
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
        , viewErrorMessage model.errorMessage
        , ul [ class "results" ] (List.map viewSearchResult model.results)
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage errorMessage =
    case errorMessage of
        Just message ->
            div [ class "error" ] [ text message ]

        Nothing ->
            text ""


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
    | HandleSearchResponse (Result Http.Error (List SearchResult))


termsFromQuery : String -> List SearchTerm
termsFromQuery query =
    case Parser.run searchTerms query of
        Ok validTerms ->
            validTerms

        Err invalidTerms ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            let
                terms =
                    termsFromQuery model.query
            in
            ( { model | terms = terms }
            , searchFeed terms
            )

        HandleSearchResponse result ->
            case result of
                Ok results ->
                    ( { model | results = withoutExcludes model.terms results }
                    , Cmd.none
                    )

                Err error ->
                    -- TODO if decoding failed, store the message in model.errorMessage
                    --
                    -- HINT 1: Remember, model.errorMessage is a Maybe String - so it
                    -- can only be set to either Nothing or (Just "some string here")
                    --
                    -- Hint 2: look for "decode" in the documentation for this union type:
                    -- http://package.elm-lang.org/packages/elm-lang/http/latest/Http#Error
                    --
                    -- Hint 3: to check if this is working, break responseDecoder
                    -- by changing "stargazers_count" to "description"
                    ( model, Cmd.none )

        SetQuery query ->
            ( { model | query = query }, Cmd.none )

        DeleteById idToHide ->
            let
                newResults =
                    model.results
                        |> List.filter (\{ id } -> id /= idToHide)

                newModel =
                    { model | results = newResults }
            in
            ( newModel, Cmd.none )


withoutExcludes : List SearchTerm -> List SearchResult -> List SearchResult
withoutExcludes terms results =
    let
        excludes =
            List.filterMap onlyExclude terms

        containsNoExcludes result =
            let
                shouldExclude exclude =
                    String.contains exclude result.name
            in
            not (List.any shouldExclude excludes)
    in
    List.filter containsNoExcludes results
