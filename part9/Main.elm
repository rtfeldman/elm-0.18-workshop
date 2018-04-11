port module Main exposing (..)

import Auth
import Html exposing (..)
import Html.Attributes exposing (class, defaultValue, href, property, target)
import Html.Events exposing (..)
import Json.Decode exposing (..)
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
        , init = ( initialModel, githubSearch (getQueryString initialModel.terms) )
        , subscriptions = \_ -> githubResponse decodeResponse
        }


getQueryString : List SearchTerm -> String
getQueryString terms =
    let
        includes =
            List.filterMap onlyInclude terms
    in
    -- See https://developer.github.com/v3/search/#example for how to customize!
    "access_token="
        ++ Auth.token
        ++ "&q="
        ++ String.join "+" includes
        ++ "+language:elm&sort=stars&order=desc"


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
            , githubSearch (getQueryString terms)
            )

        SetQuery query ->
            ( { model | query = query }, Cmd.none )

        HandleSearchResponse results ->
            ( { model | results = results }, Cmd.none )

        HandleSearchError error ->
            ( { model | errorMessage = error }, Cmd.none )

        DeleteById idToDelete ->
            let
                newResults =
                    model.results
                        |> List.filter (\{ id } -> id /= idToDelete)

                newModel =
                    { model | results = newResults }
            in
            ( newModel, Cmd.none )


type Msg
    = Search
    | SetQuery String
    | DeleteById Int
    | HandleSearchResponse (List SearchResult)
    | HandleSearchError (Maybe String)


decodeResponse : Value -> Msg
decodeResponse json =
    -- TODO use decodeValue to decode the response into a Msg.
    --
    -- Hint: look at the definition of Msg and
    -- the definition of responseDecoder
    HandleSearchError (Just "TODO decode the response!")


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


port githubSearch : String -> Cmd msg


port githubResponse : (Value -> msg) -> Sub msg
