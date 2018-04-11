module SearchTerm exposing (SearchTerm(..), excludes, fromQuery, includes)

import Parser exposing (..)


-- TYPES --


type SearchTerm
    = Include String
    | Exclude String



-- FILTERING --


includes : List SearchTerm -> List String
includes terms =
    List.filterMap onlyInclude terms


excludes : List SearchTerm -> List String
excludes terms =
    List.filterMap onlyExclude terms


onlyExclude : SearchTerm -> Maybe String
onlyExclude term =
    case term of
        Include _ ->
            Nothing

        Exclude str ->
            Just str


onlyInclude : SearchTerm -> Maybe String
onlyInclude term =
    case term of
        Include str ->
            Just str

        Exclude _ ->
            Nothing



-- PARSING --


fromQuery : String -> List SearchTerm
fromQuery query =
    case Parser.run searchTerms query of
        Ok validTerms ->
            validTerms

        Err invalidTerms ->
            []


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
