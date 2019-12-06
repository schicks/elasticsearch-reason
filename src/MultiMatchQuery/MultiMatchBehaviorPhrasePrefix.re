module PhrasePrefix = {
    open Domain
    open Primitives

    type content = {
        max_expansions: option(positiveInt),
        analyzer: option(string),
        lenient: option(bool),
        zero_terms_query: option(zeroTermsBehavior),
        slop: option(positiveInt) // 0
    }
}