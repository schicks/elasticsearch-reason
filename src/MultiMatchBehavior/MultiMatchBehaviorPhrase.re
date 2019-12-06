module Phrase = {
    open Domain
    open Primitives

    type content = {
        analyzer: option(string),
        lenient: option(bool),
        zero_terms_query: option(zeroTermsBehavior),
        slop: option(positiveInt) // 0
    }
}