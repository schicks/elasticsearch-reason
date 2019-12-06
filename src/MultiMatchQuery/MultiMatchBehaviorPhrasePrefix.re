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

    let noOptions = {
        max_expansions: None,
        analyzer: None,
        lenient: None,
        zero_terms_query: None,
        slop: None
    }

    let format = (options) => {
        [
            ("max_expansions", Belt.Option.map(options.max_expansions, serializePositiveInt)),
            ("analyzer", Belt.Option.map(options.analyzer, Js.Json.string)),
            ("lenient", Belt.Option.map(options.lenient, Js.Json.boolean)),
            ("zero_terms_query", Belt.Option.map(options.zero_terms_query, serializeZeroTermsBehavior)),
            ("slop", Belt.Option.map(options.slop, serializePositiveInt))
        ]
        |> List.fold_left((acc, a) => switch (a) {
            | (key, Some(el)) => [(key, el), ...acc]
            | (_, None) => acc
        }, [])
    }
}