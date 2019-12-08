module Cross = {
    open Domain
    open Primitives

    type content = {
        analyzer: option(string),
        operator: option(operator),
        minimum_should_match: option(msmExpression),
        lenient: option(bool),
        zero_terms_query: option(zeroTermsBehavior),
        tie_breaker: option(positiveNumber)
    }

    let noOptions = {
        analyzer: None,
        operator: None,
        minimum_should_match: None,
        lenient: None,
        zero_terms_query: None,
        tie_breaker: None
    }

    let format = (options) => {
        [
            ("analyzer", Belt.Option.map(options.analyzer, Js.Json.string)),
            ("lenient", Belt.Option.map(options.lenient, Js.Json.boolean)),
            ("operator", Belt.Option.map(options.operator, serializeOperator)),
            ("minimum_should_match", Belt.Option.map(options.minimum_should_match, serializeMsm)),
            ("zero_terms_query", Belt.Option.map(options.zero_terms_query, serializeZeroTermsBehavior)),
            ("tie_breaker", Belt.Option.map(options.tie_breaker, serializeTieBreaker))
        ]
        |> Domain.filterNoneSnd
    }
}