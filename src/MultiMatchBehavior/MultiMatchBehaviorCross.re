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
}