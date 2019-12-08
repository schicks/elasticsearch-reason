module BoolPrefix = {
    open Domain
    open Primitives

    type content = {
        rewrite: option(rewriteBehavior), // TopTermsBlendedFreqs(Positive(max_expansions))
        analyzer: option(string),
        fuzziness: option(levenshteinDistance), // Zero
        max_expansions: option(positiveInt), // 50
        prefix_length: option(positiveInt), // 0
        lenient: option(bool), // false
        operator: option(operator), // Or
        minimum_should_match: option(msmExpression), // Single(Number(1))
        zero_terms_query: option(zeroTermsBehavior) // None
    }

    let noOptions = {
        rewrite: None,
        analyzer: None,
        fuzziness: None,
        max_expansions: None,
        prefix_length: None,
        operator: None,
        minimum_should_match: None,
        lenient: None,
        zero_terms_query: None
    }

    let format = (options) => {
        [
            ("rewrite", Belt.Option.map(options.rewrite, serializeRewrite)),
            ("analyzer", Belt.Option.map(options.analyzer, Js.Json.string)),
            ("fuzziness", Belt.Option.map(options.fuzziness, serializeLevenshtein)),
            ("max_expansions", Belt.Option.map(options.max_expansions, serializePositiveInt)),
            ("prefix_length", Belt.Option.map(options.prefix_length, serializePositiveInt)),
            ("operator", Belt.Option.map(options.operator, serializeOperator)),
            ("minimum_should_match", Belt.Option.map(options.minimum_should_match, serializeMsm)),
            ("lenient", Belt.Option.map(options.lenient, Js.Json.boolean)),
            ("zero_terms_query", Belt.Option.map(options.zero_terms_query, serializeZeroTermsBehavior)),
        ]
        |> Domain.filterNoneSnd
    }
}