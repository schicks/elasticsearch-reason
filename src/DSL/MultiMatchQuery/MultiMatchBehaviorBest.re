module Best = {
    open Domain
    open Primitives

    type content = {
        fuzzy_transpositions: option(bool), // true
        auto_generate_synonyms_phrase_query: option(bool), // true
        rewrite: option(rewriteBehavior), // TopTermsBlendedFreqs(Positive(max_expansions))
        analyzer: option(string),
        fuzziness: option(levenshteinDistance), // Zero
        max_expansions: option(positiveInt), // 50
        prefix_length: option(positiveInt), // 0
        lenient: option(bool), // false
        operator: option(operator), // Or
        minimum_should_match: option(msmExpression), // Single(Number(1))
        zero_terms_query: option(zeroTermsBehavior), // None
        tie_breaker: option(positiveNumber)
        //TODO boost?
    }

    let noOptions = {
        fuzzy_transpositions: None,
        auto_generate_synonyms_phrase_query: None,
        rewrite: None,
        analyzer: None,
        fuzziness: None,
        max_expansions: None,
        prefix_length: None,
        lenient: None,
        operator: None,
        minimum_should_match: None,
        zero_terms_query: None,
        tie_breaker: None
    }

    let format = (options) => {
        [
            ("fuzzy_transpositions", Belt.Option.map(options.fuzzy_transpositions, Js.Json.boolean)),
            ("auto_generate_synonyms_phrase_query", Belt.Option.map(options.auto_generate_synonyms_phrase_query, Js.Json.boolean)),
            ("rewrite", Belt.Option.map(options.rewrite, serializeRewrite)),
            ("analyzer", Belt.Option.map(options.analyzer, Js.Json.string)),
            ("fuzziness", Belt.Option.map(options.fuzziness, serializeLevenshtein)),
            ("max_expansions", Belt.Option.map(options.max_expansions, serializePositiveInt)),
            ("prefix_length", Belt.Option.map(options.prefix_length, serializePositiveInt)),
            ("lenient", Belt.Option.map(options.lenient, Js.Json.boolean)),
            ("operator", Belt.Option.map(options.operator, serializeOperator)),
            ("minimum_should_match", Belt.Option.map(options.minimum_should_match, serializeMsm)),
            ("zero_terms_query", Belt.Option.map(options.zero_terms_query, serializeZeroTermsBehavior)),
            ("tie_breaker", Belt.Option.map(options.tie_breaker, serializeTieBreaker))
        ]
        |> Domain.filterNoneSnd
    }
}