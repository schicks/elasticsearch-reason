module Most = {
    open Domain
    open Primitives

    type content = { // does this actually take everything best does and is just poorly documented?
        rewrite: option(rewriteBehavior), // TopTermsBlendedFreqs(Positive(max_expansions))
        analyzer: option(string),
        fuzziness: option(levenshteinDistance), // Zero
        max_expansions: option(positiveInt), // 50
        prefix_length: option(positiveInt), // 0
        lenient: option(bool), // false
        operator: option(operator), // Or
        minimum_should_match: option(msmExpression), // Single(Number(1))
        zero_terms_query: option(zeroTermsBehavior) // None
        //TODO boost?
    }

    let noOptions = {
        rewrite: None,
        analyzer: None,
        fuzziness: None,
        max_expansions: None,
        prefix_length: None,
        lenient: None,
        operator: None,
        minimum_should_match: None,
        zero_terms_query: None
    }

    let format = (options) => {
        [
            ("rewrite", Belt.Option.map(options.rewrite, serializeRewrite)),
            ("analyzer", Belt.Option.map(options.analyzer, Js.Json.string)),
            ("fuzziness", Belt.Option.map(options.fuzziness, serializeLevenshtein)),
            ("max_expansions", Belt.Option.map(options.max_expansions, serializeMaxExpansions)),
            ("prefix_length", Belt.Option.map(options.prefix_length, serializePrefixLength)),
            ("lenient", Belt.Option.map(options.lenient, Js.Json.boolean)),
            ("operator", Belt.Option.map(options.operator, serializeOperator)),
            ("minimum_should_match", Belt.Option.map(options.minimum_should_match, serializeMsm)),
            ("zero_terms_query", Belt.Option.map(options.zero_terms_query, serializeZeroTermsBehavior))
        ]
        |> List.fold_left((acc, a) => switch (a) {
            | (key, Some(el)) => [(key, el), ...acc]
            | (_, None) => acc
        }, [("type", Js.Json.string("most"))])
    }
}