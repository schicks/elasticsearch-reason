open Domain

// https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-match-query.html

type requiredParams = {
    field: string,
    query: string
}

type optionalParams = {
    transpositions: option(bool), // true
    fuzzy_transpositions: option(bool), // true
    fuzzy_rewrite: option(Primitives.rewriteBehavior), // TopTermsBlendedFreqs(Positive(max_expansions))
    analyzer: option(string),
    auto_generate_synonyms_phrase_query: option(bool), // true
    fuzziness: option(Primitives.levenshteinDistance), // Zero
    max_expansions: option(positiveInt), // 50
    prefix_length: option(positiveInt), // 0
    lenient: option(bool), // false
    operator: option(Primitives.operator), // Or
    minimum_should_match: option(Primitives.msmExpression), // Single(Number(1))
    zero_terms_query: option(Primitives.zeroTermsBehavior) // None
}

let noOptions = {
    transpositions: None,
    fuzzy_transpositions: None,
    fuzzy_rewrite: None,
    analyzer: None,
    auto_generate_synonyms_phrase_query: None,
    fuzziness: None,
    max_expansions: None,
    prefix_length: None,
    lenient: None,
    operator: None,
    minimum_should_match: None,
    zero_terms_query: None
}

type content = (requiredParams, optionalParams)

let serialize = ((required, options): content) => [
    ("analyzer", Belt.Option.map(options.analyzer, Js.Json.string)),
    ("auto_generate_synonyms_phrase_query", Belt.Option.map(options.auto_generate_synonyms_phrase_query, Js.Json.boolean)),
    ("fuzzy_transpositions", Belt.Option.map(options.fuzzy_transpositions, Js.Json.boolean)),
    ("fuzziness", Belt.Option.map(options.fuzziness, Primitives.serializeLevenshtein)),
    ("max_expansions", Belt.Option.map(options.max_expansions, Primitives.serializePositiveInt)),
    ("prefix_length", Belt.Option.map(options.prefix_length, Primitives.serializePositiveInt)),
    ("transpositions", Belt.Option.map(options.transpositions, Js.Json.boolean)),
    ("fuzzy_rewrite", Belt.Option.map(options.fuzzy_rewrite, Primitives.serializeRewrite)),
    ("lenient", Belt.Option.map(options.lenient, Js.Json.boolean)),
    ("operator", Belt.Option.map(options.operator, Primitives.serializeOperator)),
    ("minimum_should_match", Belt.Option.map(options.minimum_should_match, Primitives.serializeMsm)),
    ("zero_terms_query", Belt.Option.map(options.zero_terms_query, Primitives.serializeZeroTermsBehavior)),
]
|> Domain.filterNoneSnd
|> (options) => {
    Js.Dict.fromList([
        (
            "match", 
            Js.Dict.fromList([(
                required.field, 
                Js.Dict.fromList([
                    ("query", Js.Json.string(required.query)),
                    ...options
                ]) |> Js.Json.object_
            )]) |> Js.Json.object_
        )
    ])
    |> Js.Json.object_
}