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

type content = {
    required: requiredParams,
    options: optionalParams
}

let serialize = (match: content) => [
    ("analyzer", Belt.Option.map(match.options.analyzer, Js.Json.string)),
    ("auto_generate_synonyms_phrase_query", Belt.Option.map(match.options.auto_generate_synonyms_phrase_query, Js.Json.boolean)),
    ("fuzzy_transpositions", Belt.Option.map(match.options.fuzzy_transpositions, Js.Json.boolean)),
    ("fuzziness", Belt.Option.map(match.options.fuzziness, (fuzz) => switch (fuzz) {
        | Zero => Js.Json.number(0.)
        | One => Js.Json.number(1.)
        | Two => Js.Json.number(2.)
        | Auto => Js.Json.string("auto")
    })),
    ("max_expansions", Belt.Option.map(match.options.max_expansions, (unwrapInt >> Belt.Int.toFloat >> Js.Json.number))),
    ("prefix_length", Belt.Option.map(match.options.prefix_length, (unwrapInt >> Belt.Int.toFloat >> Js.Json.number))),
    ("transpositions", Belt.Option.map(match.options.transpositions, Js.Json.boolean)),
    ("fuzzy_rewrite", Belt.Option.map(match.options.fuzzy_rewrite, (rewrite) => switch (rewrite) {
        | ConstantScore => Js.Json.string("constant_score")
        | ConstantScoreBoolean => Js.Json.string("constant_score_boolean")
        | ScoringBoolean => Js.Json.string("scoring_boolean")
        | TopTermsBlendedFreqs(Positive(n)) =>  Js.Json.string("top_terms_blended_freqs_" ++ Belt.Int.toString(n))
        | TopTermsBoost(Positive(n)) => Js.Json.string("top_terms_boost_" ++ Belt.Int.toString(n))
        | TopTerms(Positive(n)) => Js.Json.string("top_terms_" ++ Belt.Int.toString(n))
    })),
    ("lenient", Belt.Option.map(match.options.lenient, Js.Json.boolean)),
    ("operator", Belt.Option.map(match.options.operator, (operator) => switch (operator) {
        | Or => Js.Json.string("OR")
        | And => Js.Json.string("AND")
    })),
    ("minimum_should_match", Belt.Option.map(match.options.minimum_should_match, Primitives.serializeMsm)),
    ("zero_terms_query", Belt.Option.map(match.options.zero_terms_query, (q) => switch (q) {
        | None => Js.Json.string("none")
        | All => Js.Json.string("all")
    })),
]
|> List.fold_left((acc, a) => switch (a) {
    | (key, Some(el)) => [(key, el), ...acc]
    | (_, None) => acc
}, [])
|> (options) => {
    Js.Dict.fromList([
        (
            "match", 
            Js.Dict.fromList([(
                match.required.field, 
                Js.Dict.fromList([
                    ("query", Js.Json.string(match.required.query)),
                    ...options
                ]) |> Js.Json.object_
            )]) |> Js.Json.object_
        )
    ])
    |> Js.Json.object_
}