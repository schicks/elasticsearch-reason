open Domain
open Primitives

// https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-match-query.html
type content = {
    field: string,
    query: string,
    transpositions: option(bool), // true
    fuzzy_transpositions: option(bool), // true
    fuzzy_rewrite: option(rewriteBehavior), // TopTermsBlendedFreqs(Positive(max_expansions))
    analyzer: option(string),
    auto_generate_synonyms_phrase_query: option(bool), // true
    fuzziness: option(levenshteinDistance), // Zero
    max_expansions: option(positiveInt), // 50
    prefix_length: option(positiveInt), // 0
    lenient: option(bool), // false
    operator: option(operator), // Or
    minimum_should_match: option(msmExpression), // Single(Number(1))
    zero_terms_query: option(zeroTermsBehavior) // None
}

type requiredParams = {
    field: string,
    query: string
}
let build = ( // this is a mouthful, but only needed once.
    ~analyzer=?,
    ~auto_generate_synonyms_phrase_query=?,
    ~fuzziness=?,
    ~max_expansions=?,
    ~prefix_length=?,
    ~transpositions=?,
    ~fuzzy_transpositions=?,
    ~fuzzy_rewrite=?,
    ~lenient=?,
    ~operator=?,
    ~minimum_should_match=?,
    ~zero_terms_query=?,
    required
) => ({
    field: required.field,
    query: required.query,
    analyzer,
    auto_generate_synonyms_phrase_query,
    fuzzy_transpositions,
    fuzziness,
    max_expansions,
    prefix_length,
    transpositions,
    fuzzy_rewrite,
    lenient,
    operator,
    minimum_should_match,
    zero_terms_query
})

let serialize = (match: content) => [
    (match.field, Some(Js.Json.string(match.query))),
    ("analyzer", Belt.Option.map(match.analyzer, Js.Json.string)),
    ("auto_generate_synonyms_phrase_query", Belt.Option.map(match.auto_generate_synonyms_phrase_query, Js.Json.boolean)),
    ("fuzzy_transpositions", Belt.Option.map(match.fuzzy_transpositions, Js.Json.boolean)),
    ("fuzziness", Belt.Option.map(match.fuzziness, (fuzz) => switch (fuzz) {
        | Zero => Js.Json.number(0.)
        | One => Js.Json.number(1.)
        | Two => Js.Json.number(2.)
        | Auto => Js.Json.string("auto")
    })),
    ("max_expansions", Belt.Option.map(match.max_expansions, (unwrapInt >> Belt.Int.toFloat >> Js.Json.number))),
    ("prefix_length", Belt.Option.map(match.prefix_length, (unwrapInt >> Belt.Int.toFloat >> Js.Json.number))),
    ("transpositions", Belt.Option.map(match.transpositions, Js.Json.boolean)),
    ("fuzzy_rewrite", Belt.Option.map(match.fuzzy_rewrite, (rewrite) => switch (rewrite) {
        | ConstantScore => Js.Json.string("constant_score")
        | ConstantScoreBoolean => Js.Json.string("constant_score_boolean")
        | ScoringBoolean => Js.Json.string("scoring_boolean")
        | TopTermsBlendedFreqs(Positive(n)) =>  Js.Json.string("top_terms_blended_freqs_" ++ Belt.Int.toString(n))
        | TopTermsBoost(Positive(n)) => Js.Json.string("top_terms_boost_" ++ Belt.Int.toString(n))
        | TopTerms(Positive(n)) => Js.Json.string("top_terms_" ++ Belt.Int.toString(n))
    })),
    ("lenient", Belt.Option.map(match.lenient, Js.Json.boolean)),
    ("operator", Belt.Option.map(match.operator, (operator) => switch (operator) {
        | Or => Js.Json.string("OR")
        | And => Js.Json.string("AND")
    })),
    ("minimum_should_match", Belt.Option.map(match.minimum_should_match, serializeMsm)),
    ("zero_terms_query", Belt.Option.map(match.zero_terms_query, (q) => switch (q) {
        | None => Js.Json.string("none")
        | All => Js.Json.string("all")
    })),
]
|> List.fold_left((acc, a) => switch (a) {
    | (key, Some(el)) => [(key, el), ...acc]
    | (_, None) => acc
}, [])
|> Js.Dict.fromList
|> Js.Json.object_