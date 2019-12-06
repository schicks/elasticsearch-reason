open Domain
open Primitives


// https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-multi-match-query.html
type field = {
    name: string,
    weight: option(positiveNumber) // 1
}

let serializeField = (f) => switch(f.weight) {
    | None => f.name
    | Some(Positive(n)) => f.name ++ "^" ++ Belt.Float.toString(n)
}

type bestContent = {
    fuzzy_transpositions: option(bool), // true
    auto_generate_synonyms_phrase_query: option(bool), // true
    rewrite: option(string), // TopTermsBlendedFreqs(Positive(max_expansions))
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

type mostContent = { // does this actually take everything best does and is just poorly documented?
    rewrite: option(string), // TopTermsBlendedFreqs(Positive(max_expansions))
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

type crossContent = {
    analyzer: option(string),
    operator: option(operator),
    minimum_should_match: option(msmExpression),
    lenient: option(bool),
    zero_terms_query: option(zeroTermsBehavior),
    tie_breaker: option(positiveNumber)
}

type phraseContent = {
    analyzer: option(string),
    lenient: option(bool),
    zero_terms_query: option(zeroTermsBehavior),
    slop: option(positiveInt) // 0
}

type phrasePrefixContent = {
    max_expansions: option(positiveInt),
    analyzer: option(string),
    lenient: option(bool),
    zero_terms_query: option(zeroTermsBehavior),
    slop: option(positiveInt) // 0
}

type boolPrefixContent = {
    rewrite: option(string), // TopTermsBlendedFreqs(Positive(max_expansions))
    analyzer: option(string),
    fuzziness: option(levenshteinDistance), // Zero
    max_expansions: option(positiveInt), // 50
    prefix_length: option(positiveInt), // 0
    lenient: option(bool), // false
    operator: option(operator), // Or
    minimum_should_match: option(msmExpression), // Single(Number(1))
    zero_terms_query: option(zeroTermsBehavior) // None
}

type behavior = 
    | Best(bestContent)
    | Most(mostContent)
    | Cross(crossContent)
    | Phrase(phraseContent)
    | PhrasePrefix(phrasePrefixContent)
    | BoolPrefix(boolPrefixContent)

type content = {
    query: string,
    fields: list(field),
    behavior: option(behavior)
}

let formatBehavior = (b) => switch (b) {
    | Best(content) => [("type", Js.Json.string("best_fields"))]
    | Most(content) => [("type", Js.Json.string("most_fields"))]
    | Cross(content) => [("type", Js.Json.string("cross_fields"))]
    | Phrase(content) => [("type", Js.Json.string("phrase"))]
    | PhrasePrefix(content) => [("type", Js.Json.string("phrase_prefix"))]
    | BoolPrefix(content) => [("type", Js.Json.string("bool_prefix"))]
}

let serialize = (q) => {
    let behaviorFields = switch(q.behavior) {
        | None => []
        | Some(behavior) => formatBehavior(behavior)
    }
    let content = Js.Dict.fromList([
        ("query", Js.Json.string(q.query)),
        ("fields", q.fields
        |> List.map(serializeField)
        |> Array.of_list
        |> Js.Json.stringArray
        ),
        ...behaviorFields
    ]) |> Js.Json.object_

    Js.Dict.fromList([
        ("multi_match", content)
    ]) |> Js.Json.object_
}