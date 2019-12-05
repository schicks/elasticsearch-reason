open Domain
open Primitives


// https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-multi-match-query.html
type field = {
    name: string,
    weight: option(positiveInt) // 1
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