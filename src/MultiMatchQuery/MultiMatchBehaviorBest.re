module Best = {
    open Domain
    open Primitives

    type content = {
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
}