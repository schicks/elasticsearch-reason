module Most = {
    open Domain
    open Primitives

    type content = { // does this actually take everything best does and is just poorly documented?
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
}