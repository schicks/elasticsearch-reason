open Domain

type simpleMsmExpression = 
    // https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-minimum-should-match.html
    | Number(int)
    | Percentage(int)
and msmExpression =
    | Single(simpleMsmExpression)
    | Combination(positiveInt, simpleMsmExpression)
    | MultipleCombination(list((positiveInt, simpleMsmExpression))) // each positiveInt greater than the last

let serializeMsm = (msm) => {
    let pSimple = (simple) => switch (simple) {
        | Number(n) => Belt.Int.toString(n)
        | Percentage(n) => Belt.Int.toString(n)
    };

    switch (msm) {
        | Single(simple) => pSimple(simple)
        | Combination(Positive(n), simple) => Belt.Int.toString(n) ++ "<" ++ pSimple(simple)
        | MultipleCombination(combinations) => List.fold_left(
            (acc, (n: positiveInt, simple)) => acc ++ " " ++ Belt.Int.toString(unwrapInt(n)) ++ "<-" ++ pSimple(simple),
            "",
            combinations
        )
    }
    |> Js.Json.string
}

type rewriteBehavior =
    // https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-multi-term-rewrite.html
    | ConstantScore
    | ConstantScoreBoolean
    | ScoringBoolean
    | TopTermsBlendedFreqs(positiveInt)
    | TopTermsBoost(positiveInt)
    | TopTerms(positiveInt)

type operator = Or | And
type levenshteinDistance = Zero | One | Two | Auto
type zeroTermsBehavior = None | All
