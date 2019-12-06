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
        | Percentage(n) => Belt.Int.toString(n) ++ {|%|}
    };
    let pCombination = (n, simple) => Belt.Int.toString(unwrapInt(n)) ++ "<" ++ pSimple(simple);

    switch (msm) {
        | Single(simple) => pSimple(simple)
        | Combination(n, simple) => pCombination(n, simple)
        | MultipleCombination(combinations) => List.fold_left(
            (
                acc, 
                (n: positiveInt, simple)
            ) => {
                let nextExpr = pCombination(n, simple)
                switch (acc) {
                | "" => acc ++ nextExpr
                | _ => acc ++ " " ++ nextExpr
                }
            },
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
let serializeRewrite = (rewrite) => switch (rewrite) {
    | ConstantScore => Js.Json.string("constant_score")
    | ConstantScoreBoolean => Js.Json.string("constant_score_boolean")
    | ScoringBoolean => Js.Json.string("scoring_boolean")
    | TopTermsBlendedFreqs(Positive(n)) =>  Js.Json.string("top_terms_blended_freqs_" ++ Belt.Int.toString(n))
    | TopTermsBoost(Positive(n)) => Js.Json.string("top_terms_boost_" ++ Belt.Int.toString(n))
    | TopTerms(Positive(n)) => Js.Json.string("top_terms_" ++ Belt.Int.toString(n))
}

type operator = Or | And
let serializeOperator = (operator) => switch (operator) {
    | Or => Js.Json.string("OR")
    | And => Js.Json.string("AND")
}

type levenshteinDistance = Zero | One | Two | Auto
let serializeLevenshtein = (fuzz) => switch (fuzz) {
    | Zero => Js.Json.number(0.)
    | One => Js.Json.number(1.)
    | Two => Js.Json.number(2.)
    | Auto => Js.Json.string("auto")
}

type zeroTermsBehavior = None | All
let serializeZeroTermsBehavior = (q) => switch (q) {
    | None => Js.Json.string("none")
    | All => Js.Json.string("all")
}

let serializePositiveInt = unwrapInt >> Belt.Int.toFloat >> Js.Json.number
let serializeTieBreaker = unwrapNumber >> Js.Json.number
