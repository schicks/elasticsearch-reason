open Domain

type nestedScoreMode = 
    | Average // Default
    | Max
    | Min
    | Zero // Renamed from None for type sanity
    | Sum
type nestedOptions = {
    score_mode: nestedScoreMode,
    ignore_unmapped: bool
}

type query =
    | Match(MatchQuery.content)
    | Terms(TermsQuery.content)
    | MultiMatch(MultiMatchQuery.content)
    | FunctionScore(functionScoreContent)
    | Boolean(booleanContent)
    | DisMax(disMaxContent)
    | Nested(nestedContent)
    | MatchAll
and functionScoreContent = {
    query: query,
    boost: option(positiveNumber),
    functions: list((option(query), FunctionScore.scoringFunction, option(positiveNumber)))
}
and nestedContent = {
    query: query,
    path: string,
    options: option(nestedOptions)
}
and disMaxContent = {
    queries: list(query),
    tie_breaker: option(positiveNumber)
}
and booleanContent = {
    must: list(query),
    filter: list(query),
    should: list(query),
    must_not: list(query),
    minimum_should_match: option(Primitives.msmExpression)
}

let cataQuery = (
    fnMatch,
    fnTerms,
    fnMultiMatch,
    fnFunctionScore,
    fnBoolean,
    fnDisMax,
    fnNested,
    fnMatchAll
) => {
    let rec recurse = (q) => switch(q) {
    | Match(content) => fMatch(content)
    | Terms(content) => fTerms(content)
    | MultiMatch(content) => fMultiMatch(content)
    | FunctionScore(content) => fFunctionScore(content)
    | Boolean(content) => fBoolean(content)
    | DisMax(content) => fDisMax(content)
    | Nested(content) => fNested(content)
    | MatchAll => fMatchAll()
    }
    and fMatch = fnMatch(_)
    and fTerms = fnTerms(_)
    and fMultiMatch = fnMultiMatch(_)
    and fFunctionScore = fnFunctionScore(recurse, _)
    and fBoolean = fnBoolean(recurse, _)
    and fDisMax = fnDisMax(recurse, _)
    and fNested = fnNested(recurse, _)
    and fMatchAll = fnMatchAll

    recurse
}

let emptyBoolean = {
    must: [],
    filter: [],
    should: [],
    must_not: [],
    minimum_should_match: None
}

let match = (~options=MatchQuery.noOptions, required) => Match((required, options))