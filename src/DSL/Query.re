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
and func = {
    filter: option(query),
    func: FunctionScore.scoringFunction,
    weight: option(positiveNumber)
}
and functionScoreContent = {
    query: query,
    boost: option(positiveNumber),
    functions: list(func)
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
    let rec applied = (q) => switch(q) {
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
    and fFunctionScore = fnFunctionScore(applied, _)
    and fBoolean = fnBoolean(applied, _)
    and fDisMax = fnDisMax(applied, _)
    and fNested = fnNested(applied, _)
    and fMatchAll = fnMatchAll

    applied
}

let emptyBoolean = {
    must: [],
    filter: [],
    should: [],
    must_not: [],
    minimum_should_match: None
}

let match = (~options=MatchQuery.noOptions, required) => Match((required, options))

let serializeMatchAll = () => Domain.fromPairs([("match_all", Js.Json.object_(Js.Dict.empty()))])

let serializeBoolean = (serializeQuery, content) => [
        ("must", content.must),
        ("filter", content.filter),
        ("should", content.should),
        ("must_not", content.must_not)
    ] 
    |> List.map(((key, qArray)): option((string, Js.Json.t)) => switch(qArray) {
        | [] => None
        | items => Some((key, Array.of_list(items) |> Array.map(serializeQuery) |> Js.Json.array))
    })
    |> Domain.filterNone
    |> (els) => switch (content.minimum_should_match) {
        | Some(msm) => [("minimum_should_match", Primitives.serializeMsm(msm)), ...els]
        | None => els
    }
    |> Js.Dict.fromList
    |> Js.Json.object_
    |> (content) => {
        Js.Dict.fromList([
            ("bool", content)
        ]) |> Js.Json.object_
    }
let serializeDisMax = (serializeQuery, q) => {
    let queries = (
        "queries",
        q.queries |> List.map(serializeQuery) |> Array.of_list |> Js.Json.array
    );

    let content = switch (q.tie_breaker) {
        | Some(Positive(n)) => [queries, ("tie_breaker", Js.Json.number(n))]
        | None => [queries]
    } |> Js.Dict.fromList |> Js.Json.object_

    Js.Dict.fromList([
        ("dis_max", content)
    ]) |> Js.Json.object_
}
let serializeNested = (serializeQuery, q: nestedContent) => {
    let required = [("query", serializeQuery(q.query))]
    let content = switch (q.options) {
        | Some({ignore_unmapped, score_mode}) => [
            ("ignore_unmapped", Js.Json.boolean(ignore_unmapped)),
            ("score_mode", switch (score_mode) {
                | Average => "avg"
                | Max => "max"
                | Min => "min"
                | Zero => "none"
                | Sum => "sum"
            } |> Js.Json.string),
            ...required
        ]
        | None => required
    } |> Js.Dict.fromList |> Js.Json.object_
    Js.Dict.fromList([
        ("nested", content)
    ]) |> Js.Json.object_
}

let serializeFunctionScore = (serializeQuery, {query, boost, functions}) => {
    let serializeFunc = ({filter, func, weight}) => [
        switch(filter) {
        | None => None
        | Some(f) => Some(("filter", serializeQuery(f)))
        },
        switch(weight) {
        | None => None
        | Some(Positive(w)) => Some(("weight", Js.Json.number(w)))
        }
    ] 
    |> filterNone(~base=[FunctionScore.format(func)])
    |> Js.Dict.fromList
    |> Js.Json.object_

    let body = Js.Dict.fromList([
        ("query", serializeQuery(query)),
        ("boost", switch (boost) {
            | None => 1.
            | Some(Positive(n)) => n
        } |> Js.Json.number),
        ("functions", functions |> List.map(serializeFunc) |> Array.of_list|> Js.Json.array)
    ]) |> Js.Json.object_

    Js.Dict.fromList([
        ("function_score", body)
    ]) |> Js.Json.object_
}

let serializeQuery: (query) => Js.Json.t = cataQuery(
    MatchQuery.serialize,
    TermsQuery.serialize,
    MultiMatchQuery.serialize,
    serializeFunctionScore,
    serializeBoolean,
    serializeDisMax,
    serializeNested,
    serializeMatchAll
)