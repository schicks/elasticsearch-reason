open Domain

type nestedScoreMode = 
    | Average // Default
    | Max
    | Min
    | Zero // Renamed from None for type sanity
    | Sum
type nestedOptions = {
    score_mode: option(nestedScoreMode),
    ignore_unmapped: option(bool)
}

type query =
    | Match(MatchQuery.content)
    | MultiMatch(MultiMatchQuery.content)
    | Boolean(booleanContent)
    | DisMax(disMaxContent)
    | Nested(nestedContent)
    | MatchAll
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

let emptyBoolean = {
    must: [],
    filter: [],
    should: [],
    must_not: [],
    minimum_should_match: None
}
let boolean = ( // Worth creating, but not convinced that this would ever be a better api than straight record creation
    ~must=[],
    ~filter=[],
    ~should=[],
    ~must_not=[],
    ~minimum_should_match=None,
    ()
) => Boolean({
    must,
    filter,
    should,
    must_not,
    minimum_should_match
})

let empty_object: Js.Dict.t(Js.Json.t) = Js.Dict.empty()

let match = (~options=MatchQuery.noOptions, required) => Match((required, options))

let rec serializeQuery = (q:query): Js.Json.t => switch (q) {
    | Boolean(content) => serializeBoolean(content)
    | DisMax(content) => serializeDisMax(content)
    | Match(content) => MatchQuery.serialize(content)
    | MultiMatch(content) => MultiMatchQuery.serialize(content)
    | MatchAll => Js.Dict.fromList([("match_all", Js.Json.object_(empty_object))]) |> Js.Json.object_
    | Nested(content) => serializeNested(content)
} 
and serializeBoolean = (content) => [
        ("must", content.must),
        ("filter", content.filter),
        ("should", content.should),
        ("must_not", content.must_not)
    ] 
    |> List.map(((key, qArray)): option((string, Js.Json.t)) => switch(qArray) {
        | [] => None
        | items => Some((key, Array.of_list(items) |> Array.map(serializeQuery) |> Js.Json.array))
    })
    |> List.fold_left((acc, a) => switch (a) {
        | Some(el) => [el, ...acc]
        | None => acc
    }, [])
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
and serializeDisMax = (q) => {
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
and serializeNested = (q) => {
    let required = [("query", serializeQuery(q.query))]
    let content = switch (q.options) {
        | Some(queryOptions) => [
            ("ignore_unmapped", Belt.Option.map(queryOptions.ignore_unmapped, Js.Json.boolean)),
            ("score_mode", Belt.Option.map(queryOptions.score_mode, (a) => switch (a) {
                | Average => "avg"
                | Max => "max"
                | Min => "min"
                | Zero => "none"
                | Sum => "sum"
            } |> Js.Json.string))
        ] |> List.fold_left((acc, (key, v)) => switch (v) {
            | Some(value) => [(key, value), ...acc]
            | None => acc
        }, required)
        | None => required
    } |> Js.Dict.fromList |> Js.Json.object_
    Js.Dict.fromList([
        ("nested", content)
    ]) |> Js.Json.object_
}