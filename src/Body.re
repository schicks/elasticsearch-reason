open Domain;

type sort = 
    | Score
    | Field(string)
let serializeSort = (s) => switch (s) {
    | Score => Js.Json.string("_score")
    | Field(f) => Js.Json.string(f)
}

type rescoreMode = 
    | Total
    | Multiply
    | Average
    | Max
    | Min

type rescore = {
    window_size: positiveInt,
    score_mode: rescoreMode,
    query: Query.query
}
let serializeRescore = ({query, window_size, score_mode}) => Js.Dict.fromList([
    ("window_size", Primitives.serializePositiveInt(window_size)),
    ("query", Js.Json.object_(Js.Dict.fromList([
        ("score_mode", switch (score_mode) {
                | Average => "avg"
                | Max => "max"
                | Min => "min"
                | Multiply => "multiply"
                | Total => "total"
            } |> Js.Json.string),
        ("rescore_query", Serialize.serializeQuery(query))
    ])))
]) |> Js.Json.object_

type highlight = {
    pre_tags: list(string),
    post_tags: list(string),
    fields: list(string)
}
let serializeHighlight = ({pre_tags, post_tags, fields}) => fromPairs([
    ("pre_tags", Js.Json.stringArray(pre_tags |> Array.of_list)),
    ("post_tags", Js.Json.stringArray(post_tags |> Array.of_list)),
    ("fields", fromPairs(List.map((f) => (f, Js.Json.object_(Js.Dict.empty())), fields)))
])

type options = {
    size: option(positiveInt),
    from: option(positiveInt),
    highlight: option(highlight),
    aggregations: list(Aggregation.aggregation)
}
let noOptions = {
    size: None,
    from: None,
    highlight: None,
    aggregations: []
}
type bodyContent = {
    query: Query.query,
    options: option(options)
}

type body = 
    | Just(bodyContent)
    | Rescoring(bodyContent, list(rescore))
    | Sorting(bodyContent, list(sort))

let serializeBody = (b) => {
    let entries = switch (b) {
    | Just({options: None}) | Rescoring({options: None}, _) | Sorting({options: None}, _) => []
    | Just({options: Some(o)}) | Rescoring({options: Some(o)}, _) | Sorting({options: Some(o)}, _) => [
        switch(o.size) {
        | None => None
        | Some(n) => Some(("size", Primitives.serializePositiveInt(n)))
        },
        switch(o.from) {
        | None => None
        | Some(n) => Some(("from", Primitives.serializePositiveInt(n)))
        },
        switch(o.aggregations) {
        | [] => None
        | aggs => Some(("aggs", List.map(Aggregation.format, aggs) |> fromPairs))
        }
    ]
    }
    |> filterNone(~base=[
        ("query", switch(b) {
        | Just({query}) | Rescoring({query}, _) | Sorting({query}, _) => Serialize.serializeQuery(query)
        })
    ])

    switch (b) {
        | Just(_) => entries
        | Rescoring(_, rescores) => [
            ("rescore", rescores |> List.map(serializeRescore) |> Array.of_list |> Js.Json.array),
            ...entries 
        ]
        | Sorting(_, sorts) => [
            ("sort", sorts |> List.map(serializeSort) |> Array.of_list |> Js.Json.array),
            ...entries
        ]
    } |> Js.Dict.fromList |> Js.Json.object_

}