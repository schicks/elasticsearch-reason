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
        ("rescore_query", Query.serializeQuery(query))
    ])))
]) |> Js.Json.object_

type bodyContent = {
    query: Query.query
}

type body = 
    | Just(bodyContent)
    | Rescoring(bodyContent, list(rescore))
    | Sorting(bodyContent, list(sort))

let serializeBody = (b) => {
    let queryEntry = (
        "query", 
        switch(b) {
        | Just({query}) | Rescoring({query}, _) | Sorting({query}, _) => Query.serializeQuery(query)
        }
    )

    switch (b) {
        | Just(_) => [queryEntry]
        | Rescoring(_, rescores) => [
            queryEntry, 
            ("rescore", rescores |> List.map(serializeRescore) |> Array.of_list |> Js.Json.array)
        ]
        | Sorting(_, sorts) => [
            queryEntry,
            ("sort", sorts |> List.map(serializeSort) |> Array.of_list |> Js.Json.array)
        ]
    } |> Js.Dict.fromList |> Js.Json.object_

}