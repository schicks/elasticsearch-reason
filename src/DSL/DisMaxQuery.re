let serialize = (serializeQuery, q: Query.disMaxContent) => {
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