let serialize = (serializeQuery, q: Query.nestedContent) => {
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