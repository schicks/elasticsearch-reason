open Domain
open MultiMatchBehavior


// https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-multi-match-query.html
type field = {
    name: string,
    weight: option(positiveNumber) // 1
}

let serializeField = (f) => switch(f.weight) {
    | None => f.name
    | Some(Positive(n)) => f.name ++ "^" ++ Belt.Float.toString(n)
}

type content = {
    query: string,
    fields: list(field),
    behavior: option(behavior)
}

let serialize = (q) => {
    let behaviorFields = switch(q.behavior) {
        | None => []
        | Some(behavior) => formatBehavior(behavior)
    }
    let content = Js.Dict.fromList([
        ("query", Js.Json.string(q.query)),
        ("fields", q.fields
        |> List.map(serializeField)
        |> Array.of_list
        |> Js.Json.stringArray
        ),
        ...behaviorFields
    ]) |> Js.Json.object_

    Js.Dict.fromList([
        ("multi_match", content)
    ]) |> Js.Json.object_
}