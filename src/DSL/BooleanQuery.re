

let serialize = (serializeQuery, content: Query.booleanContent) => [
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