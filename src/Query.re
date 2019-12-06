type query =
    | Match(MatchQuery.content)
    // | MultiMatch(MultiMatch.content)
    | Boolean(booleanContent)
and booleanContent = {
    must: list(query),
    filter: list(query),
    should: list(query),
    must_not: list(query),
    minimum_should_match: option(Primitives.msmExpression)
}


let match = (~options=MatchQuery.noOptions, required) => Match(MatchQuery.{required, options})

let rec serializeQuery = (q:query): Js.Json.t => switch (q) {
    | Boolean(content) => serializeBoolean(content)
    | Match(content) => MatchQuery.serialize(content)
    // | MultiMatch(content) => MultiMatch.serialize(content)
} and serializeBoolean = (content) => [
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