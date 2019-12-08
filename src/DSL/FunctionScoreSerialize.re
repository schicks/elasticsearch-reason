let format = (func) => FunctionScore.(switch(func) {
| Weight(Positive(n)) => ("weight", Js.Json.number(n))
| Random(rc) => ("random_score", serializeRandom(rc))
| Field(field, conf) => ("field_value_factor", serializeFieldFactor(conf, field))
| GaussianDecay(field, conf) => ("gaussian", serializeDecay(conf, field))
| LinearDecay(field, conf) => ("linear", serializeDecay(conf, field))
| ExponentialDecay(field, conf) => ("exp", serializeDecay(conf, field))
})
let serialize = (serializeQuery, {query, boost, functions}: Query.functionScoreContent) => {
    let innerSerialize = ((filter, func, weight)) => [
        switch(filter) {
        | None => None
        | Some(f) => Some(("filter", serializeQuery(f)))
        },
        switch(weight) {
        | None => None
        | Some(Domain.Positive(w)) => Some(("weight", Js.Json.number(w)))
        }
    ] 
    |> Domain.filterNone(~base=[format(func)])
    |> Js.Dict.fromList
    |> Js.Json.object_

    let body = Js.Dict.fromList([
        ("query", serializeQuery(query)),
        ("boost", switch (boost) {
            | None => 1.
            | Some(Positive(n)) => n
        } |> Js.Json.number),
        ("functions", functions |> List.map(innerSerialize) |> Array.of_list|> Js.Json.array)
    ]) |> Js.Json.object_

    Js.Dict.fromList([
        ("function_score", body)
    ]) |> Js.Json.object_
}