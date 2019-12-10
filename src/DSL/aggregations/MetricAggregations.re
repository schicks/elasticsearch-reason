
type avgOptions = {
    field: string,
    missing: option(float)
}

type metricAggregation =
| Average(avgOptions)

let serialize = (agg) => switch (agg) {
| Average({field, missing}) => Domain.(fromPairs([
    ("avg", filterNone([
        Some(("field", Js.Json.string(field))),
        switch(missing) {
        | None => None
        | Some(m) => Some(("missing", Js.Json.number(m)))
        }
    ]) |> fromPairs)
]))
}