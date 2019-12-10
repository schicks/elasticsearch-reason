
type order =  Ascending | Descending
let serializeOrder = (o) => switch(o) {
| Ascending => Js.Json.string("asc")
| Descending => Js.Json.string("desc")
}

type termOptions = {
    field: string,
    order: option((string, order))
}


type bucketAggregation =
| Term(termOptions)

let serialize = (agg, subAggs: list((string, Js.Json.t))) => switch(agg) {
| Term({field, order}) => Domain.(filterNone([
        Some(("terms", filterNone([
            Some(("field", Js.Json.string(field))),
            switch(order) {
            | None => None
            | Some((sortField, direction)) => Some(("order", fromPairs([(sortField, serializeOrder(direction))])))
            }
        ]) |> fromPairs)),
        switch (subAggs) {
        | [] => None
        | fullSubAggs => Some(("aggs", fromPairs(fullSubAggs)))
        }
    ]) |> fromPairs)
}