type randomConfig = {
    seed: option(string),
    field: string
}
let serializeRandom = ({seed, field}) => {
    let base = ("field", Js.Json.string(field))
    let entries = switch(seed) {
    | None => [base]
    | Some(s) => [base, ("seed", Js.Json.string(s))]
    }
    Domain.fromPairs(entries)
}

type fieldValueModifier =  Sqrt | Square | Ln2p
type fieldValueFactorConfig = {
    factor: option(Domain.positiveNumber),
    missing: option(Domain.positiveNumber),
    modifier: option(fieldValueModifier)
}
let serializeFieldFactor = ({factor, missing, modifier}, field) => {
    [
        switch(factor) {
        | None => None
        | Some(Positive(n)) => Some(("factor", Js.Json.number(n)))
        },
        switch(missing) {
        | None => None
        | Some(Positive(n)) => Some(("missing", Js.Json.number(n)))
        },
        switch(modifier) {
        | None => None
        | Some(Sqrt) => Some(("modifier", Js.Json.string("sqrt")))
        | Some(Square) => Some(("modifier", Js.Json.string("square")))
        | Some(Ln2p) => Some(("modifier", Js.Json.string("ln2p")))
        }
    ]
    |> Domain.filterNone(~base=[("field", Js.Json.string(field))])
    |> Domain.fromPairs
}

type decayConfig = {
    origin: option(string),
    scale: string,
    offset: option(string),
    decay: option(Domain.probability)
}
let emptyDecay = {
    origin: None,
    scale: "0",
    offset: None,
    decay: None
}
let serializeDecay = ({origin, scale, offset, decay}, field) => {
    let serializedConf = [
        switch(offset) {
        | None => None
        | Some(o) => Some(("offset", Js.Json.string(o)))
        },
        switch(decay) {
        | None => None
        | Some(Probability(p)) => Some(("decay", Js.Json.number(p)))
        },
        switch(origin) {
        | None => None
        | Some(o) => Some(("origin", Js.Json.string(o)))
        }
    ] |> Domain.filterNone(~base=[
        ("scale", Js.Json.string(scale)),
    ])
    |> Domain.fromPairs
    
    Domain.fromPairs([(field, serializedConf)])
}
type scoringFunction = 
| Weight(Domain.positiveNumber)
| Random(randomConfig)
| Field(string, fieldValueFactorConfig)
| GaussianDecay(string, decayConfig)
| LinearDecay(string, decayConfig)
| ExponentialDecay(string, decayConfig)