

type content = {
    field: string,
    terms: list(string),
    boost: option(Domain.positiveNumber)
}

let serialize = ({field, terms, boost}) => Js.Dict.fromList([
    ("terms", Js.Dict.fromList([
        (field, Array.of_list(terms) |> Js.Json.stringArray),
        ("boost", switch(boost) {
        | None => 1.
        | Some(Positive(n)) => n
        } |> Js.Json.number)
    ]) |> Js.Json.object_)
]) |> Js.Json.object_
