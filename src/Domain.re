
let (>>) = (f, g, x) => g(f(x))

let filterNone = (~base=[], l) => List.fold_left((acc, a) => switch(a) {
    | Some(el) => [el, ...acc]
    | None => acc
}, base, l)

let filterNoneSnd = List.fold_left((acc, a) => switch (a) {
    | (key, Some(el)) => [(key, el), ...acc]
    | (_, None) => acc
}, [], _)

let fromPairs = Js.Dict.fromList >> Js.Json.object_

let each = (f,l) => l |> List.fold_left((_acc, a) => f(a), ())

type positiveInt = | Positive(int);
let positiveInt = (n) =>  n > 0 ? Some(Positive(n)) : None;
let unwrapInt = (n) => switch (n) {
    | Positive(n) => n
}

type positiveNumber = | Positive(float);
let positiveNumber = (n: float) =>  n > 0. ? Some(Positive(n)) : None;
let unwrapNumber = (n) => switch (n) {
    | Positive(n) => n
}

type probability = | Probability(float);
let probability = (n: float) =>  n >= 0. && n <= 1. ? Some(Positive(n)) : None;