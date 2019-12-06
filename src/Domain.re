
let (>>) = (f, g, x) => g(f(x))

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