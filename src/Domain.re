
let (>>) = (f, g, x) => g(f(x))

type positiveInt = | Positive(int);
let positiveInt = (n) =>  n > 0 ? Some(Positive(n)) : None;
let unwrapInt = (n) => switch (n) {
    | Positive(n) => n
}

type positiveNumber = | Positive(float);
let positiveNumber = (n: float) =>  n > 0. ? Some(Positive(n)) : None;

type probability = | Probability(float);
let probability = (n: float) =>  n >= 0. && n <= 1. ? Some(Positive(n)) : None;