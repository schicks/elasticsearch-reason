open Jest;
open Primitives;

describe("serializeMsm", () => {
    open Expect;

    [
        (Single(Number(7)), "7"),
        (Single(Percentage(7)), {|7%|}),
        (Combination(Positive(4), Percentage(7)), {|4<7%|}),
        (
            MultipleCombination([
                (Positive(2), Percentage(10)),
                (Positive(6), Percentage(-90)),
                (Positive(10), Number(9))
            ]),
            {|2<10% 6<-90% 10<9|}
        )
    ] 
    |> Domain.each(((input, output)) => 
    test("it should serialize correctly", () => {
        expect(serializeMsm(input)) |> toBe(Js.Json.string(output))
    }))
})