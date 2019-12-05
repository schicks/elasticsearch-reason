open Jest;
open Query;

describe("Match queries", () => {
    open Expect;

    [ // not really tests yet, just testing my understanding of how to run simd tests
        match({
            query: "python",
            field: "csTitle"
        }),
        match({
            query: "python",
            field: "csTitle"
        })
    ] |> List.fold_left(
        (_acc, query) => test("It should generate well formed queries", () => {
            expect(true) |> toBe(true)
        }),
        ()
    )
})