[%raw "require('isomorphic-fetch')"]

open Domain;
open Jest;
open Query;

let each = (f,l) => l |> List.fold_left((_acc, a) => f(a), ())

let basicQuery = MatchQuery.{
            query: "python",
            field: "csTitle"
        }

describe("Query construction", () => {
    open Expect;

    [
        match(basicQuery),
        match(
            ~options={
                ...MatchQuery.noOptions, 
                operator: Some(And)
            },
            basicQuery
        ),
        Boolean({
            should: [match(basicQuery)],
            must: [],
            filter: [],
            must_not: [],
            minimum_should_match: None
        }),
        DisMax({
            queries: [match(basicQuery)],
            tie_breaker: None
        }),
        MultiMatch({
            query: "python",
            fields: [{name: "csTitle", weight: positiveNumber(7.)}],
            behavior: MultiMatchBehavior.most({
                ...MultiMatchBehavior.Most.noOptions,
                fuzziness: Some(Primitives.Auto)
            })
        })
    ] |> each(
        (query) => testPromise("It should generate well formed queries", () => {
            Js.Promise.(
                Fetch.fetchWithInit(
                    "http://localhost:9200/_search",
                    Fetch.RequestInit.make(
                        ~method_=Post,
                        ~body=Js.Dict.fromList([
                            ("query", serializeQuery(query))
                        ])
                        |> Js.Json.object_
                        |> Js.Json.stringify
                        // |> (d) => {Js.Console.log(d); d}
                        |> Fetch.BodyInit.make,
                        ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
                        ()
                    )
                )
                |> then_(Fetch.Response.status >> expect >> toBe(200) >> resolve)
            )
        })
    )
})