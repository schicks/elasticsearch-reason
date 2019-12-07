[%raw "require('isomorphic-fetch')"]

open Domain;
open Jest;
open Query;

let basicQuery: MatchQuery.requiredParams = {
            query: "python",
            field: "csTitle"
        }

describe("Query construction", () => {
    open Expect;

    [
        Terms({
            field: "_id",
            terms: ["1", "2", "3"],
            boost: Some(Positive(7.))
        }),
        Match((basicQuery, MatchQuery.noOptions)),
        Match((
            basicQuery,
            {
                ...MatchQuery.noOptions, 
                operator: Some(And)
            }
        )),
        Boolean({...emptyBoolean, should: [Match((basicQuery, MatchQuery.noOptions))]}),
        DisMax({
            queries: [Match((basicQuery, MatchQuery.noOptions))],
            tie_breaker: None
        }),
        MultiMatch({
            query: "python",
            fields: [("csTitle", positiveNumber(7.))],
            behavior: MultiMatchBehavior.most({
                ...MultiMatchBehavior.Most.noOptions,
                fuzziness: Some(Primitives.Auto)
            })
        }),
        Boolean({ // the big one
            ...emptyBoolean,
            filter: [MatchAll],
            should: [
                DisMax({
                    queries: [
                        Match((basicQuery, MatchQuery.noOptions)),
                        MultiMatch({
                            query: "python",
                            fields: [("csTitle", positiveNumber(7.))],
                            behavior: None
                        })
                    ],
                    tie_breaker: None
                })
            ],
            minimum_should_match: Primitives.(
                Some(MultipleCombination([
                    (Positive(2), Number(2)),
                    (Positive(7), Percentage(34))
                ]))
            )
        })
    ] |> each(
        (q) => testPromise("It should generate well formed queries", () => {
            Js.Promise.(
                Fetch.fetchWithInit(
                    "http://localhost:9200/_search",
                    Fetch.RequestInit.make(
                        ~method_=Post,
                        ~body=Body.serializeBody(Just({query: q}))
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

describe("complex bodies", () => {
    open Expect;
    [
        Body.Rescoring(
            {query: Match((basicQuery, MatchQuery.noOptions))},
            [{window_size: Positive(10), score_mode: Multiply, query: Match((basicQuery, MatchQuery.noOptions))}]
        ),
        Body.Sorting(
            {query: Match((basicQuery, MatchQuery.noOptions))},
            [Score]
        ),
        Body.Sorting(
            {query: Match((basicQuery, MatchQuery.noOptions))},
            [Field("csTitle"), Score]
        )
    ] |> each((b) => testPromise("It should generate well formed bodies", () => {
        Js.Promise.(
                Fetch.fetchWithInit(
                    "http://localhost:9200/_search",
                    Fetch.RequestInit.make(
                        ~method_=Post,
                        ~body=Body.serializeBody(b)
                        |> Js.Json.stringify
                        // |> (d) => {Js.Console.log(d); d}
                        |> Fetch.BodyInit.make,
                        ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
                        ()
                    )
                )
                |> then_(Fetch.Response.status >> expect >> toBe(200) >> resolve)
            )
    }))
})