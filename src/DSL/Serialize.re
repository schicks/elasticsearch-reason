let serializeMatchAll = () => Domain.fromPairs([("match_all", Js.Json.object_(Js.Dict.empty()))])

let serializeQuery: (Query.query) => Js.Json.t = Query.cataQuery(
    MatchQuery.serialize,
    TermsQuery.serialize,
    MultiMatchQuery.serialize,
    FunctionScoreSerialize.serialize,
    BooleanQuery.serialize,
    DisMaxQuery.serialize,
    NestedQuery.serialize,
    serializeMatchAll
)