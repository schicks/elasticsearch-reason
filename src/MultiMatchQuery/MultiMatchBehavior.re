include MultiMatchBehaviorBest
include MultiMatchBehaviorMost
include MultiMatchBehaviorCross
include MultiMatchBehaviorPhrase
include MultiMatchBehaviorPhrasePrefix
include MultiMatchBehaviorBoolPrefix

type behavior = 
    | Best(Best.content)
    | Most(Most.content)
    | Cross(Cross.content)
    | Phrase(Phrase.content)
    | PhrasePrefix(PhrasePrefix.content)
    | BoolPrefix(BoolPrefix.content)

let formatBehavior = (b) => switch (b) {
    | Best(content) => [("type", Js.Json.string("best_fields"))]
    | Most(content) => [("type", Js.Json.string("most_fields")), ...Most.format(content)]
    | Cross(content) => [("type", Js.Json.string("cross_fields"))]
    | Phrase(content) => [("type", Js.Json.string("phrase"))]
    | PhrasePrefix(content) => [("type", Js.Json.string("phrase_prefix"))]
    | BoolPrefix(content) => [("type", Js.Json.string("bool_prefix"))]
}