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

let best = (content) => Some(Best(content))
let most = (content) => Some(Most(content))
let cross = (content) => Some(Cross(content))
let phrase = (content) => Some(Phrase(content))
let phrasePrefix = (content) => Some(PhrasePrefix(content))
let boolPrefix = (content) => Some(BoolPrefix(content))

let formatBehavior = (b) => switch (b) {
    | Best(content) => [("type", Js.Json.string("best_fields")), ...Best.format(content)]
    | Most(content) => [("type", Js.Json.string("most_fields")), ...Most.format(content)]
    | Cross(content) => [("type", Js.Json.string("cross_fields")), ...Cross.format(content)]
    | Phrase(content) => [("type", Js.Json.string("phrase")), ...Phrase.format(content)]
    | PhrasePrefix(content) => [("type", Js.Json.string("phrase_prefix")), ...PhrasePrefix.format(content)]
    | BoolPrefix(content) => [("type", Js.Json.string("bool_prefix")), ...BoolPrefix.format(content)]
}