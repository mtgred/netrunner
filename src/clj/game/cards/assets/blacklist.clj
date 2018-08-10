(in-ns 'game.cards.assets)

(def card-definition-blacklist
  {"Blacklist"
   {:effect (effect (lock-zone (:cid card) :runner :discard))
    :leave-play (effect (release-zone (:cid card) :runner :discard))}})
