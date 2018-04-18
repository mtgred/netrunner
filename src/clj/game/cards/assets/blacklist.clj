(in-ns 'game.core)

(def card-definitions-assets-blacklist
  {"Blacklist"
   {:effect (effect (lock-zone (:cid card) :runner :discard))
    :leave-play (effect (release-zone (:cid card) :runner :discard))}})
