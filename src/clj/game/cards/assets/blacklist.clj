(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-blacklist
  {"Blacklist"
   {:effect (effect (lock-zone (:cid card) :runner :discard))
    :leave-play (effect (release-zone (:cid card) :runner :discard))}})