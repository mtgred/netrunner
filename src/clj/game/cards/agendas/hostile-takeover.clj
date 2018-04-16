(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-hostile-takeover
  {"Hostile Takeover"
   {:msg "gain 7 [Credits] and take 1 bad publicity"
    :effect (effect (gain :credit 7)
                    (gain-bad-publicity :corp 1))
    :interactive (req true)}})
