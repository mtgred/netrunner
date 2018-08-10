(in-ns 'game.cards.agendas)

(def card-definition-hostile-takeover
  {"Hostile Takeover"
   {:msg "gain 7 [Credits] and take 1 bad publicity"
    :effect (effect (gain-credits 7)
                    (gain-bad-publicity :corp 1))
    :interactive (req true)}})
