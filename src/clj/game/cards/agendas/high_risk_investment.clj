(in-ns 'game.cards.agendas)

(def card-definition-high-risk-investment
  {"High-Risk Investment"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:click 1]
                 :counter-cost [:agenda 1]
                 :label "Gain [Credits]"
                 :msg (msg "gain " (:credit runner) " [Credits]")
                 :effect (effect (gain-credits (:credit runner)))}]}})
