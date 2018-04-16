(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-high-risk-investment
  {"High-Risk Investment"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:click 1]
                 :counter-cost [:agenda 1]
                 :msg (msg "gain " (:credit runner) " [Credits]")
                 :effect (effect (gain :credit (:credit runner)))}]}})
