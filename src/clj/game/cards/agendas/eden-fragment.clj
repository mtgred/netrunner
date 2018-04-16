(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-eden-fragment
  {"Eden Fragment"
   {:events {:pre-corp-install
               {:req (req (and (is-type? target "ICE")
                               (empty? (let [cards (map first (turn-events state side :corp-install))]
                                         (filter #(is-type? % "ICE") cards)))))
                :effect (effect (ignore-install-cost true))}
             :corp-install
               {:req (req (and (is-type? target "ICE")
                               (empty? (let [cards (map first (turn-events state side :corp-install))]
                                         (filter #(is-type? % "ICE") cards)))))
                :msg (msg "ignore the install cost of the first ICE this turn")}}}})