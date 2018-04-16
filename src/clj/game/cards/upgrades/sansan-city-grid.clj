(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-sansan-city-grid
  {"SanSan City Grid"
   {:effect (req (when-let [agenda (some #(when (is-type? % "Agenda") %)
                                         (:content (card->server state card)))]
                   (update-advancement-cost state side agenda)))
    :events {:corp-install {:req (req (and (is-type? target "Agenda")
                                           (in-same-server? card target)))
                            :effect (effect (update-advancement-cost target))}
             :pre-advancement-cost {:req (req (in-same-server? card target))
                                    :effect (effect (advancement-cost-bonus -1))}}}})