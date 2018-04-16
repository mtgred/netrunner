(in-ns 'game.core)

(declare can-host?)

(def card-programs-diwan
  {"Diwan"
   {:prompt "Choose the server that this copy of Diwan is targeting:"
    :choices (req servers)
    :effect (effect (update! (assoc card :server-target target)))
    :events {:purge {:effect (effect (trash card))}
             :pre-corp-install {:req (req (let [c target
                                                serv (:server (second targets))]
                                            (and (= serv (:server-target card))
                                                 (not (and (is-central? serv)
                                                           (is-type? c "Upgrade"))))))
                                :effect (effect (install-cost-bonus [:credit 1]))}}}})
