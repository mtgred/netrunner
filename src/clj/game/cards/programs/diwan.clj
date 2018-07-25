(in-ns 'game.cards.programs)

(def card-definition-diwan
  {"Diwan"
   {:prompt "Choose the server that this copy of Diwan is targeting:"
    :choices (req servers)
    :effect (effect (update! (assoc card :server-target target)))
    :events {:purge {:effect (effect (trash card {:cause :purge}))}
             :pre-corp-install {:req (req (let [c target
                                                serv (:server (second targets))]
                                            (and (= serv (:server-target card))
                                                 (not (and (is-central? serv)
                                                           (is-type? c "Upgrade"))))))
                                :effect (effect (install-cost-bonus [:credit 1]))}}}})
