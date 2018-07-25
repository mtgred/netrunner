(in-ns 'game.cards.resources)

(def card-definition-oracle-may
  {"Oracle May"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :prompt "Choose card type"
                 :choices ["Event" "Hardware" "Program" "Resource"]
                 :effect (req (let [c (first (get-in @state [:runner :deck]))]
                                (system-msg state side (str "spends [Click] to use Oracle May, names " target
                                                            " and reveals " (:title c)))
                                (if (is-type? c target)
                                  (do (system-msg state side (str "gains 2 [Credits] and draws " (:title c)))
                                      (gain-credits state side 2) (draw state side))
                                  (do (system-msg state side (str "trashes " (:title c))) (mill state side)))))}]}})
