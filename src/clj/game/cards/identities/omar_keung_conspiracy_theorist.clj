(in-ns 'game.cards.identities)

(def card-definition-omar-keung-conspiracy-theorist
  {"Omar Keung: Conspiracy Theorist"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on Archives"
                 :once :per-turn
                 :makes-run true
                 :effect (effect (update! (assoc card :omar-run-activated true))
                                 (run :archives nil (get-card state card)))}]
    :events {:pre-successful-run {:interactive (req true)
                                  :req (req (:omar-run-activated card))
                                  :prompt "Treat as a successful run on which server?"
                                  :choices ["HQ" "R&D"]
                                  :effect (req (let [target-server (if (= target "HQ") :hq :rd)]
                                                 (swap! state update-in [:runner :register :successful-run] #(rest %))
                                                 (swap! state assoc-in [:run :server] [target-server])
                                                 ; remove the :req from the run-effect, so that other cards that replace
                                                 ; access don't use Omar's req.
                                                 (swap! state dissoc-in [:run :run-effect :req])
                                                 (trigger-event state :corp :no-action)
                                                 (swap! state update-in [:runner :register :successful-run] #(conj % target-server))
                                                 (system-msg state side (str "uses Omar Keung: Conspiracy Theorist to make a successful run on " target))))}
             :run-ends {:effect (req (swap! state dissoc-in [:runner :identity :omar-run-activated]))}}}})
