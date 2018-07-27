(in-ns 'game.cards.resources)

(def card-definition-slipstream
  {"Slipstream"
    {:implementation "Use Slipstream before hitting Continue to pass current ice"
     :abilities [{:req (req (:run @state))
                  :label "Approach another piece of ice"
                  :effect (req (let [ice-pos  (get-in @state [:run :position])]
                                 (resolve-ability state side
                                   {:prompt (msg "Choose a piece of ice protecting a central server at the same position as " (:title current-ice))
                                    :choices {:req #(and (is-central? (second (:zone %)))
                                                         (ice? %)
                                                         (= ice-pos (inc (ice-index state %))))}
                                    :msg (msg "approach " (card-str state target))
                                    :effect (req (let [dest (second (:zone target))]
                                                   (swap! state update-in [:run]
                                                          #(assoc % :position ice-pos :server [dest]))
                                                   (trash state side card)))}
                                card nil)))}]}})
