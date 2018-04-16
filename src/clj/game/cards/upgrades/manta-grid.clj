(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-manta-grid
  {"Manta Grid"
   {:events {:successful-run-ends
             {:msg "gain a [Click] next turn"
              :req (req (and (= (first (:server target)) (second (:zone card)))
                             (or (< (:credit runner) 6) (zero? (:click runner)))))
              :effect (req (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}}}})
