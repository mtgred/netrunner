(in-ns 'game.core)

(def card-definitions-upgrades-crisium-grid
  {"Crisium Grid"
   (let [suppress-event {:req (req (and this-server (not= (:cid target) (:cid card))))}]
     {:suppress {:pre-successful-run suppress-event
                 :successful-run suppress-event}
      :events {:pre-successful-run {:silent (req true)
                                    :req (req this-server)
                                    :effect (req (swap! state update-in [:run :run-effect] dissoc :replace-access)
                                                 (swap! state update-in [:run] dissoc :successful)
                                                 (swap! state update-in [:runner :register :successful-run] #(next %)))}}})})
