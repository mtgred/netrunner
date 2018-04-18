(in-ns 'game.core)

(def card-definitions-events-itinerant-protesters
  {"Itinerant Protesters"
   {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"
    :effect (req (lose state :corp :hand-size-modification (:bad-publicity corp))
                 (add-watch state :itin
                   (fn [k ref old new]
                     (let [bpnew (get-in new [:corp :bad-publicity])
                           bpold (get-in old [:corp :bad-publicity])]
                       (when (> bpnew bpold)
                         (lose state :corp :hand-size-modification (- bpnew bpold)))
                       (when (< bpnew bpold)
                         (gain state :corp :hand-size-modification (- bpold bpnew)))))))
    :leave-play (req (remove-watch state :itin)
                     (gain state :corp :hand-size-modification (:bad-publicity corp)))}})
