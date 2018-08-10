(in-ns 'game.cards.events)

(def card-definition-itinerant-protesters
  {"Itinerant Protesters"
   {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"
    :effect (req (lose state :corp :hand-size {:mod  (:bad-publicity corp)})
                 (add-watch state :itin
                   (fn [k ref old new]
                     (let [bpnew (get-in new [:corp :bad-publicity])
                           bpold (get-in old [:corp :bad-publicity])]
                       (when (> bpnew bpold)
                         (lose state :corp :hand-size {:mod (- bpnew bpold)}))
                       (when (< bpnew bpold)
                         (gain state :corp :hand-size {:mod (- bpold bpnew)}))))))
    :leave-play (req (remove-watch state :itin)
                     (gain state :corp :hand-size {:mod (:bad-publicity corp)}))}})
