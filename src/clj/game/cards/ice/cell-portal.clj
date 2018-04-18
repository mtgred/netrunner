(in-ns 'game.core)

(def card-definitions-ice-cell-portal
  {"Cell Portal"
   {:subroutines [{:msg "make the Runner approach the outermost ICE"
                   :effect (req (let [srv (first (:server run))
                                      n (count (get-in @state [:corp :servers srv :ices]))]
                                  (swap! state assoc-in [:run :position] n)
                                  (derez state side card)))}]}})
