(in-ns 'game.cards.ice)

(def card-definition-seidr-adaptive-barrier
  {"Seidr Adaptive Barrier"
   (let [recalculate-strength (req (update-ice-strength state side (get-card state card)))
         recalc-event {:req (req (= (:zone target) (:zone card)))
                       :effect recalculate-strength}]
     {:effect recalculate-strength
      :strength-bonus (req (count (:ices (card->server state card))))
      :subroutines [end-the-run]
      :events {:card-moved recalc-event
               :corp-install recalc-event}})})
