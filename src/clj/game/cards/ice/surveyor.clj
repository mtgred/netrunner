(in-ns 'game.cards.ice)

(def card-definition-surveyor
  {"Surveyor"
   (let [x (req (* 2 (count (:ices (card->server state card)))))
         recalculate-strength (req (update-ice-strength state side (get-card state card)))
         recalc-event {:req (req (= (:zone target) (:zone card)))
                       :effect recalculate-strength}]
     {:effect recalculate-strength
      :strength-bonus x
      :subroutines [{:label "Trace X - Give the Runner 2 tags"
                     :trace {:base x
                             :label "Give the Runner 2 tags"
                             :successful (give-tags 2)}}
                    {:label "Trace X - End the run"
                     :trace {:base x
                             :label "End the run"
                             :successful end-the-run}}]
      :events {:card-moved recalc-event
               :corp-install recalc-event}})})
