(in-ns 'game.cards.hardware)

(def card-definition-record-reconstructor
  {"Record Reconstructor"
   {:events
    {:successful-run
     {:req (req (= (get-in @state [:run :server]) [:archives]))
      :effect (req (let [rr card]
                     (swap! state assoc-in [:run :run-effect :replace-access]
                       {:effect (effect (resolve-ability
                                          {:prompt "Choose one faceup card to add to the top of R&D"
                                           :choices (req (filter #(:seen %) (:discard corp)))
                                           :msg (msg "add " (:title target) " to the top of R&D")
                                           :effect (req (move state :corp target :deck {:front true}))}
                                         rr nil))})))}}}})
