(in-ns 'game.cards.programs)

(def card-definition-harbinger
  {"Harbinger"
   {:trash-effect
     {:req (req (not (some #{:facedown :hand} (:previous-zone card))))
      :effect (req (let [lock (get-in @state [:runner :locked :discard])]
                     (swap! state assoc-in [:runner :locked] nil)
                     (runner-install state :runner card {:facedown true})
                     (swap! state assoc-in [:runner :locked] lock)))}}})
