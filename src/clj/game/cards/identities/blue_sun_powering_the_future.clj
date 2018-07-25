(in-ns 'game.cards.identities)

(def card-definition-blue-sun-powering-the-future
  {"Blue Sun: Powering the Future"
   {:flags {:corp-phase-12 (req (and (not (:disabled card))
                                     (some #(rezzed? %) (all-installed state :corp))))}
    :abilities [{:choices {:req #(:rezzed %)}
                 :effect (req (trigger-event state side :pre-rez-cost target)
                              (let [cost (rez-cost state side target)]
                                (gain-credits state side cost)
                                (move state side target :hand)
                                (system-msg state side (str "adds " (:title target) " to HQ and gains " cost " [Credits]"))
                                (swap! state update-in [:bonus] dissoc :cost)))}]}})
