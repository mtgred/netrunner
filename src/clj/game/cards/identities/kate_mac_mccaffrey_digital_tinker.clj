(in-ns 'game.cards.identities)

(def card-definition-kate-mac-mccaffrey-digital-tinker
  {"Kate \"Mac\" McCaffrey: Digital Tinker"
   ;; Effect marks Kate's ability as "used" if it has already met it's trigger condition this turn
   (letfn [(kate-type? [card] (or (is-type? card "Hardware")
                                  (is-type? card "Program")))
           (not-triggered? [state card] (not (get-in @state [:per-turn (:cid card)])))
           (mark-triggered [state card] (swap! state assoc-in [:per-turn (:cid card)] true))]
     {:effect (req (when (pos? (event-count state :runner :runner-install #(kate-type? (first %))))
                     (mark-triggered state card)))
      :events {:pre-install {:req (req (and (kate-type? target)
                                            (not-triggered? state card)))
                             :effect (effect (install-cost-bonus [:credit -1]))}
               :runner-install {:req (req (and (kate-type? target)
                                               (not-triggered? state card)))
                                :silent (req true)
                                :msg (msg "reduce the install cost of " (:title target) " by 1 [Credits]")
                                :effect (req (mark-triggered state card))}}})})
