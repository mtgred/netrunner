(in-ns 'game.cards.programs)

(def card-definition-algernon
  {"Algernon"
   {:events
    {:runner-turn-begins
     {:req (req (can-pay? state :runner nil [:credit 2]))
      :optional
      {:prompt (msg "Pay 2 [Credits] to gain [Click]")
       :player :runner
       :yes-ability {:msg "gain [Click]"
                     :effect (req (gain state :runner :click 1)
                                  (update! state :runner (assoc-in (get-card state card) [:special :used-algernon] true)))
                     :cost [:credit 2]}}}
     :runner-turn-ends
     {:async true
      :effect (req (if (get-in card [:special :used-algernon])
                     (do
                       (update! state :runner (dissoc-in card [:special :used-algernon]))
                       (if-not (:successful-run runner-reg)
                         (do
                           (system-msg state :runner "trashes Algernon because a successful run did not occur")
                           (trash state :runner eid card nil))
                         (effect-completed state side eid)))
                     (effect-completed state side eid)))}}}})
