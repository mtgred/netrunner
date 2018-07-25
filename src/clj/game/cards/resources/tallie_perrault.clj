(in-ns 'game.cards.resources)

(def card-definition-tallie-perrault
  {"Tallie Perrault"
   {:abilities [{:label "Draw 1 card for each Corp bad publicity"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (draw (+ (:bad-publicity corp) (:has-bad-pub corp))))
                 :msg (msg "draw " (:bad-publicity corp) " cards")}]
    :events {:play-operation
             {:req (req (or (has-subtype? target "Black Ops")
                            (has-subtype? target "Gray Ops")))
              :effect (req (show-wait-prompt state :corp "Runner to use Tallie Perrault")
                           (resolve-ability
                             state :runner
                             {:optional
                              {:prompt "Use Tallie Perrault to give the Corp 1 bad publicity and take 1 tag?"
                               :player :runner
                               :yes-ability {:msg "give the Corp 1 bad publicity and take 1 tag"
                                             :async true
                                             :effect (effect (gain-bad-publicity :corp 1)
                                                             (gain-tags :runner eid 1)
                                                             (clear-wait-prompt :corp))}
                               :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                            card nil))}}}})
