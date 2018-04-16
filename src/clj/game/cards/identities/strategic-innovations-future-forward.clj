(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-strategic-innovations-future-forward
  {"Strategic Innovations: Future Forward"
   {:events {:pre-start-game {:effect draft-points-target}
             :runner-turn-ends
             {:req (req (and (not (:disabled card))
                             (has-most-faction? state :corp "Haas-Bioroid")
                             (pos? (count (:discard corp)))))
              :prompt "Select a card in Archives to shuffle into R&D"
              :choices {:req #(and (card-is? % :side :corp) (= (:zone %) [:discard]))}
              :player :corp :show-discard true :priority true
              :msg (msg "shuffle " (if (:seen target) (:title target) "a card")
                        " into R&D")
              :effect (effect (move :corp target :deck)
                              (shuffle! :corp :deck))}}}

   ;; No special implementation
   "Sunny Lebeau: Security Specialist"
   {}})
