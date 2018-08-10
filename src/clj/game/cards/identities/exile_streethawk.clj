(in-ns 'game.cards.identities)

(def card-definition-exile-streethawk
  {"Exile: Streethawk"
   {:flags {:runner-install-draw true}
    :events {:runner-install {:silent (req (not (and (is-type? target "Program")
                                                     (some #{:discard} (:previous-zone target)))))
                              :async true
                              :req (req (and (is-type? target "Program")
                                             (some #{:discard} (:previous-zone target))))
                              :msg (msg "draw a card")
                              :effect (req (draw state side eid 1 nil))}}}})
