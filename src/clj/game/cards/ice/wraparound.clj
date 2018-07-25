(in-ns 'game.cards.ice)

(def card-definition-wraparound
  {"Wraparound"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-active-installed state :runner))
                           0 7))
    :events (let [wr {:silent (req true)
                      :req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "Fracter")))
                      :effect (effect (update-ice-strength card))}]
              {:runner-install wr :trash wr :card-moved wr})}})
