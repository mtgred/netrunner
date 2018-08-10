(in-ns 'game.cards.ice)

(def card-definition-next-bronze
  {"NEXT Bronze"
   {:subroutines [end-the-run]
    :strength-bonus (req (next-ice-count corp))
    :events (let [nb {:req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "NEXT")))
                      :effect (effect (update-ice-strength card))}]
              {:rez nb
               :derez nb
               :trash nb
               :card-moved nb})}})
