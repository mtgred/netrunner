(in-ns 'game.cards.assets)

(def card-definition-psychic-field
  {"Psychic Field"
   (let [ab {:psi {:req (req installed)
                   :not-equal {:msg (msg "do " (count (:hand runner)) " net damage")
                               :async true
                               :effect (effect (damage eid :net (count (:hand runner)) {:card card}))}}}]
     {:expose ab :access ab})})
