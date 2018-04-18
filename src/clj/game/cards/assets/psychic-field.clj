(in-ns 'game.core)

(def card-definitions-assets-psychic-field
  {"Psychic Field"
   (let [ab {:psi {:req (req installed)
                   :not-equal {:msg (msg "do " (count (:hand runner)) " net damage")
                               :delayed-completion true
                               :effect (effect (damage eid :net (count (:hand runner)) {:card card}))}}}]
     {:expose ab :access ab})})
