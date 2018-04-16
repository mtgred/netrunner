(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-psychic-field
  {"Psychic Field"
   (let [ab {:psi {:req (req installed)
                   :not-equal {:msg (msg "do " (count (:hand runner)) " net damage")
                               :delayed-completion true
                               :effect (effect (damage eid :net (count (:hand runner)) {:card card}))}}}]
     {:expose ab :access ab})})
