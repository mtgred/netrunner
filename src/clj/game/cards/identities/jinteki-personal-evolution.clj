(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-jinteki-personal-evolution
  {"Jinteki: Personal Evolution"
   {:events {:agenda-scored {:interactive (req true)
                             :delayed-completion true
                             :req (req (not (:winner @state)))
                             :msg "do 1 net damage"
                             :effect (effect (damage eid :net 1 {:card card}))}
             :agenda-stolen {:msg "do 1 net damage"
                             :delayed-completion true
                             :req (req (not (:winner @state)))
                             :effect (effect (damage eid :net 1 {:card card}))}}}})
