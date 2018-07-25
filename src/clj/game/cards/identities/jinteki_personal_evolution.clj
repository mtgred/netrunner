(in-ns 'game.cards.identities)

(def card-definition-jinteki-personal-evolution
  {"Jinteki: Personal Evolution"
   {:events {:agenda-scored {:interactive (req true)
                             :async true
                             :req (req (not (:winner @state)))
                             :msg "do 1 net damage"
                             :effect (effect (damage eid :net 1 {:card card}))}
             :agenda-stolen {:msg "do 1 net damage"
                             :async true
                             :req (req (not (:winner @state)))
                             :effect (effect (damage eid :net 1 {:card card}))}}}})
