(in-ns 'game.cards.events)

(def card-definition-emergency-shutdown
  {"Emergency Shutdown"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :msg (msg "derez " (:title target))
    :choices {:req #(and (ice? %)
                         (rezzed? %))}
    :effect (effect (derez target))}})
