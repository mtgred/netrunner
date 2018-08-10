(in-ns 'game.cards.events)

(def card-definition-injection-attack
  {"Injection Attack"
   (run-event
    {:async true}
    nil
    nil
    (effect (continue-ability
             {:prompt "Select an icebreaker"
              :choices {:req #(and (installed? %) (has-subtype? % "Icebreaker"))}
              :effect (effect (pump target 2 :all-run))}
             card nil)))})
