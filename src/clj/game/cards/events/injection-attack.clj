(in-ns 'game.core)

(declare run-event)

(def card-events-injection-attack
  {"Injection Attack"
   (run-event
    {:delayed-completion true}
    nil
    nil
    (effect (continue-ability
             {:prompt "Select an icebreaker"
              :choices {:req #(and (installed? %) (has-subtype? % "Icebreaker"))}
              :effect (effect (pump target 2 :all-run))}
             card nil)))})
