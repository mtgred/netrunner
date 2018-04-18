(in-ns 'game.core)

(def card-definitions-programs-takobi
  {"Takobi"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (system-msg "adds a power counter to Takobi"))}
                {:req (req (and (:run @state)
                                (rezzed? current-ice)
                                (>= (get-in card [:counter :power] 0) 2)))
                 :counter-cost [:power 2]
                 :label "Increase non-AI icebreaker strength by +3 until end of encounter"
                 :prompt "Choose an installed non-AI icebreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "add +3 strength to " (:title target) " for remainder of encounter")
                 :effect (effect (pump target 3 :encounter))}]}})
