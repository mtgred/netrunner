(in-ns 'game.core)

(def card-definitions-assets-kala-ghoda-real-tv
  {"Kala Ghoda Real TV"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :abilities [{:msg "look at the top card of the Runner's Stack"
                  :effect (effect (prompt! card (str "The top card of the Runner's Stack is "
                                                     (:title (first (:deck runner)))) ["OK"] {}))}
                {:label "[Trash]: Trash the top card of the Runner's Stack"
                 :msg (msg "trash " (:title (first (:deck runner))) " from the Runner's Stack")
                 :effect (effect (mill :runner)
                                 (trash card {:cause :ability-cost}))}]}})
