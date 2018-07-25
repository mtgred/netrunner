(in-ns 'game.cards.assets)

(def card-definition-kuwinda-k4h1u3
  {"Kuwinda K4H1U3"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :abilities [{:label "Trace X - do 1 brain damage (start of turn)"
                 :trace {:base (req (get-counters card :power))
                          :successful {:async true
                                       :msg "do 1 brain damage"
                                       :effect (effect (damage :runner eid :brain 1 {:card card})
                                                       (trash card))}
                          :unsuccessful {:effect (effect (add-counter card :power 1)
                                                         (system-msg "adds 1 power counter to Kuwinda K4H1U3"))}}}]}})
