(in-ns 'game.core)

(def card-definitions-assets-kuwinda-k4h1u3
  {"Kuwinda K4H1U3"
   (let [ability {:trace {:base (req (get-in card [:counter :power] 0))
                          :delayed-completion true
                          :effect (effect (damage :runner eid :brain 1 {:card card})
                                          (trash card))
                          :msg "do 1 brain damage"
                          :unsuccessful {:effect (effect (add-counter card :power 1)
                                                         (system-msg "adds 1 power counter to Kuwinda K4H1U3"))}}}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins
               {:optional {:prompt "Initiate trace with Kuwinda K4H1U3?"
                           :delayed-completion true
                           :yes-ability ability}}}
      :abilities [(assoc ability :label "Trace X - do 1 brain damage (start of turn)")]})})
