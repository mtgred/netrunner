(in-ns 'game.core)

(def card-definitions-assets-palana-agroplex
  {"Pālanā Agroplex"
   (let [ability {:msg "make each player draw 1 card"
                  :label "Make each player draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1) (draw :runner))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability}
      :abilities [ability]})})
