(in-ns 'game.core)

(def card-definitions-agendas-crisis-management
  {"Crisis Management"
   (let [ability {:req (req tagged)
                  :delayed-completion true
                  :label "Do 1 meat damage (start of turn)"
                  :once :per-turn
                  :msg "do 1 meat damage"
                  :effect (effect (damage eid :meat 1 {:card card}))}]
     {:events {:corp-turn-begins ability}
      :abilities [ability]})})
