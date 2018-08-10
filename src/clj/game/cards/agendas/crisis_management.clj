(in-ns 'game.cards.agendas)

(def card-definition-crisis-management
  {"Crisis Management"
   (let [ability {:req (req tagged)
                  :async true
                  :label "Do 1 meat damage (start of turn)"
                  :once :per-turn
                  :msg "do 1 meat damage"
                  :effect (effect (damage eid :meat 1 {:card card}))}]
     {:events {:corp-turn-begins ability}
      :abilities [ability]})})
