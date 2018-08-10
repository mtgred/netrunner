(in-ns 'game.cards.agendas)

(def card-definition-nisei-mk-ii
  {"Nisei MK II"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 1))
    :abilities [{:req (req (:run @state))
                 :counter-cost [:agenda 1]
                 :msg "end the run"
                 :effect (effect (end-run))}]}})
