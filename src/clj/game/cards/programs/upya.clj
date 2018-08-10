(in-ns 'game.cards.programs)

(def card-definition-upya
  {"Upya"
   {:implementation "Power counters added automatically"
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1]
                 :counter-cost [:power 3]
                 :once :per-turn
                 :msg "gain [Click][Click]"
                 :effect (effect (gain :click 2))}]}})
