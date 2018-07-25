(in-ns 'game.cards.hardware)

(def card-definition-minds-eye
  {"Mind's Eye"
   {:in-play [:memory 1]
    :implementation "Power counters added automatically"
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:async true
                 :cost [:click 1]
                 :counter-cost [:power 3]
                 :msg "access the top card of R&D"
                 :effect (req (do-access state side eid [:rd] {:no-root true}))}]}})
