(in-ns 'game.cards.agendas)

(def card-definition-oaktown-renovation
  {"Oaktown Renovation"
   {:install-state :face-up
    :events {:advance {:req (req (= (:cid card) (:cid target)))
                       :msg (msg "gain " (if (>= (get-counters (get-card state card) :advancement) 5) "3" "2") " [Credits]")
                       :effect (req (gain-credits state side
                                          (if (>= (get-counters (get-card state card) :advancement) 5) 3 2)))}}}})
