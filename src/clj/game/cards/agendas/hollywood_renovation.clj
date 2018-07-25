(in-ns 'game.cards.agendas)

(def card-definition-hollywood-renovation
  {"Hollywood Renovation"
   {:install-state :face-up
    :events {:advance
             {:async true
              :req (req (= (:cid card)
                           (:cid target)))
              :effect (req (let [n (if (>= (get-counters (get-card state card) :advancement) 6) 2 1)]
                             (continue-ability state side
                              {:choices {:req #(and (not= (:cid %)
                                                          (:cid card))
                                                    (can-be-advanced? %))}
                               :msg (msg "place " n
                                         " advancement tokens on "
                                         (card-str state target))
                               :effect (effect (add-prop :corp target :advance-counter n {:placed true}))}
                              card nil)))}}}})
