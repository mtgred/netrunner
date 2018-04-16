(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-hollywood-renovation
  {"Hollywood Renovation"
   {:install-state :face-up
    :events {:advance
             {:req (req (= (:cid card)
                           (:cid target)))
              :effect (req (let [n (if (>= (:advance-counter (get-card state card)) 6) 2 1)]
                             (continue-ability state side
                              {:choices {:req #(and (not= (:cid %)
                                                          (:cid card))
                                                    (can-be-advanced? %))}
                               :msg (msg "place " n
                                         " advancement tokens on "
                                         (card-str state target))
                               :effect (final-effect (add-prop :corp target :advance-counter n {:placed true}))}
                              card nil)))}}}})