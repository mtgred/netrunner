(in-ns 'game.cards.operations)

(def card-definition-mass-commercialization
  {"Mass Commercialization"
   {:msg (msg "gain " (* 2 (count (filter #(pos? (+ (get-counters % :advancement) (:extra-advance-counter % 0)))
                                          (get-all-installed state)))) " [Credits]")
    :effect (effect (gain-credits (* 2 (count (filter #(pos? (+ (get-counters % :advancement) (:extra-advance-counter % 0)))
                                                      (get-all-installed state))))))}})
