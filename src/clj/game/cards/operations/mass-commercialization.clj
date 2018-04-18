(in-ns 'game.core)

(def card-definitions-operations-mass-commercialization
  {"Mass Commercialization"
   {:msg (msg "gain " (* 2 (count (filter #(pos? (+ (:advance-counter % 0) (:extra-advance-counter % 0)))
                                          (get-all-installed state)))) " [Credits]")
    :effect (effect (gain :credit (* 2 (count (filter #(pos? (+ (:advance-counter % 0) (:extra-advance-counter % 0)))
                                                      (get-all-installed state))))))}})
