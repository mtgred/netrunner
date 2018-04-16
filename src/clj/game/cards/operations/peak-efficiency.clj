(in-ns 'game.core)

(def card-operations-peak-efficiency
  {"Peak Efficiency"
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))
              " [Credits]")
    :effect (effect (gain :credit
                          (reduce (fn [c server]
                                    (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                                  0 (flatten (seq (:servers corp))))))}})
