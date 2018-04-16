(in-ns 'game.core)

(def card-hardware-autoscripter
  {"Autoscripter"
   {:events {:runner-install {:silent (req true)
                              :req (req (and (is-type? target "Program")
                                             (= (:active-player @state) :runner)
                                             ;; only trigger when played a programm from grip
                                             (some #{:hand} (:previous-zone target))
                                             ;; check if didn't played a program from the grip this turn
                                             (empty? (let [cards (map first (turn-events state side :runner-install))
                                                           progs (filter #(is-type? % "Program") cards)]
                                                          (filter #(some #{:hand} (:previous-zone %)) progs)))))
                              :msg "gain [Click]" :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (trash card)
                                                (system-msg "trashes Autoscripter"))}}}})
