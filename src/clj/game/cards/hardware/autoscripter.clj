(in-ns 'game.cards.hardware)

(def card-definition-autoscripter
  {"Autoscripter"
   {:events {:runner-install {:silent (req true)
                              :req (req (and (is-type? target "Program")
                                             ;; only trigger on Runner's turn
                                             (= (:active-player @state) :runner)
                                             ;; only trigger when playing a Program from grip
                                             (some #{:hand} (:previous-zone target))
                                             ;; check that we haven't played a Program from the grip this turn
                                             ;; which translates to just one case of playing a Program in turn-events
                                             (first-event? state :runner :runner-install
                                                           (fn [[card _]] (and (some #{:hand} (:previous-zone card))
                                                                               (is-type? card "Program"))))))
                              :msg "gain [Click]"
                              :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (trash card)
                                                (system-msg "trashes Autoscripter"))}}}})
