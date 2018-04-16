(in-ns 'game.core)

(declare run-event)

(def card-events-blackmail
  {"Blackmail"
   (run-event
    {:req (req has-bad-pub)
     :msg "prevent ICE from being rezzed during this run"}
    nil
    (effect (register-run-flag!
              card
              :can-rez
              (fn [state side card]
                (if (ice? card)
                  ((constantly false)
                    (toast state :corp "Cannot rez ICE on this run due to Blackmail"))
                  true)))))})
