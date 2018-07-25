(in-ns 'game.cards.icebreakers)

(def card-definition-cradle
  {"Cradle"
   {:abilities [(break-sub 2 0 "Code Gate")]
    :events {:card-moved {:silent (req true)
                          :req (req (and (= "Runner" (:side target))
                                         (= [:hand] (or (:zone target)
                                                        (:previous-zone target)))))
                          :effect (effect (update-breaker-strength card))}
             :runner-draw {:silent (req true)
                           :req (req (when-let [drawn (-> @state :runner :register :most-recent-drawn first)]
                                       (= [:hand] (or (:zone drawn)
                                                      (:previous-zone drawn)))))
                           :effect (effect (update-breaker-strength card))} }
    :strength-bonus (req (-> @state :runner :hand count -))}})
