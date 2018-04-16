(in-ns 'game.core)

(declare run-event)

(def card-events-credit-crash
  {"Credit Crash"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:pre-access-card
             {:once :per-run
              :delayed-completion true
              :req (req (not= (:type target) "Agenda"))
              :effect (req (let [c target
                                 cost (:cost c)
                                 title (:title c)]
                             (if (can-pay? state :corp nil :credit cost)
                               (do (show-wait-prompt state :runner "Corp to decide whether or not to prevent the trash")
                                   (continue-ability state :corp
                                     {:optional
                                      {:delayed-completion true
                                       :prompt (msg "Spend " cost " [Credits] to prevent the trash of " title "?")
                                       :player :corp
                                       :yes-ability {:effect (req (lose state :corp :credit cost)
                                                                  (system-msg state :corp (str "spends " cost " [Credits] to prevent "
                                                                                               title " from being trashed at no cost"))
                                                                  (clear-wait-prompt state :runner))}
                                       :no-ability {:msg (msg "trash " title " at no cost")
                                                    :effect (effect (clear-wait-prompt :runner)
                                                                    (resolve-trash-no-cost c))}}}
                                    card nil))
                               (do (resolve-trash-no-cost state side c)
                                   (system-msg state side (str "uses Credit Crash to trash " title " at no cost"))
                                   (effect-completed state side eid)))))}
             :run-ends {:effect (effect (unregister-events card))}}}})
