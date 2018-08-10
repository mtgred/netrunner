(in-ns 'game.cards.events)

(def card-definition-credit-crash
  {"Credit Crash"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:pre-access-card
             {:once :per-run
              :async true
              :req (req (not= (:type target) "Agenda"))
              :effect (req (let [c target
                                 cost (:cost c)
                                 title (:title c)]
                             (if (can-pay? state :corp nil :credit cost)
                               (do (show-wait-prompt state :runner "Corp to decide whether or not to prevent the trash")
                                   (continue-ability state :corp
                                     {:optional
                                      {:prompt (msg "Spend " cost " [Credits] to prevent the trash of " title "?")
                                       :player :corp
                                       :yes-ability {:effect (req (lose-credits state :corp cost)
                                                                  (system-msg state :corp (str "spends " cost " [Credits] to prevent "
                                                                                               title " from being trashed at no cost"))
                                                                  (clear-wait-prompt state :runner))}
                                       :no-ability {:msg (msg "trash " title " at no cost")
                                                    :async true
                                                    :effect (effect (clear-wait-prompt :runner)
                                                                    (trash-no-cost eid c))}}}
                                    card nil))
                               (do (system-msg state side (str "uses Credit Crash to trash " title " at no cost"))
                                   (trash-no-cost state side eid c)))))}
             :run-ends {:effect (effect (unregister-events card))}}}})
