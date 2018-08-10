(in-ns 'game.cards.identities)

(def card-definition-419-amoral-scammer
  {"419: Amoral Scammer"
   {:events {:corp-install
             {:async true
              :req (req (and (first-event? state :corp :corp-install)
                             (pos? (:turn @state))
                             (not (rezzed? target))))
              :effect
              (req (show-wait-prompt state :corp "Runner to use 419: Amoral Scammer")
                     (let [itarget target]
                       (continue-ability
                         state side
                         {:optional
                          {:prompt "Expose installed card unless Corp pays 1 [Credits]?"
                           :player :runner
                           :no-ability {:effect (req (clear-wait-prompt state :corp))}
                           :yes-ability
                           {:async true
                            :effect (req (clear-wait-prompt state :corp)
                                         (if (not (can-pay? state :corp nil :credit 1))
                                           (do
                                             (toast state :corp "Cannot afford to pay 1 credit to block card exposure" "info")
                                             (expose state side eid itarget))
                                           (do
                                             (show-wait-prompt state :runner "Corp decision")
                                             (continue-ability
                                               state side
                                               {:optional
                                                {:prompt "Pay 1 [Credits] to prevent exposure of installed card?"
                                                 :player :corp
                                                 :no-ability
                                                 {:async true
                                                  :effect (req (expose state side eid itarget)
                                                               (clear-wait-prompt state :runner))}
                                                 :yes-ability
                                                 {:effect (req (pay state :corp card [:credit 1])
                                                               (system-msg state :corp (str "spends 1 [Credits] to prevent "
                                                                                            " card from being exposed"))
                                                               (clear-wait-prompt state :runner))}}}
                                               card nil))))}}}
                         card nil)))}}}})
