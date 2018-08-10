(in-ns 'game.cards.hardware)

(def card-definition-acacia
  {"Acacia"
   {:events {:pre-purge {:effect (req (let [counters (number-of-virus-counters state)]
                                        (update! state side (assoc-in (get-card state card) [:special :numpurged] counters))))}
             :purge {:async true
                     :effect (effect (show-wait-prompt  :corp "Runner to decide if they will use Acacia")
                                     (continue-ability
                                       {:optional
                                        {:player :runner
                                         :prompt "Use Acacia?"
                                         :yes-ability {:effect (req (let [counters (- (get-in (get-card state card) [:special :numpurged])
                                                                                      (number-of-virus-counters state))]
                                                                      (gain-credits state side counters)
                                                                      (system-msg state side (str "uses Acacia and gains " counters "[Credit]"))
                                                                      (trash state side card)
                                                                      (clear-wait-prompt state :corp)
                                                                      (effect-completed state side eid)))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                                       card nil))}}}})
