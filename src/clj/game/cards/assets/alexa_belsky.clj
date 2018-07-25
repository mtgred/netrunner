(in-ns 'game.cards.assets)

(def card-definition-alexa-belsky
  {"Alexa Belsky"
   {:abilities [{:label "[Trash]: Shuffle all cards in HQ into R&D"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (show-wait-prompt :corp "Runner to decide whether or not to prevent Alexa Belsky")
                                 (resolve-ability
                                   {:prompt "Prevent Alexa Belsky from shuffling back in 1 card for every 2 [Credits] spent. How many credits?"
                                    :choices :credit
                                    :player :runner
                                    :priority 2
                                    :msg (msg "shuffle "
                                              (quantify (- (count (:hand corp)) (quot target 2)) "card")
                                              " in HQ into R&D")
                                    :effect (req (if (pos? (quot target 2))
                                                   (let [prevented (quot target 2)
                                                         unprevented (- (count (:hand corp)) prevented)]
                                                     (doseq [c (take unprevented (shuffle (:hand corp)))]
                                                       (move state :corp c :deck))
                                                     (when (pos? unprevented)
                                                       (shuffle! state :corp :deck))
                                                     (system-msg state :runner
                                                                 (str "pays " target " [Credits] to prevent "
                                                                      (quantify prevented "random card")
                                                                      " in HQ from being shuffled into R&D")))
                                                   (shuffle-into-deck state :corp :hand)))
                                    :end-effect (effect (clear-wait-prompt :corp))}
                                   card nil))}]}})
