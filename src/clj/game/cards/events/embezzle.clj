(in-ns 'game.cards.events)

(def card-definition-embezzle
  {"Embezzle"
   (letfn [(name-string [cards]
             (join " and " (map :title cards)))] ; either 'card' or 'card1 and card2'
    {:req (req hq-runnable)
     :effect (effect
              (run :hq {:req (req (= target :hq))
                        :replace-access
                        {:mandatory true
                         :msg (msg "reveal 2 cards from HQ and trash all "
                                   target (when (not= "ICE" (:type target)) "s"))
                         :prompt "Choose a card type"
                         :choices ["Asset" "Upgrade" "Operation" "ICE"]
                         :effect (req (let [chosen-type target
                                            cards-to-reveal (take 2 (shuffle (:hand corp)))
                                            cards-to-trash (filter #(is-type? % chosen-type) cards-to-reveal)]
                                        (system-msg state side (str " reveals " (name-string cards-to-reveal) " from HQ"))
                                        (when-not (empty? cards-to-trash)
                                          (system-msg state side (str " trashes " (name-string cards-to-trash)
                                                                      " from HQ and gain " (* 4 (count cards-to-trash)) "[Credits]"))
                                          (doseq [c cards-to-trash]
                                            (trash state :runner (assoc c :seen true)))
                                          (gain-credits state :runner (* 4 (count cards-to-trash))))))}}
                card))})})
