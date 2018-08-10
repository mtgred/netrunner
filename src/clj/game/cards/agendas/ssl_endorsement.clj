(in-ns 'game.cards.agendas)

(def card-definition-ssl-endorsement
  {"SSL Endorsement"
   (let [add-credits (effect (add-counter card :credit 9))
         remove-credits {:optional {:req (req (pos? (get-counters card :credit)))
                                    :prompt "Gain 3 [Credits] from SSL Endorsement?"
                                    :yes-ability
                                    {:effect (req (when (pos? (get-counters card :credit))
                                                    (take-credits state :corp 3)
                                                    (system-msg state :corp (str "uses SSL Endorsement to gain 3 [Credits]"))
                                                    (add-counter state side card :credit -3)))}}}]
     {:effect add-credits
      :stolen {:effect add-credits}
      :interactive (req true)
      :events {:corp-turn-begins remove-credits}
      :flags {:has-events-when-stolen true}})})
