(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-ssl-endorsement
  {"SSL Endorsement"
   (let [add-credits (effect (add-counter card :credit 9))
         remove-credits {:optional {:req (req (pos? (get-in card [:counter :credit] -1)))
                                    :prompt "Gain 3 [Credits] from SSL Endorsement?"
                                    :yes-ability
                                    {:effect (req (when (pos? (get-in card [:counter :credit] -1))
                                                    (gain state :corp :credit 3)
                                                    (system-msg state :corp (str "uses SSL Endorsement to gain 3 [Credits]"))
                                                    (add-counter state side card :credit -3)))}}}]
     {:effect add-credits
      :stolen {:effect add-credits}
      :interactive (req true)
      :events {:corp-turn-begins remove-credits}
      :flags {:has-events-when-stolen true}})})
