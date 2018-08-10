(in-ns 'game.cards.events)

(def card-definition-office-supplies
  {"Office Supplies"
   {:play-cost-bonus (req [:credit (- (:link runner 0))])
    :effect (effect (continue-ability
                      {:prompt "Gain 4 [Credits] or draw 4 cards?"
                       :choices ["Gain 4 [Credits]" "Draw 4 cards"]
                       :effect (req (cond
                                      (= target "Gain 4 [Credits]")
                                      (gain-credits state :runner 4)
                                      (= target "Draw 4 cards")
                                      (draw state :runner 4)))}
                      card nil))}})
