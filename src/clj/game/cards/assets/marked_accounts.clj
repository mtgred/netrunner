(in-ns 'game.cards.assets)

(def card-definition-marked-accounts
  {"Marked Accounts"
   (let [ability {:msg "take 1 [Credits]"
                  :label "Take 1 [Credits] (start of turn)"
                  :once :per-turn
                  :counter-cost [:credit 1]
                  :effect (effect (take-credits 1))}]
   {:abilities [ability
                {:cost [:click 1]
                 :msg "store 3 [Credits]"
                 :effect (effect (add-counter card :credit 3))}]
    :events {:corp-turn-begins ability}})})
