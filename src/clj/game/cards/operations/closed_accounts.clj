(in-ns 'game.cards.operations)

(def card-definition-closed-accounts
  {"Closed Accounts"
   {:req (req tagged)
    :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
    :effect (effect (lose-credits :runner :all))}})
