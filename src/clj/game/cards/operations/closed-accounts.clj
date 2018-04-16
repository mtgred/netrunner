(in-ns 'game.core)

(def card-operations-closed-accounts
  {"Closed Accounts"
   {:req (req tagged)
    :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
    :effect (effect (lose :runner :credit :all))}})
