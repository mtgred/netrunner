(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-marked-accounts
  {"Marked Accounts"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :counter-cost [:credit 1]
                  :effect (effect (gain :credit 1))}]
   {:abilities [ability
                {:cost [:click 1]
                 :msg "store 3 [Credits]"
                 :effect (effect (add-counter card :credit 3))}]
    :events {:corp-turn-begins ability}})})
