(in-ns 'game.cards.assets)

(def card-definition-reversed-accounts
  {"Reversed Accounts"
   {:advanceable :always
    :abilities [{:cost [:click 1]
                 :label "Force the Runner to lose 4 [Credits] per advancement"
                 :msg (msg "force the Runner to lose " (min (* 4 (get-counters card :advancement)) (:credit runner)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (lose-credits :runner (* 4 (get-counters card :advancement))))}]}})
