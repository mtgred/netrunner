(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-ghost-runner
  {"Ghost Runner"
   {:data {:counter {:credit 3}}
    :abilities [{:counter-cost [:credit 1]
                 :msg "gain 1 [Credits]"
                 :req (req (:run @state))
                 :effect (req (gain state side :credit 1)
                              (trigger-event state side :spent-stealth-credit card)
                              (when (zero? (get-in card [:counter :credit] 0))
                                (trash state :runner card {:unpreventable true})))}]}})