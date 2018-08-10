(in-ns 'game.cards.ice)

(def card-definition-universal-connectivity-fee
  {"Universal Connectivity Fee"
   {:subroutines [{:label "Force the Runner to lose credits"
                   :msg (msg "force the Runner to lose " (if tagged "all credits" "1 [Credits]"))
                   :effect (req (if tagged
                                  (do (lose-credits state :runner :all)
                                      (lose state :runner :run-credit :all)
                                      (when current-ice
                                        (no-action state side nil)
                                        (continue state side nil))
                                      (trash state side card))
                                  (lose-credits state :runner 1)))}]}})
