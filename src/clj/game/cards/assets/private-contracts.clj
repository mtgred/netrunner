(in-ns 'game.core)

(def card-definitions-assets-private-contracts
  {"Private Contracts"
   {:effect (effect (add-counter card :credit 14))
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 2]
                 :msg "gain 2 [Credits]"
                 :effect (req (gain state :corp :credit 2)
                              (when (= (get-in card [:counter :credit]) 0)
                                (trash state :corp card)))}]}})
