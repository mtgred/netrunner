(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-armitage-codebusting
  {"Armitage Codebusting"
   {:data {:counter {:credit 12}}
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 2]
                 :msg "gain 2 [Credits]"
                 :effect (req (gain state :runner :credit 2)
                              (when (zero? (get-in card [:counter :credit] 0))
                                (trash state :runner card {:unpreventable true})))}]}})