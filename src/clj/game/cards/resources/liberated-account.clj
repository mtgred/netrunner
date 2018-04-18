(in-ns 'game.core)

(def card-definitions-resources-liberated-account
  {"Liberated Account"
   {:data {:counter {:credit 16}}
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 4]
                 :msg "gain 4 [Credits]"
                 :effect (req (gain state :runner :credit 4)
                              (when (<= (get-in card [:counter :credit] 0) 0)
                                (trash state :runner card {:unpreventable true})))}]}})
