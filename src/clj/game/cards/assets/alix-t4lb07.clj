(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-alix-t4lb07
  {"Alix T4LB07"
   {:events {:corp-install {:effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1] :label "Gain 2 [Credits] for each counter on Alix T4LB07"
                 :msg (msg "gain " (* 2 (get-in card [:counter :power] 0)) " [Credits]")
                 :effect (effect (gain :credit (* 2 (get-in card [:counter :power] 0)))
                                 (trash card))}]}})
