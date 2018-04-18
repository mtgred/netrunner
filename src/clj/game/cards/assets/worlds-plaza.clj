(in-ns 'game.core)

(def card-definitions-assets-worlds-plaza
  {"Worlds Plaza"
   {:abilities [{:label "Install an asset on Worlds Plaza"
                 :req (req (< (count (:hosted card)) 3))
                 :cost [:click 1]
                 :prompt "Select an asset to install on Worlds Plaza"
                 :choices {:req #(and (is-type? % "Asset")
                                      (in-hand? %)
                                      (= (:side %) "Corp"))}
                 :msg (msg "host " (:title target))
                 :effect (req (corp-install state side target card) ;; install target onto card
                              (rez-cost-bonus state side -2) (rez state side (last (:hosted (get-card state card)))))}]}})
