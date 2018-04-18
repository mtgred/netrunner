(in-ns 'game.core)

(def card-definitions-ice-next-opal
  {"NEXT Opal"
   {:subroutines [{:label "Install a card from HQ, paying all costs"
                   :prompt "Choose a card in HQ to install"
                   :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (in-hand? %)
                                        (= (:side %) "Corp"))}
                   :effect (effect (corp-install target nil))
                   :msg (msg (corp-install-msg target))}]}})
