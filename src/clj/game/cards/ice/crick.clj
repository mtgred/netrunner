(in-ns 'game.core)

(def card-definitions-ice-crick
  {"Crick"
   {:subroutines [{:label "install a card from Archives"
                   :prompt "Select a card to install from Archives"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (= (:zone %) [:discard])
                                        (= (:side %) "Corp"))}
                   :msg (msg (corp-install-msg target))
                   :effect (effect (corp-install target nil))}]
    :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))}})
