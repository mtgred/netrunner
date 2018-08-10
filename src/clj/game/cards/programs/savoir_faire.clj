(in-ns 'game.cards.programs)

(def card-definition-savoir-faire
  {"Savoir-faire"
   {:abilities [{:cost [:credit 2]
                 :label "Install program from grip"
                 :once :per-turn
                 :req (req (not (install-locked? state side)))
                 :msg (msg "install " (:title target))
                 :prompt "Choose a program to install from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (in-hand? %))}
                 :effect (effect (runner-install target))}]}})
