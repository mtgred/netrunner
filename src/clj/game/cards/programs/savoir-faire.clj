(in-ns 'game.core)

(def card-definitions-programs-savoir-faire
  {"Savoir-faire"
   {:abilities [{:cost [:credit 2]
                 :once :per-turn
                 :req (req (not (install-locked? state side)))
                 :msg (msg "install " (:title target))
                 :prompt "Choose a program to install from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (in-hand? %))}
                 :effect (effect (runner-install target))}]}})
