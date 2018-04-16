(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-marcus-batty
  {"Marcus Batty"
   {:abilities [{:req (req this-server)
                 :label "[Trash]: Start a Psi game"
                 :msg "start a Psi game"
                 :psi {:not-equal {:prompt "Select a rezzed piece of ICE to resolve one of its subroutines"
                                   :choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "resolve a subroutine on " (:title target))}}
                 :effect (effect (trash card))}]}})