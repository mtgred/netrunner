(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-tithonium
  {"Tithonium"
   {:alternative-cost [:forfeit]
    :implementation "Does not handle UFAQ for Pawn or Blackguard interaction"
    :cannot-host true
    :subroutines [trash-program
                  end-the-run
                  {:label "Trash a resource"
                   :msg (msg "trash " (:title target))
                   :delayed-completion true
                   :choices {:req #(and (installed? %)
                                        (is-type? % "Resource"))}
                   :effect (effect (trash target {:reason :subroutine}))}]}})