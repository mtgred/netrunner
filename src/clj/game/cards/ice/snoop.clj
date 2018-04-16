(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-snoop
  {"Snoop"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-ice card))
                 :label "Reveal all cards in the Runner's Grip"
                 :msg (msg "reveal the Runner's Grip ( " (join ", " (map :title (:hand runner))) " )")}
                {:req (req (> (get-in card [:counter :power] 0) 0))
                 :counter-cost [:power 1]
                 :label "Hosted power counter: Reveal all cards in Grip and trash 1 card"
                 :msg (msg "look at all cards in Grip and trash " (:title target)
                           " using 1 power counter")
                 :choices (req (cancellable (:hand runner) :sorted))
                 :prompt "Choose a card to trash"
                 :effect (effect (trash target))}]
    :subroutines [(trace-ability 3 add-power-counter)]}})
