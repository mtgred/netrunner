(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-chiyashi
  {"Chiyashi"
   {:implementation "Trash effect when using an AI to break is activated manually"
    :abilities [{:label "Trash the top 2 cards of the Runner's Stack"
                 :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                 :msg (msg (str "trash " (join ", " (map :title (take 2 (:deck runner)))) " from the Runner's Stack"))
                 :effect (effect (mill :corp :runner 2))}]
    :subroutines [(do-net-damage 2)
                  end-the-run]}})