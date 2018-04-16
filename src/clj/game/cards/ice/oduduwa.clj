(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-oduduwa
  {"Oduduwa"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement counter on Oduduwa"
                 :msg (msg "place 1 advancement counter on Oduduwa")
                 :effect (req (add-prop state side card :advance-counter 1 {:placed true}))}
                {:label "Place X advancement token on another piece of ice"
                 :msg (msg "place " (:advance-counter card 0) " advancement token on " (card-str state target))
                 :choices {:req ice?
                           :not-self (req (:cid card))}
                 :effect (req (add-prop state side target :advance-counter (:advance-counter card 0) {:placed true}))}]
    :subroutines [end-the-run]}})
