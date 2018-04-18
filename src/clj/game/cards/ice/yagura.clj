(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-yagura
  {"Yagura"
   {:subroutines [(do-net-damage 1)
                  {:msg "look at the top card of R&D"
                   :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                              :yes-ability {:effect (effect (move (first (:deck corp)) :deck)
                                                            (do (system-msg state side "uses Yagura to move the top card of R&D to the bottom")))}
                              :no-ability {:effect (req (system-msg state :corp (str "does not use Yagura to move the top card of R&D to the bottom")))}}}]}})
