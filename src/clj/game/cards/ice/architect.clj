(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-architect
  {"Architect"
   {:flags {:untrashable-while-rezzed true}
    :subroutines [{:label "Look at the top 5 cards of R&D"
                   :prompt "Choose a card to install"
                   :priority true
                   :activatemsg "uses Architect to look at the top 5 cards of R&D"
                   :req (req (and (not (string? target))
                                  (not (is-type? target "Operation"))))
                   :not-distinct true
                   :choices (req (conj (take 5 (:deck corp)) "No install"))
                   :effect (effect (system-msg (str "chooses the card in position "
                                                    (+ 1 (.indexOf (take 5 (:deck corp)) target))
                                                    " from R&D (top is 1)"))
                                   (corp-install (move state side target :play-area) nil {:no-install-cost true}))}
                  {:label "Install a card from HQ or Archives"
                   :prompt "Select a card to install from Archives or HQ"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (#{[:hand] [:discard]} (:zone %))
                                        (= (:side %) "Corp"))}
                   :effect (effect (corp-install target nil))
                   :msg (msg (corp-install-msg target))}]}})
