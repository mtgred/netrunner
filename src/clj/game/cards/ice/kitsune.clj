(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-kitsune
  {"Kitsune"
   {:subroutines [{:prompt "Select a card in HQ to force access"
                   :choices {:req in-hand?}
                   :label "Force the Runner to access a card in HQ"
                   :msg (msg "force the Runner to access " (:title target))
                   :effect (req (trash state side card)
                                (when-completed (handle-access state side targets)
                                  (when-completed (trigger-event-sync state side :pre-access :hq)
                                    (let [from-hq (dec (access-count state side :hq-access))]
                                      (continue-ability
                                        state :runner
                                        (access-helper-hq
                                          state from-hq
                                          ; access-helper-hq uses a set to keep track of which cards have already
                                          ; been accessed. by adding HQ root's contents to this set, we make the runner
                                          ; unable to access those cards, as Kitsune intends.
                                          (conj (set (get-in @state [:corp :servers :hq :content])) target))
                                       card nil)))))}]}})
