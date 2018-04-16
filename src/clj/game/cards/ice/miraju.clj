(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-miraju
  {"Mirāju"
   {:abilities [{:label "Runner broke subroutine: Redirect run to Archives"
                 :msg "make the Runner continue the run on Archives. Mirāju is derezzed"
                 :effect (req (swap! state update-in [:run]
                                     #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                               :server [:archives]))
                              (derez state side card))}]
    :subroutines [{:label "Draw 1 card, then shuffle 1 card from HQ into R&D"
                   :effect (req (when-completed (resolve-ability state side
                                                  {:optional
                                                   {:delayed-completion true
                                                    :prompt "Draw 1 card?"
                                                    :yes-ability {:msg "draw 1 card"
                                                                  :effect (effect (draw))}
                                                    :no-ability {:effect (req (effect-completed state side eid))}}}
                                                 card nil)
                                                (resolve-ability state side
                                                  {:prompt "Choose 1 card in HQ to shuffle into R&D"
                                                   :choices {:req #(and (in-hand? %) (= (:side %) "Corp"))}
                                                   :msg "shuffle 1 card in HQ into R&D"
                                                   :effect (effect (move target :deck)
                                                                   (shuffle! :deck))}
                                                 card nil)))}]}})
