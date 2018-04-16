(in-ns 'game.core)

(def card-hardware-spy-camera
  {"Spy Camera"
   {:abilities [{:cost [:click 1]
                 :delayed-completion true
                 :label "Look at the top X cards of your Stack"
                 :msg "look at the top X cards of their Stack and rearrange them"
                 :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their stack")
                              (let [n (count (filter #(= (:title %) (:title card))
                                                     (all-active-installed state :runner)))
                                    from (take n (:deck runner))]
                                (if (pos? (count from))
                                  (continue-ability state side (reorder-choice :runner :corp from '()
                                                                               (count from) from) card nil)
                                  (do (clear-wait-prompt state :corp)
                                      (effect-completed state side eid card)))))}
                {:label "[Trash]: Look at the top card of R&D"
                 :msg "trash it and look at the top card of R&D"
                 :effect (effect (prompt! card (str "The top card of R&D is " (:title (first (:deck corp)))) ["OK"] {})
                                 (trash card {:cause :ability-cost}))}]}})