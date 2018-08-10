(in-ns 'game.cards.hardware)

(def card-definition-maya
  {"Maya"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :async true
                 :label "Move this accessed card to bottom of R&D"
                 :req (req (when-let [accessed-card (-> @state :runner :prompt first :card)]
                             (in-deck? accessed-card)))
                 :msg "move the card just accessed to the bottom of R&D"
                 :effect (req (let [accessed-card (-> @state :runner :prompt first :card)]
                                (move state :corp accessed-card :deck)
                                (wait-for (gain-tags state :runner (make-eid state) 1)
                                          (close-access-prompt state side))))}
                {:once :per-turn
                 :label "Move a previously accessed card to bottom of R&D"
                 :effect (effect (resolve-ability
                                   {:async true
                                    ;; only allow targeting cards that were accessed this turn
                                    :choices {:req #(some (fn [accessed-card]
                                                            (= (:cid %) (:cid accessed-card)))
                                                          (map first (turn-events state side :access)))}
                                    :msg (msg "move " (:title target) " to the bottom of R&D")
                                    :effect (req (move state :corp target :deck)
                                                 (gain-tags state :runner eid 1)
                                                 (swap! state update-in [side :prompt] rest)
                                                 (when-let [run (:run @state)]
                                                   (when (and (:ended run)
                                                              (empty? (get-in @state [:runner :prompt])))
                                                     (handle-end-run state :runner))))}
                                   card nil))}]}})
