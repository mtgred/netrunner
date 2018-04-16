(in-ns 'game.core)

(def card-hardware-maya
  {"Maya"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :delayed-completion true
                 :label "Move this accessed card to bottom of R&D"
                 :req (req (when-let [c (:card (first (get-in @state [:runner :prompt])))]
                             (in-deck? c)))
                 :msg "move the card just accessed to the bottom of R&D"
                 :effect (req (let [c (:card (first (get-in @state [:runner :prompt])))]
                                (when (is-type? c "Agenda") ; trashing before the :access events actually fire; fire them manually
                                  (resolve-steal-events state side c))
                                (move state :corp c :deck)
                                (when-completed (tag-runner state :runner (make-eid state) 1)
                                                (close-access-prompt state side))))}
                {:once :per-turn
                 :label "Move a previously accessed card to bottom of R&D"
                 :effect (effect (resolve-ability
                                   {; only allow targeting cards that were accessed this turn -- not perfect, but good enough?
                                    :delayed-completion true
                                    :choices {:req #(some (fn [c] (= (:cid %) (:cid c)))
                                                          (map first (turn-events state side :access)))}
                                    :msg (msg "move " (:title target) " to the bottom of R&D")
                                    :effect (req (move state :corp target :deck)
                                                 (tag-runner state :runner eid 1)
                                                 (swap! state update-in [side :prompt] rest)
                                                 (when-let [run (:run @state)]
                                                   (when (and (:ended run) (empty? (get-in @state [:runner :prompt])))
                                                     (handle-end-run state :runner))))} card nil))}]}})
