(in-ns 'game.cards.programs)

(def card-definition-hivemind
  {"Hivemind"
   (let [update-programs (req (doseq [p (filter #(and (has-subtype? % "Icebreaker")
                                                      (has-subtype? % "Virus"))
                                                (all-active-installed state :runner))]
                                (update-breaker-strength state side p)))]
     {:data {:counter {:virus 1}}
      :effect update-programs
      :trash-effect {:effect update-programs}
      :events {:counter-added {:req (req (= (:cid target)
                                            (:cid card)))
                               :effect update-programs}}
      :abilities [{:req (req (pos? (get-counters card :virus)))
                   :priority true
                   :label "Move a virus counter"
                   :prompt "Move a virus counter to which card?"
                   :choices {:req #(has-subtype? % "Virus")}
                   :effect (req (let [abilities (:abilities (card-def target))
                                      virus target]
                                  (add-counter state :runner virus :virus 1)
                                  (add-counter state :runner card :virus -1)
                                  (if (= (count abilities) 1)
                                    (do (swap! state update-in [side :prompt] rest) ; remove the Hivemind prompt so Imp works
                                        (resolve-ability state side (first abilities) (get-card state virus) nil))
                                    (resolve-ability
                                      state side
                                      {:prompt "Choose an ability to trigger"
                                       :choices (vec (map :msg abilities))
                                       :effect (req (swap! state update-in [side :prompt] rest)
                                                    (resolve-ability
                                                      state side
                                                      (first (filter #(= (:msg %) target) abilities))
                                                      card nil))}
                                      (get-card state virus) nil))))
                   :msg (msg "trigger an ability on " (:title target))}]})})
