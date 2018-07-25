(in-ns 'game.cards.hardware)

(def card-definition-gebrselassie
  {"Gebrselassie"
   {:abilities [{:msg (msg "host it on an installed non-AI icebreaker")
                 :cost [:click 1]
                 :choices {:req #(and (installed? %)
                                      (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI")))}
                 :effect (req (when-let [host (get-card state (:host card))]
                                (update! state side (dissoc-in host [:pump :all-turn]))
                                (update-breaker-strength state side host))
                              (host state side target card))}]
    :events {:pump-breaker {:silent (req true)
                            :req (req (= (:cid (second targets)) (:cid (:host card))))
                            :effect (effect (update! (update-in (second targets) [:pump :all-turn] (fnil #(+ % (first targets)) 0)))
                                            (update-breaker-strength (second targets)))}}
    :leave-play (req (when-let [host (get-card state (:host card))]
                       (update! state side (dissoc-in host [:pump :all-turn]))
                       (update-breaker-strength state side host)))}})
