(in-ns 'game.cards.hardware)

(def card-definition-dinosaurus
  {"Dinosaurus"
   {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Select a non-AI icebreaker in your Grip to install on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                 :effect (effect (runner-install target {:host-card card :no-mu true})
                                 (update! (assoc-in (get-card state card) [:special :dino-breaker] (:cid target))))}
                {:label "Host an installed non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Select an installed non-AI icebreaker to host on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (free-mu state (:memoryunits target))
                              (->> target
                                (get-card state)
                                (host state side card)
                                (update-breaker-strength state side))
                              (update! state side (assoc-in (get-card state card) [:special :dino-breaker] (:cid target))))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}
             :card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :dino-breaker])))
                          :effect (effect (update! (dissoc-in card [:special :dino-breaker]))
                                          (use-mu (:memoryunits target)))}}}})
