(in-ns 'game.core)

(def card-hardware-dinosaurus
  {"Dinosaurus"
   {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Select a non-AI icebreaker in your Grip to install on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card})
                                 (update! (assoc (get-card state card) :dino-breaker (:cid target))))}
                {:label "Host an installed non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Select an installed non-AI icebreaker to host on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (update-breaker-strength state side (host state side card target))
                              (update! state side (assoc (get-card state card) :dino-breaker (:cid target)))
                              (gain state side :memory (:memoryunits target)))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}
             :card-moved {:req (req (= (:cid target) (:dino-breaker (get-card state card))))
                          :effect (effect (update! (dissoc card :dino-breaker))
                                          (lose :memory (:memoryunits target)))}}}})
