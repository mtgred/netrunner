(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-baba-yaga
  {"Baba Yaga"
   (let [host-click {:cost [:click 1]
                     :label "Install a non-AI icebreaker on Baba Yaga"
                     :prompt "Choose a non-AI icebreaker in your Grip to install on Baba Yaga"
                     :choices {:req #(and (has-subtype? % "Icebreaker")
                                          (not (has-subtype? % "AI"))
                                          (in-hand? %))}
                     :effect (effect (runner-install target {:host-card card}))}
         host-free {:label "Host an installed non-AI icebreaker on Baba Yaga"
                    :prompt "Choose an installed non-AI icebreaker to host on Baba Yaga"
                    :choices {:req #(and (has-subtype? % "Icebreaker")
                                         (not (has-subtype? % "AI"))
                                         (installed? %))}
                    :effect (req (when (host state side card target)
                                   (gain :memory (:memoryunits target))))}
         gain-abis (req (let [new-abis (mapcat (fn [c] (map-indexed #(assoc %2 :dynamic :copy, :source (:title c)
                                                                               :index %1, :label (make-label %2))
                                                                    (filter #(not= :manual-state (:ability-type %))
                                                                            (:abilities (card-def c)))))
                                               (:hosted card))]
                          (update! state :runner (assoc card :abilities (concat new-abis [host-click host-free])))))]
   {:abilities [host-click host-free]
    :hosted-gained gain-abis
    :hosted-lost gain-abis})})