(in-ns 'game.cards.resources)

(def card-definition-sacrificial-clone
  {"Sacrificial Clone"
   {:interactions {:prevent [{:type #{:net :brain :meat}
                              :req (req true)}]}
    :abilities [{:effect (req (doseq [c (concat (get-in runner [:rig :hardware])
                                                (filter #(not (has-subtype? % "Virtual"))
                                                        (get-in runner [:rig :resource]))
                                                (:hand runner))]
                                (trash state side c {:cause :ability-cost}))
                              (lose-credits state side :all)
                              (lose-tags state side :all)
                              (lose state side :run-credit :all)
                              (damage-prevent state side :net Integer/MAX_VALUE)
                              (damage-prevent state side :meat Integer/MAX_VALUE)
                              (damage-prevent state side :brain Integer/MAX_VALUE))}]}})
