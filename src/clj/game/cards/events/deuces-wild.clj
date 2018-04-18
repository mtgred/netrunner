(in-ns 'game.core)

(def card-definitions-events-deuces-wild
  {"Deuces Wild"
   (let [all [{:effect (effect (gain :credit 3))
               :msg "gain 3 [Credits]"}
              {:effect (effect (draw 2))
               :msg "draw 2 cards"}
              {:effect (effect (lose :tag 1))
               :msg "remove 1 tag"}
              {:prompt "Select 1 piece of ice to expose"
               :msg "expose 1 ice and make a run"
               :choices {:req #(and (installed? %) (ice? %))}
               :delayed-completion true
               :effect (req (when-completed (expose state side target)
                                            (continue-ability
                                              state side
                                              {:prompt "Choose a server"
                                               :choices (req runnable-servers)
                                               :delayed-completion true
                                               :effect (effect (game.core/run eid target))}
                                              card nil)))}]
         choice (fn choice [abis]
                  {:prompt "Choose an ability to resolve"
                   :choices (map #(capitalize (:msg %)) abis)
                   :delayed-completion true
                   :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                  (when-completed
                                    (resolve-ability state side chosen card nil)
                                    (if (= (count abis) 4)
                                      (continue-ability state side (choice (remove-once #(= % chosen) abis)) card nil)
                                      (effect-completed state side eid)))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (choice all) card nil))})})
