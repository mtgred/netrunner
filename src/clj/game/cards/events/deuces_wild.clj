(in-ns 'game.cards.events)

(def card-definition-deuces-wild
  {"Deuces Wild"
   (let [all [{:effect (effect (gain-credits 3))
               :msg "gain 3 [Credits]"}
              {:effect (effect (draw 2))
               :msg "draw 2 cards"}
              {:effect (effect (lose-tags 1))
               :msg "remove 1 tag"}
              {:prompt "Select 1 piece of ice to expose"
               :msg "expose 1 ice and make a run"
               :choices {:req #(and (installed? %) (ice? %))}
               :async true
               :effect (req (wait-for (expose state side target)
                                      (continue-ability
                                        state side
                                        {:prompt "Choose a server"
                                         :choices (req runnable-servers)
                                         :async true
                                         :effect (effect (game.core/run eid target))}
                                        card nil)))}]
         choice (fn choice [abis]
                  {:prompt "Choose an ability to resolve"
                   :choices (map #(capitalize (:msg %)) abis)
                   :async true
                   :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                  (wait-for
                                    (resolve-ability state side chosen card nil)
                                    (if (= (count abis) 4)
                                      (continue-ability state side (choice (remove-once #(= % chosen) abis)) card nil)
                                      (effect-completed state side eid)))))})]
     {:async true
      :effect (effect (continue-ability (choice all) card nil))})})
