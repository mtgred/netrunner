(in-ns 'game.core)

(def card-definitions-ice-data-loop
  {"Data Loop"
   {:implementation "Encounter effect is manual"
    :subroutines [end-the-run-if-tagged
                  end-the-run]
    :runner-abilities [{:label "Add 2 cards from your Grip to the top of the Stack"
                        :req (req (pos? (count (:hand runner))))
                        :effect (req (let [n (min 2 (count (:hand runner)))]
                                       (resolve-ability state side
                                         {:prompt (msg "Choose " n " cards in your Grip to add to the top of the Stack (first card targeted will be topmost)")
                                          :choices {:max n :all true
                                                    :req #(and (in-hand? %) (= (:side %) "Runner"))}
                                          :effect (req (doseq [c targets]
                                                         (move state :runner c :deck {:front true}))
                                                       (system-msg state :runner (str "adds " n " cards from their Grip to the top of the Stack")))}
                                        card nil)))}]}})
