(in-ns 'game.core)

(def card-definitions-resources-grifter
  {"Grifter"
   {:events {:runner-turn-ends
             {:effect (req (let [ab (if (get-in @state [:runner :register :successful-run])
                                      {:effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}
                                      {:effect (effect (trash card)) :msg "trash Grifter"})]
                             (resolve-ability state side ab card targets)))}}}})
