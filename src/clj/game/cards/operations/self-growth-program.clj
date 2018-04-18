(in-ns 'game.core)

(def card-definitions-operations-self-growth-program
  {"Self-Growth Program"
   {:req (req tagged)
    :prompt "Select two installed Runner cards"
    :choices {:req #(and (installed? %)
                         (= "Runner" (:side %)))
              :max 2}
    :msg (msg (str "move " (join ", " (map :title targets)) " to the Runner's grip"))
    :effect (req (doseq [c targets]
                   (move state :runner c :hand)))}})
