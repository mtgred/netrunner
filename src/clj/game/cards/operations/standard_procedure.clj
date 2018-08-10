(in-ns 'game.cards.operations)

(def card-definition-standard-procedure
  {"Standard Procedure"
   {:req (req (last-turn? state :runner :successful-run))
    :prompt "Choose a card type"
    :choices ["Event" "Hardware" "Program" "Resource"]
    :effect (req (let [n (* 2 (count (filter #(is-type? % target) (:hand runner))))]
                   (gain-credits state :corp n)
                   (system-msg state side (str "uses Standard Procedure to name " target ", reveal "
                                               (join ", " (map :title (:hand runner)))
                                               " in the Runner's Grip, and gain " n " [Credits]"))))}})
