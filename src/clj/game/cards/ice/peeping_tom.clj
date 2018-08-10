(in-ns 'game.cards.ice)

(def card-definition-peeping-tom
  {"Peeping Tom"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-ice card))
                 :label "Name a card type and reveal all cards in the Runner's Grip"
                 :prompt "Choose a card type"
                 :choices ["Event" "Hardware" "Program" "Resource"]
                 :effect (req (let [n (count (filter #(is-type? % target) (:hand runner)))]
                                (system-msg state side (str "uses Peeping Tom to name " target ", then reveals "
                                                            (join ", " (map :title (:hand runner)))
                                                            " in the Runner's Grip. Peeping Tom gains " n " subroutines"))))}]
    :runner-abilities [{:label "End the run"
                        :effect (req (end-run state :runner)
                                     (system-msg state :runner "chooses to end the run"))}
                       {:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :runner "chooses to take 1 tag from Peeping Tom")
                                     (gain-tags state :runner eid 1))}]}})
