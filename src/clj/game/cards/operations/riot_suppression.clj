(in-ns 'game.cards.operations)

(def card-definition-riot-suppression
  {"Riot Suppression"
   {:req (req (last-turn? state :runner :trashed-card))
    :async true
    :effect (req (let [c card]
                   (show-wait-prompt state :corp "Runner to decide if they will take 1 brain damage")
                   (continue-ability
                     state :runner
                     {:optional
                      {:prompt "Take 1 brain damage to prevent having 3 fewer clicks next turn?"
                       :player :runner
                       :end-effect (req (clear-wait-prompt state :corp)
                                        (move state :corp (find-latest state c) :rfg))
                       :yes-ability
                       {:async true
                        :effect (req (system-msg
                                       state :runner
                                       "suffers 1 brain damage to prevent losing 3[Click] to Riot Suppression")
                                     (damage state :runner eid :brain 1 {:card card}))}
                       :no-ability
                       {:msg "give the runner 3 fewer [Click] next turn"
                        :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil #(- % 3) 0)))}}}
                     card nil)))}})
