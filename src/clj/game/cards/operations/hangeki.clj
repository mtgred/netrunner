(in-ns 'game.cards.operations)

(def card-definition-hangeki
  {"Hangeki"
   {:req (req (last-turn? state :runner :trashed-card))
    :async true
    :prompt "Choose an installed Corp card"
    :choices {:req #(and (= (:side %) "Corp")
                         (installed? %))}
    :effect (effect (show-wait-prompt :corp "Runner to resolve Hangeki")
                    (continue-ability
                      {:optional
                       {:player :runner
                        :async true
                        :prompt "Access card? (If not, add Hangeki to your score area worth -1 agenda point)"
                        :yes-ability
                        {:effect (req (clear-wait-prompt state :corp)
                                      (wait-for (access-card state side target)
                                                (move state :corp (find-latest state card) :rfg)
                                                (system-msg state :corp "removes Hangeki from the game")
                                                (effect-completed state side eid)))}
                        :no-ability
                        {:msg "add it to the Runner's score area as an agenda worth -1 agenda point"
                         :effect (effect (clear-wait-prompt :corp)
                                         (as-agenda :runner eid (find-latest state card) -1))}}}
                      card targets))}})
