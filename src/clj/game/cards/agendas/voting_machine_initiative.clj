(in-ns 'game.cards.agendas)

(def card-definition-voting-machine-initiative
  {"Voting Machine Initiative"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :events {:runner-turn-begins
             {:async true
              :req (req (pos? (get-counters card :agenda)))
              :effect (effect (show-wait-prompt :runner "Corp to use Voting Machine Initiative")
                              (continue-ability
                                {:optional
                                 {:player :corp
                                  :prompt "Use Voting Machine Initiative to make the Runner lose 1 [Click]?"
                                  :yes-ability {:msg "make the Runner lose 1 [Click]"
                                                :effect (effect (lose :runner :click 1)
                                                                (add-counter card :agenda -1)
                                                                (clear-wait-prompt :runner))}
                                  :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                                card nil))}}}})
