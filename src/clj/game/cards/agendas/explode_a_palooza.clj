(in-ns 'game.cards.agendas)

(def card-definition-explode-a-palooza
  {"Explode-a-palooza"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (effect (show-wait-prompt :runner "Corp to use Explode-a-palooza")
                             (continue-ability
                               {:optional {:prompt "Gain 5 [Credits] with Explode-a-palooza ability?"
                                           :yes-ability {:msg "gain 5 [Credits]"
                                                         :effect (effect (gain-credits :corp 5)
                                                                         (clear-wait-prompt :runner))}
                                           :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil))}}})
