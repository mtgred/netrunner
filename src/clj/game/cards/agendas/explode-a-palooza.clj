(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-explode-a-palooza
  {"Explode-a-palooza"
   {:flags {:rd-reveal (req true)}
    :access {:delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Explode-a-palooza")
                             (continue-ability
                               {:optional {:prompt "Gain 5 [Credits] with Explode-a-palooza ability?"
                                           :yes-ability {:msg "gain 5 [Credits]"
                                                         :effect (effect (gain :corp :credit 5)
                                                                         (clear-wait-prompt :runner))}
                                           :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil))}}})