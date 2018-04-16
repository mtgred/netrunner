(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-puppet-master
  {"Puppet Master"
   {:events {:successful-run
             {:interactive (req true)
              :delayed-completion true
              :effect (req (show-wait-prompt state :runner "Corp to use Puppet Master")
                           (continue-ability
                             state :corp
                             {:prompt "Select a card to place 1 advancement token on"
                              :player :corp
                              :choices {:req can-be-advanced?}
                              :cancel-effect (final-effect (clear-wait-prompt :runner))
                              :msg (msg "place 1 advancement token on " (card-str state target))
                              :effect (final-effect (add-prop :corp target :advance-counter 1 {:placed true})
                                                    (clear-wait-prompt :runner))} card nil))}}}})