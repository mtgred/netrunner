(in-ns 'game.core)

(def card-definitions-upgrades-prisec
  {"Prisec"
   {:access {:req (req (installed? card))
             :delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Prisec")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 2 [Credits] to use Prisec ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:cost [:credit 2]
                                               :msg "do 1 meat damage and give the Runner 1 tag"
                                               :delayed-completion true
                                               :effect (req (when-completed (damage state side :meat 1 {:card card})
                                                                            (tag-runner state :runner eid 1)))}}}
                               card nil))}}})
