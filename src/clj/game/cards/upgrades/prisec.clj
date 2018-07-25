(in-ns 'game.cards.upgrades)

(def card-definition-prisec
  {"Prisec"
   {:access {:req (req (installed? card))
             :async true
             :effect (effect (show-wait-prompt :runner "Corp to use Prisec")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 2 [Credits] to use Prisec ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:cost [:credit 2]
                                               :msg "do 1 meat damage and give the Runner 1 tag"
                                               :async true
                                               :effect (req (wait-for (damage state side :meat 1 {:card card})
                                                                      (gain-tags state :corp eid 1)))}}}
                               card nil))}}})
