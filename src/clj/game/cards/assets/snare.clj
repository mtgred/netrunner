(in-ns 'game.cards.assets)

(def card-definition-snare
  {"Snare!"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :async true
             :effect (effect (show-wait-prompt :runner "Corp to use Snare!")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 4 [Credits] to use Snare! ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:async true
                                               :cost [:credit 4]
                                               :msg "do 3 net damage and give the Runner 1 tag"
                                               :effect (req (wait-for (damage state side :net 3 {:card card})
                                                                      (gain-tags state :corp eid 1)))}}}
                               card nil))}}})
