(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-snare!
  {"Snare!"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Snare!")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 4 [Credits] to use Snare! ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:delayed-completion true
                                               :cost [:credit 4]
                                               :msg "do 3 net damage and give the Runner 1 tag"
                                               :effect (req (when-completed (damage state side :net 3 {:card card})
                                                                            (tag-runner state :runner eid 1)))}}}
                               card nil))}}})
