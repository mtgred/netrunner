(in-ns 'game.cards.hardware)

(def card-definition-doppelganger
  {"Doppelgänger"
   {:in-play [:memory 1]
    :events {:runner-install
             {:req (req (= card target))
              :silent (req true)
              :effect (effect (update! (assoc card :dopp-active true)))}
             :runner-turn-begins
             {:effect (effect (update! (assoc card :dopp-active true)))}
             :successful-run-ends
             {:interactive (req true)
              :optional
              {:req (req (:dopp-active card))
               :player :runner
               :prompt "Use Doppelgänger to run again?"
               :yes-ability {:prompt "Choose a server"
                             :async true
                             :choices (req runnable-servers)
                             :msg (msg "make a run on " target)
                             :makes-run true
                             :effect (effect (update! (dissoc card :dopp-active))
                                             (clear-wait-prompt :corp)
                                             (run eid target))}}}}}})
