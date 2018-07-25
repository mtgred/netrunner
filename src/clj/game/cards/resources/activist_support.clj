(in-ns 'game.cards.resources)

(def card-definition-activist-support
  {"Activist Support"
   {:events
    {:corp-turn-begins {:async true
                        :effect (req (if (zero? (:tag runner))
                                       (do (gain-tags state :runner eid 1)
                                           (system-msg state :runner (str "uses " (:title card) " to take 1 tag")))
                                       (effect-completed state :runner eid)))}
     :runner-turn-begins {:async true
                          :effect (req (if (not has-bad-pub)
                                         (do (gain-bad-publicity state :corp eid 1)
                                             (system-msg state :runner
                                                         (str "uses " (:title card) " to give the corp 1 bad publicity")))
                                         (effect-completed state :runner eid)))}}}})
