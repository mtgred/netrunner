(in-ns 'game.cards.operations)

(def card-definition-under-the-bus
  {"Under the Bus"
   {:req (req (and (last-turn? state :runner :accessed-cards)
                   (not-empty (filter
                                #(and (is-type? % "Resource")
                                      (has-subtype? % "Connection"))
                                (all-active-installed state :runner)))))
    :prompt "Choose a connection to trash"
    :choices {:req #(and (= (:side %) "Runner")
                         (is-type? % "Resource")
                         (has-subtype? % "Connection")
                         (installed? %))}
    :msg (msg "trash " (:title target) " and take 1 bad publicity")
    :async true
    :effect (req (wait-for (trash state side target nil)
                           (gain-bad-publicity state :corp eid 1)))}})
