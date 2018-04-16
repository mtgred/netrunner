(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-human-first
  {"Human First"
   {:events {:agenda-scored {:msg (msg "gain " (get-agenda-points state :corp target) " [Credits]")
                             :effect (effect (gain :runner :credit (get-agenda-points state :corp target)))}
             :agenda-stolen {:msg (msg "gain " (get-agenda-points state :runner target) " [Credits]")
                             :effect (effect (gain :credit (get-agenda-points state :runner target)))}}}})
