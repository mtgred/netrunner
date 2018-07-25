(in-ns 'game.cards.resources)

(def card-definition-human-first
  {"Human First"
   {:events {:agenda-scored {:msg (msg "gain " (get-agenda-points state :corp target) " [Credits]")
                             :effect (effect (gain-credits :runner (get-agenda-points state :corp target)))}
             :agenda-stolen {:msg (msg "gain " (get-agenda-points state :runner target) " [Credits]")
                             :effect (effect (gain-credits (get-agenda-points state :runner target)))}}}})
