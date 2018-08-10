(in-ns 'game.cards.events)

(def card-definition-system-outage
  {"System Outage"
   {:events {:corp-draw {:req (req (not (first-event? state side :corp-draw)))
                         :msg "force the Corp to lose 1 [Credits]"
                         :effect (effect (lose-credits :corp 1))}}}})
