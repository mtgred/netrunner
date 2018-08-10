(in-ns 'game.cards.agendas)

(def card-definition-posted-bounty
  {"Posted Bounty"
   {:optional {:prompt "Forfeit Posted Bounty to give the Runner 1 tag and take 1 bad publicity?"
               :yes-ability {:msg "give the Runner 1 tag and take 1 bad publicity"
                             :async true
                             :effect (effect (gain-bad-publicity :corp eid 1)
                                             (gain-tags :corp eid 1)
                                             (forfeit card))}}}})
