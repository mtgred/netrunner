(in-ns 'game.core)

(def card-definitions-agendas-posted-bounty
  {"Posted Bounty"
   {:optional {:prompt "Forfeit Posted Bounty to give the Runner 1 tag and take 1 bad publicity?"
               :yes-ability {:msg "give the Runner 1 tag and take 1 bad publicity"
                             :delayed-completion true
                             :effect (effect (gain-bad-publicity :corp eid 1)
                                             (tag-runner :runner eid 1)
                                             (forfeit card))}}}})
