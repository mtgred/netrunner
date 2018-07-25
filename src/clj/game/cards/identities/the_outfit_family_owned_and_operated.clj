(in-ns 'game.cards.identities)

(def card-definition-the-outfit-family-owned-and-operated
  {"The Outfit: Family Owned and Operated"
   {:events {:corp-gain-bad-publicity {:msg "gain 3 [Credit]"
                                       :effect (effect (gain-credits 3))}}}})
