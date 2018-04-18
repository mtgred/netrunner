(in-ns 'game.core)

(def card-definitions-identities-the-outfit-family-owned-and-operated
  {"The Outfit: Family Owned and Operated"
   {:events {:corp-gain-bad-publicity {:delayed-completion true
                                       :msg "gain 3 [Credit]"
                                       :effect (effect (gain :credit 3))}}}

   ;; No special implementation
   "The Professor: Keeper of Knowledge"
   {}})
