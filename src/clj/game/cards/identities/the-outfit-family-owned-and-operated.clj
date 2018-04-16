(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-the-outfit-family-owned-and-operated
  {"The Outfit: Family Owned and Operated"
   {:events {:corp-gain-bad-publicity {:delayed-completion true
                                       :msg "gain 3 [Credit]"
                                       :effect (effect (gain :credit 3))}}}

   ;; No special implementation
   "The Professor: Keeper of Knowledge"
   {}})