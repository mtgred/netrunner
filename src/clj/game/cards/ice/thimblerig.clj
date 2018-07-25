(in-ns 'game.cards.ice)

(def card-definition-thimblerig
  {"Thimblerig"
   {:flags {:corp-phase-12 (req (>= (count (filter ice? (all-installed state :corp))) 2))}
    :implementation "Does not restrict usage of swap ability to start of turn or after pass"
    :abilities [{:label "Swap Thimblerig with a piece of ice"
                 :prompt "Choose a piece of ice to swap Thimblerig with"
                 :choices {:req ice?
                           :not-self true}
                 :effect (effect (swap-ice card target))}]
    :subroutines [end-the-run]}})
