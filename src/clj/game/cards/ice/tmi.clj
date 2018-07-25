(in-ns 'game.cards.ice)

(def card-definition-tmi
  {"TMI"
   {:trace {:base 2
            :msg "keep TMI rezzed"
            :label "Keep TMI rezzed"
            :unsuccessful {:effect (effect (derez card))}}
    :subroutines [end-the-run]}})
