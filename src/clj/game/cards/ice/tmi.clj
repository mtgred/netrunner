(in-ns 'game.core)

(def card-definitions-ice-tmi
  {"TMI"
   {:trace {:base 2
            :msg "keep TMI rezzed"
            :unsuccessful {:effect (effect (derez card))}}
    :subroutines [end-the-run]}})
