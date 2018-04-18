(in-ns 'game.core)

(def card-definitions-ice-conundrum
  {"Conundrum"
   {:subroutines [(assoc trash-program :player :runner
                                       :msg "force the Runner to trash 1 program"
                                       :label "The Runner trashes 1 program")
                  {:msg "force the Runner to lose 1 [Click] if able"
                   :effect runner-loses-click}
                  end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "AI") (all-active-installed state :runner)) 3 0))}})
