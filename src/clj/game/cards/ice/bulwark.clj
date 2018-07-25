(in-ns 'game.cards.ice)

(def card-definition-bulwark
  {"Bulwark"
   {:effect take-bad-pub
    :abilities [{:msg "gain 2 [Credits] if there is an installed AI"
                 :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                 :effect (effect (gain-credits 2))}]
    :subroutines [(assoc trash-program :player :runner
                                       :msg "force the Runner to trash 1 program"
                                       :label "The Runner trashes 1 program")
                  {:msg "gain 2 [Credits] and end the run"
                   :effect (effect (gain-credits 2)
                                   (end-run))}]}})
