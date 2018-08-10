(in-ns 'game.cards.agendas)

(def card-definition-project-wotan
  {"Project Wotan"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:req (req (and (ice? current-ice)
                                (rezzed? current-ice)
                                (has-subtype? current-ice "Bioroid")))
                 :counter-cost [:agenda 1]
                 :msg (str "make the approached piece of Bioroid ICE gain \"[Subroutine] End the run\""
                           "after all its other subroutines for the remainder of this run")}]}})
