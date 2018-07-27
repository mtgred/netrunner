(in-ns 'game.cards.programs)

(def card-definition-crescentus
  {"Crescentus"
   {:implementation "Does not check that all subroutines were broken"
    :abilities [{:req (req (rezzed? current-ice))
                 :label "Derez a piece of ice"
                 :msg (msg "derez " (:title current-ice))
                 :effect (effect (trash card {:cause :ability-cost}) (derez current-ice))}]}})
