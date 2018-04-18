(in-ns 'game.core)

(def card-definitions-resources-fall-guy
  {"Fall Guy"
   {:prevent {:trash [:resource]}
    :abilities [{:label "[Trash]: Prevent another installed resource from being trashed"
                 :effect (effect (trash-prevent :resource 1) (trash card {:unpreventable true :cause :ability-cost}))}
                {:label "[Trash]: Gain 2 [Credits]"
                 :effect (effect (trash card {:cause :ability-cost}) (gain :credit 2)) :msg "gain 2 [Credits]"}]}})
