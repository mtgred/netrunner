(in-ns 'game.core)

(def card-definitions-resources-keros-mcintyre
  {"Keros Mcintyre"
   {:events
    {:derez
     {:req (req (and (first-event? state side :derez)
                     (= (second targets) :runner)))
      :once :per-turn
      :msg "gain 2 [Credits]"
      :effect (effect (gain :credit 2))}}}})
