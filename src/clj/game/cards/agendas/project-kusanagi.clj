(in-ns 'game.core)

(def card-definitions-agendas-project-kusanagi
  {"Project Kusanagi"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (- (:advance-counter card) 2)))
    :abilities [{:counter-cost [:agenda 1]
                 :msg "make a piece of ICE gain \"[Subroutine] Do 1 net damage\" after all its other subroutines for the remainder of the run"}]}})
