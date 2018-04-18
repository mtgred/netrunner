(in-ns 'game.core)

(def card-definitions-ice-upayoga
  {"Upayoga"
   {:implementation "\"Resolve a subroutine...\" subroutine is not implemented"
    :subroutines [(do-psi {:label "Make the Runner lose 2 [Credits]"
                           :msg "make the Runner lose 2 [Credits]"
                           :effect (effect (lose :runner :credit 2))})
                  {:msg "resolve a subroutine on a piece of rezzed psi ICE"}]}})
