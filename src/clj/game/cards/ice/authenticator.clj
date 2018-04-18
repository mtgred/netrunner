(in-ns 'game.core)

(def card-definitions-ice-authenticator
  {"Authenticator"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag]
    :runner-abilities [{:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Authenticator to Bypass it")
                                     (tag-runner state :runner eid 1 {:unpreventable true}))}]
    :subroutines [(gain-credits 2)
                  end-the-run]}})
