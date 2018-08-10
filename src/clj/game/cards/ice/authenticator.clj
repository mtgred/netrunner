(in-ns 'game.cards.ice)

(def card-definition-authenticator
  {"Authenticator"
   {:implementation "Encounter effect is manual"
    :abilities [(give-tags 1)]
    :runner-abilities [{:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Authenticator to Bypass it")
                                     (gain-tags state :runner eid 1 {:unpreventable true}))}]
    :subroutines [(gain-credits-sub 2)
                  end-the-run]}})
