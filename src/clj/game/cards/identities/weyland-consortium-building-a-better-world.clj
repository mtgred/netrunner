(in-ns 'game.core)

(def card-definitions-identities-weyland-consortium-building-a-better-world
  {"Weyland Consortium: Building a Better World"
   {:events {:play-operation {:msg "gain 1 [Credits]"
                              :effect (effect (gain :credit 1))
                              :req (req (has-subtype? target "Transaction"))}}}})
