(in-ns 'game.cards.identities)

(def card-definition-weyland-consortium-building-a-better-world
  {"Weyland Consortium: Building a Better World"
   {:events {:play-operation {:msg "gain 1 [Credits]"
                              :effect (effect (gain-credits 1))
                              :req (req (has-subtype? target "Transaction"))}}}})
