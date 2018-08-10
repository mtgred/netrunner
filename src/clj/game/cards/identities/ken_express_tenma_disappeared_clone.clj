(in-ns 'game.cards.identities)

(def card-definition-ken-express-tenma-disappeared-clone
  {"Ken \"Express\" Tenma: Disappeared Clone"
   {:events {:play-event {:req (req (and (has-subtype? target "Run")
                                         (first-event? state :runner :play-event #(has-subtype? (first %) "Run"))))
                          :msg "gain 1 [Credits]"
                          :effect (effect (gain-credits 1))}}}})
