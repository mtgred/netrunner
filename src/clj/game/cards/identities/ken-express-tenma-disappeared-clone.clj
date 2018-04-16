(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-ken-express-tenma-disappeared-clone
  {"Ken \"Express\" Tenma: Disappeared Clone"
   {:events {:play-event {:req (req (and (has-subtype? target "Run")
                                         (empty? (filter #(has-subtype? % "Run")
                                                         ;; have to flatten because each element is a list containing
                                                         ;; the Event card that was played
                                                         (flatten (turn-events state :runner :play-event))))))
                          :msg "gain 1 [Credits]"
                          :effect (effect (gain :credit 1))}}}})