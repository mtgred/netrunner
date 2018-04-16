(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-noise-hacker-extraordinaire
  {"Noise: Hacker Extraordinaire"
   {:events {:runner-install {:msg "force the Corp to trash the top card of R&D"
                              :effect (effect (mill :corp))
                              :req (req (has-subtype? target "Virus"))}}}})
