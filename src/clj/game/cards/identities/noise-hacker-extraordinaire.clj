(in-ns 'game.core)

(def card-definitions-identities-noise-hacker-extraordinaire
  {"Noise: Hacker Extraordinaire"
   {:events {:runner-install {:msg "force the Corp to trash the top card of R&D"
                              :effect (effect (mill :corp))
                              :req (req (has-subtype? target "Virus"))}}}})
