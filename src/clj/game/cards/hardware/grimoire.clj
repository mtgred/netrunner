(in-ns 'game.cards.hardware)

(def card-definition-grimoire
  {"Grimoire"
   {:in-play [:memory 2]
    :events {:runner-install {:silent (req true)
                              :req (req (has-subtype? target "Virus"))
                              :effect (effect (add-counter target :virus 1))}}}})
