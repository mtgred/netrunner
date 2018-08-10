(in-ns 'game.cards.operations)

(def card-definition-psychographics
  {"Psychographics"
   {:req (req tagged)
    :choices :credit
    :prompt "How many credits?"
    :async true
    :effect (req (let [c (min target (:tag runner))]
                   (continue-ability state side
                                     {:msg (msg "place " c " advancement tokens on "
                                                (card-str state target))
                                      :choices {:req can-be-advanced?}
                                      :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                     card nil)))}})
