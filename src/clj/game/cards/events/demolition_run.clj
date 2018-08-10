(in-ns 'game.cards.events)

(def card-definition-demolition-run
  {"Demolition Run"
   {:req (req (or rd-runnable hq-runnable))
    :prompt "Choose a server"
    :choices ["HQ" "R&D"]
    :effect (effect (run target nil card)
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (effect (trash c))}} c)))}
                     card nil))
    :events {:run-ends nil}
    :interactions {:trash-ability
                   {:label "[Demolition Run]: Trash card"
                    :msg (msg "trash " (:title target) " at no cost")
                    :async true
                    :effect (effect (trash-no-cost eid target))}}}})
