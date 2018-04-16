(in-ns 'game.core)

(declare run-event)

(def card-events-demolition-run
  {"Demolition Run"
   {:req (req (or rd-runnable hq-runnable))
    :prompt "Choose a server"
    :choices ["HQ" "R&D"]
    :abilities [{:msg (msg "trash " (:title (:card (first (get-in @state [side :prompt])))) " at no cost")
                 :effect (effect (trash-no-cost))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Demolition Run in the Temporary Zone to trash a card being accessed at no cost") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (effect (trash c))}} c)))}
                     card nil))
    :events {:run-ends nil}}})