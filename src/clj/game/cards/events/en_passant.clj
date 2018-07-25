(in-ns 'game.cards.events)

(def card-definition-en-passant
  {"En Passant"
   {:req (req (:successful-run runner-reg))
    :effect (req (let [runtgt (first (flatten (turn-events state side :run)))
                       serv (zone->name runtgt)]
                   (resolve-ability state side
                     {:prompt (msg "Choose an unrezzed piece of ICE protecting " serv " that you passed on your last run")
                      :choices {:req #(and (ice? %)
                                           (not (rezzed? %)))}
                      :msg (msg "trash " (card-str state target))
                      :effect (req (trash state side target)
                                   (swap! state assoc-in [:runner :register :trashed-card] true))}
                    card nil)))}})
