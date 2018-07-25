(in-ns 'game.cards.events)

(def card-definition-tinkering
  {"Tinkering"
   {:prompt "Select a piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       stypes (:subtype ice)]
              (resolve-ability
                 state :runner
                 {:msg (msg "make " (card-str state ice) " gain Sentry, Code Gate, and Barrier until the end of the turn")
                  :effect (effect (update! (assoc ice :subtype (combine-subtypes true (:subtype ice) "Sentry" "Code Gate" "Barrier")))
                                  (update-ice-strength (get-card state ice))
                                  (add-icon card (get-card state ice) "T" "green")
                                  (register-events {:runner-turn-ends
                                                    {:effect (effect (remove-icon card (get-card state ice))
                                                                     (update! (assoc (get-card state ice) :subtype stypes)))}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:runner-turn-ends nil}}})
