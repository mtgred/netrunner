(in-ns 'game.cards.programs)

(def card-definition-plague
  {"Plague"
   {:prompt "Choose a server for Plague" :choices (req servers)
    :msg (msg "target " target)
    :req (req (not (get-in card [:special :server-target])))
    :effect (effect (update! (assoc-in card [:special :server-target] target)))
    :events {:successful-run
             {:req (req (= (zone->name (get-in @state [:run :server]))
                           (get-in (get-card state card) [:special :server-target])))
              :msg "gain 2 virus counters"
              :effect (effect (add-counter :runner card :virus 2))}}}})
