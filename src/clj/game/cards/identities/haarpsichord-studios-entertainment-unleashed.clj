(in-ns 'game.core)

(def card-definitions-identities-haarpsichord-studios-entertainment-unleashed
  {"Haarpsichord Studios: Entertainment Unleashed"
   (let [haarp (fn [state side card]
                 (if (is-type? card "Agenda")
                   ((constantly false)
                     (toast state :runner "Cannot steal due to Haarpsichord Studios." "warning"))
                   true))]
     {:events {:agenda-stolen
               {:effect (effect (register-turn-flag! card :can-steal haarp))}}
      :effect (req (when-not (first-event? state side :agenda-stolen)
                     (register-turn-flag! state side card :can-steal haarp)))
      :leave-play (effect (clear-turn-flag! card :can-steal))})})
