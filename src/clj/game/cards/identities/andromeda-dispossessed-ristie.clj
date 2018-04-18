(in-ns 'game.core)

(def card-definitions-identities-andromeda-dispossessed-ristie
  {"Andromeda: Dispossessed Ristie"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (draw 4 {:suppress-event true}))}}
    :mulligan (effect (draw 4 {:suppress-event true}))}})
