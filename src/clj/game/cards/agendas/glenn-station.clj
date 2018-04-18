(in-ns 'game.core)

(def card-definitions-agendas-glenn-station
  {"Glenn Station"
   {:implementation "Doesn't prohibit hosting multiple cards"
    :abilities [{:label "Host a card from HQ on Glenn Station"
                 :cost [:click 1]
                 :msg "host a card from HQ"
                 :prompt "Choose a card to host on Glenn Station"
                 :choices (req (:hand corp))
                 :effect (effect (host card target {:facedown true}))}
                {:label "Add a card on Glenn Station to HQ"
                 :cost [:click 1]
                 :msg "add a hosted card to HQ"
                 :prompt "Choose a card on Glenn Station"
                 :choices (req (:hosted card))
                 :effect (effect (move target :hand))}]}})
