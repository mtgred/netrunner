(in-ns 'game.cards.agendas)

(def card-definition-unorthodox-predictions
  {"Unorthodox Predictions"
   {:async false
    :implementation "Prevention of subroutine breaking is not enforced"
    :prompt "Choose an ICE type for Unorthodox Predictions"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "prevent subroutines on " target " ICE from being broken until next turn.")}})
