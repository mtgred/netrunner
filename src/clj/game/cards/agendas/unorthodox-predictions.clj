(in-ns 'game.core)

(def card-definitions-agendas-unorthodox-predictions
  {"Unorthodox Predictions"
   {:implementation "Prevention of subroutine breaking is not enforced"
    :delayed-completion false
    :prompt "Choose an ICE type for Unorthodox Predictions"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "prevent subroutines on " target " ICE from being broken until next turn.")}})
