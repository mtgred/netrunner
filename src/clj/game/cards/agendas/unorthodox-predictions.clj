(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-unorthodox-predictions
  {"Unorthodox Predictions"
   {:implementation "Prevention of subroutine breaking is not enforced"
    :delayed-completion false
    :prompt "Choose an ICE type for Unorthodox Predictions"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "prevent subroutines on " target " ICE from being broken until next turn.")}})