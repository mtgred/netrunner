(in-ns 'game.cards.ice)

(def card-definition-clairvoyant-monitor
  {"Clairvoyant Monitor"
   {:subroutines [(do-psi {:label "Place 1 advancement token and end the run"
                           :player :corp
                           :prompt "Select a target for Clairvoyant Monitor"
                           :msg (msg "place 1 advancement token on "
                                     (card-str state target) " and end the run")
                           :choices {:req installed?}
                           :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                           (end-run))})]}})
