(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-firmware-updates
  {"Firmware Updates"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:counter-cost [:agenda 1]
                 :choices {:req #(and (ice? %)
                                      (can-be-advanced? %))}
                 :req (req (< 0 (get-in card [:counter :agenda] 0)))
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :once :per-turn
                 :effect (final-effect (add-prop target :advance-counter 1))}]}})
