(in-ns 'game.cards.agendas)

(def card-definition-firmware-updates
  {"Firmware Updates"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:counter-cost [:agenda 1]
                 :choices {:req #(and (ice? %)
                                      (can-be-advanced? %))}
                 :req (req (pos? (get-counters card :agenda)))
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :once :per-turn
                 :effect (effect (add-prop target :advance-counter 1))}]}})
