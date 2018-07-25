(in-ns 'game.cards.hardware)

(def card-definition-gpi-net-tap
  {"GPI Net Tap"
   {:implementation "Trash and jack out effect is manual"
    :abilities [{:req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :async true
                 :effect (effect (expose eid current-ice))}]}})
