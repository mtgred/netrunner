(in-ns 'game.core)

(def card-definitions-hardware-gpi-net-tap
  {"GPI Net Tap"
   {:implementation "Trash and jack out effect is manual"
    :abilities [{:req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :delayed-completion true
                 :effect (effect (expose eid current-ice))}]}})
