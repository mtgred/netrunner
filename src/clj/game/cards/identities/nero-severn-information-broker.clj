(in-ns 'game.core)

(def card-definitions-identities-nero-severn-information-broker
  {"Nero Severn: Information Broker"
   {:abilities [{:req (req (has-subtype? current-ice "Sentry"))
                 :once :per-turn
                 :msg "jack out when encountering a Sentry"
                 :effect (effect (jack-out nil))}]}})
