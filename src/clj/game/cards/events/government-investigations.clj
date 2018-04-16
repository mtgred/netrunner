(in-ns 'game.core)

(declare run-event)

(def card-events-government-investigations
  {"Government Investigations"
   {:flags {:psi-prevent-spend (req 2)}}})
