(in-ns 'game.core)

(declare run-event)

(def card-events-satellite-uplink
  {"Satellite Uplink"
   {:choices {:max 2 :req installed?}
    :delayed-completion true
    :effect (req (let [[card1 card2] targets]
                   (when-completed (expose state side card1)
                                   (expose state side eid card2))))}})
