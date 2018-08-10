(in-ns 'game.cards.events)

(def card-definition-satellite-uplink
  {"Satellite Uplink"
   {:choices {:max 2 :req installed?}
    :async true
    :effect (req (let [[card1 card2] targets]
                   (wait-for (expose state side card1)
                             (expose state side eid card2))))}})
