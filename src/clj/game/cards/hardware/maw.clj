(in-ns 'game.core)

(def card-definitions-hardware-maw
  {"Maw"
   (let [ability {:label "Trash a card from HQ"
                  :req (req (and (first-event? state side :no-trash)
                                 (first-event? state side :no-steal)
                                 (pos? (count (:hand corp)))
                                 (not= (first (:zone target)) :discard)))
                  :once :per-turn
                  :msg "force the Corp to trash a random card from HQ"
                  :effect (req (let [card-to-trash (first (shuffle (:hand corp)))
                                     card-seen? (= (:cid target) (:cid card-to-trash))
                                     card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                                  card-to-trash)]
                                 (trash state :corp card-to-trash)))}]
     {:in-play [:memory 2]
      :abilities [ability]
      :events {:no-trash ability
               :no-steal ability}})})
