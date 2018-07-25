(in-ns 'game.cards.hardware)

(def card-definition-maw
  {"Maw"
   (let [ability {:label "Trash a card from HQ"
                  :req (req (and (= 1 (get-in @state [:runner :register :no-trash-or-steal]))
                                 (pos? (count (:hand corp)))
                                 (not= (first (:zone target)) :discard)))
                  :once :per-turn
                  :msg "force the Corp to trash a random card from HQ"
                  :effect (req (let [card-to-trash (first (shuffle (:hand corp)))
                                     card-seen? (= (:cid target) (:cid card-to-trash))
                                     card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                                  card-to-trash)]
                                 ;; toggle access flag to prevent Hiro issue #2638
                                 (swap! state dissoc :access)
                                 (trash state :corp card-to-trash)
                                 (swap! state assoc :access true)))}]
     {:in-play [:memory 2]
      :abilities [ability]
      :events {:post-access-card ability}})})
