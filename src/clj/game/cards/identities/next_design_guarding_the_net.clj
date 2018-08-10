(in-ns 'game.cards.identities)

(def card-definition-next-design-guarding-the-net
  {"NEXT Design: Guarding the Net"
   (letfn [(ndhelper [n]
             {:prompt (msg "When finished, click NEXT Design: Guarding the Net to draw back up to 5 cards in HQ. "
                           "Select a piece of ICE in HQ to install:")
              :choices {:req #(and (= (:side %) "Corp")
                                   (ice? %)
                                   (in-hand? %))}
              :effect (req (corp-install state side target nil)
                           (when (< n 3)
                             (resolve-ability state side (ndhelper (inc n)) card nil)))})]
     {:events {:pre-first-turn {:req (req (= side :corp))
                                :msg "install up to 3 pieces of ICE and draw back up to 5 cards"
                                :effect (effect (resolve-ability (ndhelper 1) card nil)
                                                (update! (assoc card :fill-hq true)))}}
      :abilities [{:req (req (:fill-hq card))
                   :label "Draw back up to 5 cards"
                   :msg (msg "draw " (- 5 (count (:hand corp))) " cards")
                   :effect (req (draw state side (- 5 (count (:hand corp))))
                                (update! state side (dissoc card :fill-hq))
                                (swap! state dissoc :turn-events))}]})})
