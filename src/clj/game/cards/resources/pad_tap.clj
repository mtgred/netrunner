(in-ns 'game.cards.resources)

(def card-definition-pad-tap
  {"PAD Tap"
   {:events {:corp-credit-gain
             {:req (req (and (not= target :corp-click-credit)
                             (= 1 (->> (turn-events state :corp :corp-credit-gain)
                                       (remove #(= (first %) :corp-click-credit))
                                       count))))
              :msg "gain 1 [Credits]"
              :effect (effect (gain-credits :runner 1))}}
    :corp-abilities [{:label "Trash PAD Tap"
                      :cost [:credit 3 :click 1]
                      :req (req (= :corp side))
                      :effect (effect (system-msg :corp "spends [Click] and 3 [Credits] to trash PAD Tap")
                                      (trash :corp card))}]}})
