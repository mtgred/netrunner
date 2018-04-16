(in-ns 'game.core)

(def card-hardware-polyhistor
  {"Polyhistor"
   (let [abi {:optional
              {:prompt "Draw 1 card to force the Corp to draw 1 card?"
               :yes-ability {:msg "draw 1 card and force the Corp to draw 1 card"
                             :effect (effect (draw :runner 1)
                                             (draw :corp 1))}
               :no-ability {:effect (req (system-msg state side (str "does not use Polyhistor"))
                                         (effect-completed state side eid))}}}]
     {:in-play [:link 1 :memory 1]
      :events {:pass-ice {:req (req (and (= (:server run) [:hq]) (= (:position run) 1) ; trigger when last ICE passed
                                         (pos? (count (:deck runner)))))
                          :delayed-completion true
                          :once :per-turn
                          :effect (req (continue-ability state :runner abi card nil))}
               :run {:req (req (and (= (:server run) [:hq]) (= (:position run) 0) ; trigger on unprotected HQ
                                    (pos? (count (:deck runner)))))
                     :delayed-completion true
                     :once :per-turn
                     :effect (req (continue-ability state :runner abi card nil))}}})})