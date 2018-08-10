(in-ns 'game.cards.assets)

(def card-definition-net-analytics
  {"Net Analytics"
   (let [ability {:req (req (seq (filter #(some #{:tag} %) targets)))
                  :effect (effect (show-wait-prompt :runner "Corp to use Net Analytics")
                                  (continue-ability :corp
                                    {:optional
                                     {:prompt "Draw from Net Analytics?"
                                      :yes-ability {:msg (msg "draw a card")
                                                    :effect (effect (draw :corp 1))}
                                      :end-effect (effect (clear-wait-prompt :runner))}}
                                    card nil))}]
     {:events {:runner-lose-tag (assoc ability :req (req (= side :runner)))
               :runner-prevent (assoc ability :req (req (seq (filter #(some #{:tag} %) targets))))}})})
