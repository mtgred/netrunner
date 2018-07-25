(in-ns 'game.cards.upgrades)

(def card-definition-off-the-grid
  {"Off the Grid"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :effect (req (prevent-run-on-server state card (second (:zone card))))
    :events {:runner-turn-begins {:effect (req (prevent-run-on-server state card (second (:zone card))))}
             :successful-run {:req (req (= target :hq))
                              :effect (req (trash state :corp card)
                                           (enable-run-on-server state card
                                                                 (second (:zone card)))
                                           (system-msg state :corp (str "trashes Off the Grid")))}}
    :leave-play (req (enable-run-on-server state card (second (:zone card))))}})
