(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-net-analytics
  {"Net Analytics"
   (let [ability {:req (req (not (empty? (filter #(some #{:tag} %) targets))))
                  :effect (effect (show-wait-prompt :runner "Corp to use Net Analytics")
                                  (continue-ability :corp
                                    {:optional
                                     {:prompt "Draw from Net Analytics?"
                                      :yes-ability {:msg (msg "draw a card")
                                                    :effect (effect (draw :corp)
                                                                    (clear-wait-prompt :runner))}
                                      :no-ability {:effect (effect (system-msg :corp "does not draw from Net Analytics")
                                                                   (clear-wait-prompt :runner))}}} card nil))}]
     {:events {:runner-loss ability
               :runner-prevent ability}})})