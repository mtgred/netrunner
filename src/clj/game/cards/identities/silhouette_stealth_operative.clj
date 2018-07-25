(in-ns 'game.cards.identities)

(def card-definition-silhouette-stealth-operative
  {"Silhouette: Stealth Operative"
   {:events {:successful-run
             {:interactive (req (some #(not (rezzed? %)) (all-installed state :corp)))
              :async true
              :req (req (and (= target :hq)
                             (first-successful-run-on-server? state :hq)))
              :effect (effect (continue-ability {:choices {:req #(and (installed? %)
                                                                      (not (rezzed? %)))}
                                                 :effect (effect (expose eid target))
                                                 :msg "expose 1 card"
                                                 :async true}
                                                card nil))}}}})
