(in-ns 'game.core)

(def card-definitions-identities-silhouette-stealth-operative
  {"Silhouette: Stealth Operative"
   {:events {:successful-run
             {:interactive (req (some #(not (rezzed? %)) (all-installed state :corp)))
              :delayed-completion true
              :req (req (and (= target :hq)
                             (first-successful-run-on-server? state :hq)))
              :effect (effect (continue-ability {:choices {:req #(and installed? (not (rezzed? %)))}
                                                 :effect (effect (expose eid target)) :msg "expose 1 card"
                                                 :delayed-completion true }
                                                card nil))}}}})
