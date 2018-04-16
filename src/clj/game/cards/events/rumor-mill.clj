(in-ns 'game.core)

(declare run-event)

(def card-events-rumor-mill
  {"Rumor Mill"
   (letfn [(eligible? [card] (and (:uniqueness card)
                                  (or (card-is? card :type "Asset")
                                      (card-is? card :type "Upgrade"))
                                  (not (has-subtype? card "Region"))))
           (rumor [state] (filter eligible? (concat (all-installed state :corp)
                                  (get-in @state [:corp :hand])
                                  (get-in @state [:corp :deck])
                                  (get-in @state [:corp :discard]))))]
   {:leave-play (req (doseq [c (rumor state)]
                       (enable-card state :corp c)))
    :effect (req (doseq [c (rumor state)]
                   (disable-card state :corp c)))
    :events {:corp-install {:req (req (eligible? target))
                            :effect (effect (disable-card :corp target))}}})})
