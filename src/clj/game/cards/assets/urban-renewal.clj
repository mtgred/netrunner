(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-urban-renewal
  {"Urban Renewal"
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins
             {:delayed-completion true
              :effect (req (add-counter state side card :power -1)
                           (if (<= (get-in card [:counter :power]) 1)
                             (when-completed
                               (trash state side card {:cause :ability-cost})
                               (do (system-msg state :corp "uses Urban Renewal to do 4 meat damage")
                                   (damage state side eid :meat 4 {:card card})))
                             (effect-completed state side eid)))}}}})