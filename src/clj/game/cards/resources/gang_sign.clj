(in-ns 'game.cards.resources)

(def card-definition-gang-sign
  {"Gang Sign"
   {:events {:agenda-scored
             {:async true
              :interactive (req true)
              :msg (msg "access " (quantify (get-in @state [:runner :hq-access]) "card") " from HQ")
              :effect (req (wait-for
                             ; manually trigger the pre-access event to alert Nerve Agent.
                             (trigger-event-sync state side :pre-access :hq)
                             (let [from-hq (access-count state side :hq-access)]
                               (continue-ability
                                 state :runner
                                 (access-helper-hq
                                   state from-hq
                                   ; access-helper-hq uses a set to keep track of which cards have already
                                   ; been accessed. by adding HQ root's contents to this set, we make the runner
                                   ; unable to access those cards, as Gang Sign intends.
                                   (set (get-in @state [:corp :servers :hq :content])))
                                 card nil))))}}}})
