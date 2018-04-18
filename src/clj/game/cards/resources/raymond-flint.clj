(in-ns 'game.core)

(def card-definitions-resources-raymond-flint
  {"Raymond Flint"
   {:effect (req (add-watch state :raymond-flint
                            (fn [k ref old new]
                              (when (< (get-in old [:corp :bad-publicity]) (get-in new [:corp :bad-publicity]))
                                (when-completed
                                  ; manually trigger the pre-access event to alert Nerve Agent.
                                  (trigger-event-sync ref side :pre-access :hq)
                                  (let [from-hq (access-count state side :hq-access)]
                                    (resolve-ability
                                      ref side
                                      (access-helper-hq
                                        state from-hq
                                        ; see note in Gang Sign
                                        (set (get-in @state [:corp :servers :hq :content])))
                                      card nil)))))))
    :leave-play (req (remove-watch state :raymond-flint))
    :abilities [{:msg "expose 1 card"
                 :choices {:req installed?}
                 :delayed-completion true
                 :effect (effect (expose eid target) (trash card {:cause :ability-cost}))}]}})
