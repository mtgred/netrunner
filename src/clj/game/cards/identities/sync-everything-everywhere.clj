(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-sync-everything-everywhere
  {"SYNC: Everything, Everywhere"
   {:effect (req (when (> (:turn @state) 1)
                   (if (:sync-front card)
                     (tag-remove-bonus state side -1)
                     (trash-resource-bonus state side 2))))
    :events {:pre-first-turn {:req (req (= side :corp))
                              :effect (effect (update! (assoc card :sync-front true)) (tag-remove-bonus -1))}}
    :abilities [{:cost [:click 1]
                 :effect (req (if (:sync-front card)
                                (do (tag-remove-bonus state side 1)
                                    (trash-resource-bonus state side 2)
                                    (update! state side (-> card (assoc :sync-front false) (assoc :code "sync"))))
                                (do (tag-remove-bonus state side -1)
                                    (trash-resource-bonus state side -2)
                                    (update! state side (-> card (assoc :sync-front true) (assoc :code "09001"))))))
                 :msg (msg "flip their ID")}]
    :leave-play (req (if (:sync-front card)
                       (tag-remove-bonus state side 1)
                       (trash-resource-bonus state side -2)))}})