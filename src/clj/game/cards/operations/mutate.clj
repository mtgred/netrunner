(in-ns 'game.core)

(def card-operations-mutate
  {"Mutate"
   {:additional-cost [:ice 1]
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))

    :events {:corp-trash {:effect (req (let [i (ice-index state target)
                       [reveal r] (split-with (complement ice?) (get-in @state [:corp :deck]))
                       titles (->> (conj (vec reveal) (first r)) (filter identity) (map :title))]
                                           (system-msg state side (str "uses Mutate to trash " (:title target)))
                                           (when (seq titles)
                                             (system-msg state side (str "reveals " (clojure.string/join ", " titles) " from R&D")))
                                           (if-let [ice (first r)]
                                             (let [newice (assoc ice :zone (:zone target) :rezzed true)
                                                   ices (get-in @state (cons :corp (:zone target)))
                                                   newices (apply conj (subvec ices 0 i) newice (subvec ices i))]
                                               (swap! state assoc-in (cons :corp (:zone target)) newices)
                                               (swap! state update-in [:corp :deck] (fn [coll] (remove-once #(= (:cid %) (:cid newice)) coll)))
                                               (trigger-event state side :corp-install newice)
                                               (card-init state side newice {:resolve-effect false})
                                               (system-msg state side (str "uses Mutate to install and rez " (:title newice) " from R&D at no cost"))
                                               (trigger-event state side :rez newice))
                                             (system-msg state side (str "does not find any ICE to install from R&D")))
                                           (shuffle! state :corp :deck)
                                           (effect-completed state side eid card)
                                           (unregister-events state side card)))}}}})
