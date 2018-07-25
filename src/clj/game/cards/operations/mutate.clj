(in-ns 'game.cards.operations)

(def card-definition-mutate
  {"Mutate"
   {:req (req (some #(and (ice? %)
                          (rezzed? %))
                    (all-installed state :corp)))
    :prompt "Select a rezzed piece of ice to trash"
    :choices {:req #(and (ice? %)
                         (rezzed? %))}
    :async true
    :effect (req (let [i (ice-index state target)
                       [reveal r] (split-with (complement ice?) (get-in @state [:corp :deck]))
                       titles (->> (conj (vec reveal) (first r))
                                   (filter identity)
                                   (map :title))]
                   (wait-for (trash state :corp target nil)
                             (do
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
                                   (card-init state side newice {:resolve-effect false
                                                                 :init-data true})
                                   (system-msg state side (str "uses Mutate to install and rez " (:title newice) " from R&D at no cost"))
                                   (trigger-event state side :rez newice))
                                 (system-msg state side (str "does not find any ICE to install from R&D")))
                               (shuffle! state :corp :deck)
                               (effect-completed state side eid)))))}})
