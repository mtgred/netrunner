(ns game.core.cost-fns
  (:require
    [game.core.card :refer [runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [any-effects get-effects sum-effects get-effect-maps get-effect-value is-disabled-reg?]]
    [game.core.eid :refer [make-eid]]
    [game.core.payment :refer [merge-costs]]))

;; State-aware cost-generating functions
(defn play-cost
  "Combines all relevant effects and costs to play a given card"
  ([state side card] (play-cost state side card nil))
  ([state side {:keys [cost] :as card} {:keys [cost-bonus]}]
   (when-not (nil? cost)
     (->> [cost
           (or cost-bonus 0)
           (when-let [playfun (get-in (card-def card) [:on-play :play-cost-bonus])]
             (playfun state side (make-eid state) card nil))
           (sum-effects state side :play-cost card)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn play-additional-cost-bonus
  [state side card]
  (merge-costs
    (concat (get-in (card-def card) [:on-play :additional-cost])
            (get-effects state side :play-additional-cost card))))

(defn rez-cost
  "Combines all rez effects and costs into a single number, not a cost vector"
  ([state side card] (rez-cost state side card nil))
  ([state side {:keys [cost] :as card} {:keys [cost-bonus]}]
   (when-not (nil? cost)
     (->> [cost
           (or cost-bonus 0)
           (when-let [rezfun (and (not (is-disabled-reg? state card))
                                  (:rez-cost-bonus (card-def card)))]
             (rezfun state side (make-eid state) card nil))
           (sum-effects state side :rez-cost card)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn rez-additional-cost-bonus
  ([state side card] (rez-additional-cost-bonus state side card nil))
  ([state side card pred]
   (let [costs (merge-costs
                 [(when-not (is-disabled-reg? state card) (:additional-cost (card-def card)))
                  (get-effects state side :rez-additional-cost card)])]
     (filterv (or pred identity) costs))))

(defn score-additional-cost-bonus
  [state side card]
  (merge-costs
    [(:additional-cost (card-def card))
     (get-effects state side :score-additional-cost card)]))

(defn trash-cost
  "Returns the number of credits required to trash the given card."
  ([state side card] (trash-cost state side card nil))
  ([state side {:keys [trash] :as card} {:keys [cost-bonus]}]
   (when-not (nil? trash)
     (->> [trash
           (or cost-bonus 0)
           (when-let [trashfun (:trash-cost-bonus (card-def card))]
             (trashfun state side (make-eid state) card nil))
           (sum-effects state side :trash-cost card)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn install-cost
  "Returns the number of credits required to install the given card."
  ([state side card] (install-cost state side card nil nil))
  ([state side card args] (install-cost state side card args nil))
  ([state side card {:keys [cost-bonus]} & targets]
   (->> [(when (runner? card)
           (:cost card))
         (or cost-bonus 0)
         (when-let [instfun (:install-cost-bonus (card-def card))]
           (instfun state side (make-eid state) card nil))
         (sum-effects state side :install-cost card targets)]
        (reduce (fnil + 0 0))
        (max 0))))

(defn install-additional-cost-bonus
  [state side card]
  (merge-costs
    [(:additional-cost (card-def card))
     (get-effects state side :install-additional-cost card)]))

(defn ignore-install-cost?
  [state side card]
  (any-effects state side :ignore-install-cost true? card))

(defn run-cost
  "Get a list of all costs required to run a server."
  ([state side card] (run-cost state side card nil nil))
  ([state side card args] (run-cost state side card args nil))
  ([state side card {:keys [cost-bonus]} & targets]
   (->> [(or cost-bonus 0)
         (sum-effects state side :run-cost card targets)]
        (reduce (fnil + 0 0))
        (max 0))))

(defn run-additional-cost-bonus
  ([state side card] (run-additional-cost-bonus state side card nil))
  ([state side card & targets]
   (merge-costs
     (get-effects state side :run-additional-cost card targets))))

(defn has-trash-ability?
  [card]
  (let [abilities (:abilities (card-def card))
        events (:events (card-def card))]
    (or (some :trash-icon (concat abilities events))
        (some #(= :trash-can (:cost/type %))
              (->> abilities
                   (map :cost)
                   (vec)
                   (merge-costs))))))

(defn card-ability-cost
  "Returns a list of all costs (printed and additional) required to use a given ability"
  ([state side ability card] (card-ability-cost state side ability card nil))
  ([state side ability card targets]
   (let [base-cost [(:cost ability)
                    (get-effects state side :card-ability-cost
                                 {:card card
                                  :ability ability
                                  :targets targets})]
         additional-cost (->> [(:additional-cost ability)
                               (get-effects state side :card-ability-additional-cost
                                            {:card card
                                             :ability ability
                                             :targets targets})]
                              (flatten)
                              ; TODO: uncomment when implementing additional costs
                              #_(keep #(when % (assoc % :cost/additional true))))]
     (merge-costs (into base-cost additional-cost)))))

(defn break-sub-ability-cost
  ([state side ability card] (break-sub-ability-cost state side ability card nil))
  ([state side ability card targets]
   (merge-costs
     [(:break-cost ability)
      (:additional-cost ability)
      (when-let [break-fn (:break-cost-bonus ability)]
        (break-fn state side (make-eid state) card targets))
      (get-effects state side :break-sub-additional-cost {:card card
                                                          :ability ability
                                                          :targets targets})])))

(defn jack-out-cost
  [state side]
  (get-effects state side :jack-out-additional-cost))

(defn steal-cost
  "Gets a vector of costs and their sources for stealing the given agenda."
  [state side eid card]
  (let [steal-cost (when-let [costfun (:steal-cost-bonus (card-def card))]
                     (costfun state side eid card nil))
        steal-cost (when steal-cost
                     (if (map? steal-cost)
                       (assoc-in steal-cost [:cost/args :source] card)
                       (mapv #(assoc-in % [:cost/args :source] card) steal-cost)))
        ev (get-effect-value state side eid [card])]
    (->> (get-effect-maps state side eid :steal-additional-cost [card])
         (reduce
           (fn [acc {ab-card :card :as ab}]
             (let [cost (ev ab)
                   cost (if (map? cost)
                          (assoc-in cost [:cost/args :source] ab-card)
                          (mapv #(assoc-in % [:cost/args :source] ab-card) cost))]
               (conj acc cost)))
           [])
         (concat [steal-cost])
         (flatten)
         (filter some?)
         (mapv #(-> %
                    (assoc :cost/additional true)
                    (assoc-in [:cost/args :source-type] :ability))))))
