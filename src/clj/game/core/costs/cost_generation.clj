(in-ns 'game.core)

;; State-aware cost-generating functions
(defn play-cost
  "Combines all relevant effects and costs to play a given card"
  ([state side card] (play-cost state side card nil))
  ([state side {:keys [cost] :as card} {:keys [cost-bonus]}]
   (when-not (nil? cost)
     (->> [cost
           (or cost-bonus 0)
           (when-let [playfun (:play-cost-bonus (card-def card))]
             (playfun state side (make-eid state) card nil))
           (sum-effects state side card :play-cost)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn play-additional-cost-bonus
  [state side card]
  (merge-costs
    (concat (:additional-cost card)
            (:additional-cost (card-def card))
            (get-effects state side card :play-additional-cost))))

(defn rez-cost
  "Combines all rez effects and costs into a single number, not a cost vector"
  ([state side card] (rez-cost state side card nil))
  ([state side {:keys [cost] :as card} {:keys [cost-bonus]}]
   (when-not (nil? cost)
     (->> [cost
           (or cost-bonus 0)
           (when-let [rezfun (:rez-cost-bonus (card-def card))]
             (rezfun state side (make-eid state) card nil))
           (sum-effects state side card :rez-cost)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn rez-additional-cost-bonus
  [state side card]
  (merge-costs
    (concat (:additional-cost card)
            (:additional-cost (card-def card))
            (get-effects state side card :rez-additional-cost))))

(defn trash-cost
  "Returns the number of credits required to trash the given card."
  ([state side card] (trash-cost state side card nil))
  ([state side {:keys [trash] :as card} {:keys [cost-bonus]}]
   (when-not (nil? trash)
     (->> [trash
           (or cost-bonus 0)
           (when-let [trashfun (:trash-cost-bonus (card-def card))]
             (trashfun state side (make-eid state) card nil))
           (sum-effects state side card :trash-cost)]
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
         (sum-effects state side card :install-cost targets)]
        (reduce (fnil + 0 0))
        (max 0))))

(defn install-additional-cost-bonus
  [state side card]
  (merge-costs
    (concat (:additional-cost card)
            (:additional-cost (card-def card))
            (get-effects state side card :install-additional-cost))))

(defn ignore-install-cost?
  [state side card]
  (any-effects state side :ignore-install-cost true? card))

(defn run-cost
  "Get a list of all costs required to run a server."
  ([state side card] (run-cost state side card nil nil))
  ([state side card args] (run-cost state side card args nil))
  ([state side card {:keys [cost-bonus]} & targets]
   (->> [(or cost-bonus 0)
         (sum-effects state side card :run-cost targets)]
        (reduce (fnil + 0 0))
        (max 0))))

(defn run-additional-cost-bonus
  ([state side card] (run-additional-cost-bonus state side card nil))
  ([state side card & targets]
   (merge-costs
     (get-effects state side card :run-additional-cost targets))))

(defn has-trash-ability?
  [card]
  (let [abilities (:abilities (card-def card))
        events (:events (card-def card))]
    (or (some :trash-icon (concat abilities events))
        (some #(= :trash (first %))
              (->> abilities
                   (map :cost)
                   (map merge-costs)
                   (apply concat))))))

(defn card-ability-cost
  "Returns a list of all costs (printed and additional) required to use a given ability"
  ([state side ability card] (card-ability-cost state side ability card nil nil))
  ([state side ability card targets] (card-ability-cost state side ability card targets nil))
  ([state side ability card targets {:keys [cost-bonus] :as args}]
   (concat (:cost ability)
           (:additional-cost ability)
           (get-effects state side card :card-ability-additional-cost (flatten [ability targets])))))

(defn break-sub-ability-cost
  ([state side ability card] (break-sub-ability-cost state side ability card nil nil))
  ([state side ability card targets] (break-sub-ability-cost state side ability card targets nil))
  ([state side ability card targets {:keys [cost-bonus] :as args}]
   (concat (:cost ability)
           (:additional-cost ability)
           (get-effects state side card :break-sub-additional-cost (flatten [ability targets])))))

(defn jack-out-cost
  ([state side] (jack-out-cost state side nil))
  ([state side args]
   (get-effects state side nil :jack-out-additional-cost args)))
