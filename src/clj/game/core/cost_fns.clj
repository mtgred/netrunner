(ns game.core.cost-fns
  (:require
    [game.core.card :refer [runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [any-effects get-effects sum-effects]]
    [game.core.eid :refer [make-eid]]
    [game.core.payment :refer [merge-costs cost-name value]]))

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
    (concat (:additional-cost card)
            (get-in (card-def card) [:on-play :additional-cost])
            (get-effects state side :play-additional-cost card))))

(defn rez-cost
  "Combines all rez effects and costs into a single number, not a cost vector"
  ([state side card] (rez-cost state side card nil))
  ([state side {:keys [cost] :as card} {:keys [cost-bonus]}]
   (when-not (nil? cost)
     (->> [cost
           (or cost-bonus 0)
           (when-let [rezfun (:rez-cost-bonus (card-def card))]
             (rezfun state side (make-eid state) card nil))
           (sum-effects state side :rez-cost card)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn rez-additional-cost-bonus
  ([state side card] (rez-additional-cost-bonus state side card nil))
  ([state side card pred]
   (let [costs (merge-costs
                 (concat (:additional-cost card)
                         (:additional-cost (card-def card))
                         (get-effects state side :rez-additional-cost card)))]
     (if pred (filterv pred costs) costs))))

(defn score-additional-cost-bonus
  [state side card]
  (merge-costs
   (concat (:additional-cost card)
           (:additional-cost (card-def card))
           (get-effects state side :score-additional-cost card))))

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
    (concat (:additional-cost card)
            (:additional-cost (card-def card))
            (get-effects state side :install-additional-cost card))))

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
        (some #(= :trash-can (first %))
              (->> abilities
                   (map :cost)
                   (map merge-costs)
                   (apply concat))))))

(defn card-ability-cost
  "Returns a list of all costs (printed and additional) required to use a given ability"
  ([state side ability card] (card-ability-cost state side ability card nil))
  ([state side ability card targets]
   (merge-costs
     (concat (:cost ability)
             (:additional-cost ability)
             (get-effects state side :card-ability-additional-cost card (cons ability targets))))))

(defn break-sub-ability-cost
  ([state side ability card] (break-sub-ability-cost state side ability card nil))
  ([state side ability card targets]
   (merge-costs
     (concat (:break-cost ability)
             (:additional-cost ability)
             (when-let [break-fn (:break-cost-bonus ability)]
               (break-fn state side (make-eid state) card targets))
             (get-effects state side :break-sub-additional-cost card (cons ability targets))))))

(defn jack-out-cost
  [state side]
  (get-effects state side :jack-out-additional-cost))

(defn all-stealth
  "To be used as the :cost-req of an ability. Requires all credits spent to be stealth credits."
  [costs]
  (mapv #(condp = (cost-name %) :x-credits [:x-credits nil -1] :credit [:credit (value %) (value %)] %) costs))

(defn min-stealth
  "Returns a function to be used as the :cost-req of an ability. Requires a minimum number of credits spent to be stealth"
  [stealth-requirement]
  (fn [costs]
    (if (some #(= (cost-name %) :credit) costs)
      (map #(if (= (cost-name %) :credit) [:credit (value %) stealth-requirement] %) costs)
      (map #(if (= (cost-name %) :x-credits) [:x-credits nil stealth-requirement] %) costs))))

(defn steal-cost
  "Gets a vector of costs and their sources for stealing the given agenda."
  [state side eid card]
  (-> (when-let [costfun (:steal-cost-bonus (card-def card))]
        [[(costfun state side eid card nil) {:source card :source-type :ability}]])
      (concat (get-effects state side :steal-additional-cost card))
      vec))
