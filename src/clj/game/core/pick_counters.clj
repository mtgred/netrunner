(ns game.core.pick-counters
  (:require
    [game.core.card :refer [get-card get-counters installed?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.eid :refer [effect-completed make-eid complete-with-result]]
    [game.core.engine :refer [resolve-ability trigger-event-sync]]
    [game.core.gaining :refer [lose]]
    [game.core.props :refer [add-counter]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [in-coll? quantify same-card?]]
    [clojure.string :as string]))

(defn- pick-counter-triggers
  [state side eid current-cards selected-cards counter-count message]
  (if-let [[_ selected] (first current-cards)]
    (if-let [{:keys [card number]} selected]
      (wait-for (trigger-event-sync state side :counter-added (get-card state card) number)
                (pick-counter-triggers state side eid (rest current-cards) selected-cards counter-count message))
      (pick-counter-triggers state side eid (rest current-cards) selected-cards counter-count message))
    (complete-with-result state side eid {:number counter-count
                                          :msg message
                                          :targets (keep #(:card (second %)) selected-cards)})))

(defn pick-virus-counters-to-spend
  "Pick virus counters to spend. For use with Freedom Khumalo and virus breakers, and any other relevant cards.
  This function returns a map for use with resolve-ability or continue-ability.
  The ability triggered returns either {:number n :msg msg} on completed effect, or :cancel on a cancel.
  n is the number of virus counters selected, msg is the msg string of all the cards and the virus counters taken from each.
  If called with no arguments, allows user to select as many counters as they like until 'Cancel' is pressed."
  ([target-count] (pick-virus-counters-to-spend nil target-count (hash-map) 0))
  ([specific-card target-count] (pick-virus-counters-to-spend specific-card target-count (hash-map) 0))
  ([specific-card target-count selected-cards counter-count]
   {:async true
    :prompt (str "Select a card with virus counters ("
                 counter-count (str " of " target-count)
                 " virus counters)")
    :choices {:card #(and (if specific-card
                            (or (same-card? % specific-card)
                                (= "Hivemind" (:title %)))
                            true)
                          (installed? %)
                          (pos? (get-counters % :virus)))}
    :effect (req (let [target (update! state :runner (update-in target [:counter :virus] dec))
                       selected-cards (update selected-cards (:cid target)
                                              ;; Store card reference and number of counters picked
                                              ;; Overwrite card reference each time
                                              #(assoc % :card target :number (inc (:number % 0))))
                       counter-count (inc counter-count)]
                   (if (or (not target-count)
                           (< counter-count target-count))
                     (continue-ability state side
                                       (pick-virus-counters-to-spend specific-card target-count selected-cards counter-count)
                                       card nil)
                     (let [message (string/join ", " (map #(let [{:keys [card number]} %
                                                          title (:title card)]
                                                      (str (quantify number "virus counter") " from " title))
                                                   (vals selected-cards)))]
                       (pick-counter-triggers state side eid selected-cards selected-cards counter-count message)))))
    :cancel-effect (if target-count
                     (req (doseq [{:keys [card number]} (vals selected-cards)]
                            (update! state :runner (update-in (get-card state card) [:counter :virus] + number)))
                          (complete-with-result state side eid :cancel))
                     (req (let [message (string/join ", " (map #(let [{:keys [card number]} %
                                                               title (:title card)]
                                                           (str (quantify number "virus counter") " from " title))
                                                        (vals selected-cards)))]
                           (complete-with-result state side eid {:number counter-count :msg message}))))}))

(defn- trigger-spend-credits-from-cards
  [state side eid cards]
  (if (seq cards)
    (wait-for (trigger-event-sync state side :spent-credits-from-card (first cards))
              (trigger-spend-credits-from-cards state side eid (rest cards)))
    (effect-completed state side eid)))

(defn pick-credit-providing-cards
  "Similar to pick-virus-counters-to-spend. Works on :recurring and normal credits."
  ([provider-func outereid] (pick-credit-providing-cards provider-func outereid nil (hash-map) 0))
  ([provider-func outereid target-count] (pick-credit-providing-cards provider-func outereid target-count (hash-map) 0))
  ([provider-func outereid target-count selected-cards counter-count]
   (let [pay-rest
         (req (if (<= (- target-count counter-count) (get-in @state [side :credit]))
                (let [remainder (- target-count counter-count)
                      remainder-str (when (pos? remainder)
                                      (str remainder " [Credits]"))
                      card-strs (when (pos? (count selected-cards))
                                  (str (string/join ", " (map #(let [{:keys [card number]} %
                                                              title (:title card)]
                                                          (str number " [Credits] from " title))
                                                       (vals selected-cards)))))
                      message (str card-strs
                                   (when (and card-strs remainder-str)
                                     " and ")
                                   remainder-str
                                   (when (and card-strs remainder-str)
                                     " from their credit pool"))]
                  (lose state side :credit remainder)
                  (let [cards (map :card (vals selected-cards))]
                    (wait-for (trigger-spend-credits-from-cards state side cards)
                              ; Now we trigger all of the :counter-added events we'd neglected previously
                              (pick-counter-triggers state side eid selected-cards selected-cards target-count message))))
                (continue-ability
                  state side
                  (pick-credit-providing-cards provider-func eid target-count selected-cards counter-count)
                  card nil)))
         provider-cards (provider-func)]
     (if (or (not (pos? target-count))        ; there is a limit
             (>= counter-count target-count)  ; paid everything
             (zero? (count provider-cards)))  ; no more additional credit sources found
       {:async true
        :effect pay-rest}
       {:async true
        :prompt (str "Select a credit providing card ("
                     counter-count (when (and target-count (pos? target-count))
                                     (str " of " target-count))
                     " credits)")
        :choices {:card #(in-coll? (map :cid provider-cards) (:cid %))}
        :effect (req (let [pay-credits-type (-> target card-def :interactions :pay-credits :type)
                           pay-credits-custom (when (= :custom pay-credits-type)
                                                (-> target card-def :interactions :pay-credits :custom))
                           custom-ability (when (= :custom pay-credits-type)
                                            {:async true :effect pay-credits-custom})
                           current-counters (get-counters target pay-credits-type)
                           ; In this next bit, we don't want to trigger any events yet
                           ; so we use `update!` to directly change the number of credits
                           gained-credits (case pay-credits-type
                                            (:recurring :credit)
                                            (do (update! state side (assoc-in target [:counter pay-credits-type] (dec current-counters)))
                                                1)
                                            ; Custom credits will be handled separately later
                                            0)
                           target (get-card state target)
                           selected-cards (update selected-cards (:cid target)
                                                  ;; Store card reference and number of counters picked
                                                  ;; Overwrite card reference each time
                                                  #(assoc % :card target :number (inc (:number % 0))))
                           counter-count (+ counter-count gained-credits)]
                       (if (= :custom pay-credits-type)
                         ; custom functions should be a 5-arg fn that returns an ability that provides the number of credits as async-result
                         (let [neweid (make-eid state outereid)
                               providing-card target]
                           (wait-for (resolve-ability state side neweid custom-ability providing-card [card])
                                     (continue-ability state side
                                                       (pick-credit-providing-cards
                                                         provider-func eid target-count
                                                         (update selected-cards (:cid providing-card)
                                                                 ;; correct credit count
                                                                 #(assoc % :card target :number (+ (:number % 0) (dec async-result))))
                                                         (min (+ counter-count async-result) target-count))
                                                       card targets)))
                         (continue-ability state side
                                           (pick-credit-providing-cards provider-func eid target-count selected-cards counter-count)
                                           card nil))))
        :cancel-effect pay-rest}))))
