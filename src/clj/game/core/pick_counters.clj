(ns game.core.pick-counters
  (:require
    [game.core.card :refer [get-card get-counters has-subtype? installed? runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.eid :refer [effect-completed make-eid complete-with-result]]
    [game.core.engine :refer [resolve-ability queue-event]]
    [game.core.gaining :refer [lose]]
    [game.core.props :refer [add-counter]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [enumerate-str in-coll? quantify same-card?]]))

(defn- pick-counter-triggers
  [state side eid current-cards selected-cards counter-type counter-count message]
  (if-let [[_ selected] (first current-cards)]
    (if-let [{:keys [card number]} selected]
      (do (queue-event state :counter-added {:card (get-card state card) :counter-type counter-type :amount number})
          (pick-counter-triggers state side eid (rest current-cards) selected-cards counter-type counter-count message))
      (pick-counter-triggers state side eid (rest current-cards) selected-cards counter-type counter-count message))
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
    :prompt (str "Choose a card with virus counters ("
                 counter-count (str " of " target-count)
                 " virus counters)")
    :choices {:card #(and (if specific-card
                            (or (same-card? % specific-card)
                                (= "Hivemind" (:title %)))
                            true)
                          (installed? %)
                          (runner? %)
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
                     (let [message (enumerate-str (map #(let [{:keys [card number]} %
                                                          title (:title card)]
                                                      (str (quantify number "virus counter") " from " title))
                                                   (vals selected-cards)))]
                       (pick-counter-triggers state side eid selected-cards selected-cards :virus counter-count message)))))
    :cancel {:async true
             :effect (if target-count
                       (req (doseq [{:keys [card number]} (vals selected-cards)]
                              (update! state :runner (update-in (get-card state card) [:counter :virus] + number)))
                            (complete-with-result state side eid :cancel))
                       (req (let [message (enumerate-str (map #(let [{:keys [card number]} %
                                                                     title (:title card)]
                                                                 (str (quantify number "virus counter") " from " title))
                                                              (vals selected-cards)))]
                              (complete-with-result state side eid {:number counter-count :msg message}))))}}))

(defn- trigger-spend-credits-from-cards
  [state side eid cards]
  (if (seq cards)
    (do (queue-event state :spent-credits-from-card {:card (first cards)})
        (trigger-spend-credits-from-cards state side eid (rest cards)))
    (effect-completed state side eid)))

(defn- take-counters-of-type
  "This builds an effect to remove a single counter of the given type, including credits. This does not fire any events."
  [counter-type]
  (req (update! state side (assoc-in card [:counter counter-type] (dec (get-counters card counter-type))))
       (complete-with-result state side eid 1)))

(defn- use-card [uses card async-res]
  (if (and async-res (pos? async-res))
    (if (get uses (:cid card))
      (update-in uses [(:cid card) :used] inc)
      (assoc uses (:cid card) {:used 1 :max-uses (or (-> card card-def :interactions :pay-credits :max-uses) 99)}))
    uses))

(defn pick-credit-reducers
  "Similar to pick-credit-providing-cards, but this happens first and is (currently) only used for patchwork"
  ([provider-func outereid] (pick-credit-reducers provider-func outereid nil 0 (hash-map)))
  ([provider-func outereid target-count] (pick-credit-reducers provider-func outereid target-count 0 (hash-map)))
  ([provider-func outereid target-count stealth-target] (pick-credit-reducers provider-func outereid target-count stealth-target (hash-map)))
  ([provider-func outereid target-count stealth-target selected-cards] (pick-credit-reducers provider-func outereid target-count stealth-target selected-cards {}))
  ([provider-func outereid target-count stealth-target selected-cards uses]
   (let [counter-count (reduce + 0 (map #(:number (second %) 0) selected-cards))
         provider-cards (provider-func)
         ;; dont select the same card more than once (for the custom ones with wierd rules - take it up with nsg)
         all-used-up? (fn [cid] (>= (get-in uses [cid :used] 0) (get-in uses [cid :max-uses] 1)))
         provider-cards (filter #(not (all-used-up? (:cid %))) provider-cards)
         discount-provider (filter #(get-in (card-def %) [:interactions :pay-credits :cost-reduction]) provider-cards)]
     (if (empty? discount-provider)
       {:async true
        :effect (req (complete-with-result state side eid {:reduction counter-count
                                                           :targets (keep #(:card (second %)) selected-cards)}))}
       {:async true
        :prompt (str "Choose a cost-reducing card")
        :choices {:card #(in-coll? (map :cid discount-provider) (:cid %))}
        :effect (req (let [pay-credits-type (-> target card-def :interactions :pay-credits :type)
                           pay-function (if (= :custom pay-credits-type)
                                          (-> target card-def :interactions :pay-credits :custom)
                                          (take-counters-of-type pay-credits-type))
                           custom-ability ^:ignore-async-check  {:async true
                                                                 :effect pay-function}
                           neweid (make-eid state outereid)
                           providing-card target]
                       (wait-for (resolve-ability state side neweid custom-ability providing-card [card])
                                 (continue-ability state side
                                                   (pick-credit-reducers
                                                     provider-func eid target-count stealth-target
                                                     (update selected-cards (:cid providing-card)
                                                             #(assoc % :card providing-card :number (+ (:number % 0) async-result)))
                                                     (use-card uses providing-card async-result))
                                                   card targets))))
        :cancel{:async true
                :effect (req (complete-with-result state side eid {:reduction counter-count
                                                                   :targets (keep #(:card (second %)) selected-cards)}))}}))))

(defn pick-credit-providing-cards
  "Similar to pick-virus-counters-to-spend. Works on :recurring and normal credits."
  ([provider-func outereid] (pick-credit-providing-cards provider-func outereid nil 0 (hash-map)))
  ([provider-func outereid target-count] (pick-credit-providing-cards provider-func outereid target-count 0 (hash-map)))
  ([provider-func outereid target-count stealth-target] (pick-credit-providing-cards provider-func outereid target-count stealth-target (hash-map)))
  ([provider-func outereid target-count stealth-target selected-cards] (pick-credit-providing-cards provider-func outereid target-count stealth-target selected-cards nil))
  ([provider-func outereid target-count stealth-target selected-cards pre-chosen] (pick-credit-providing-cards provider-func outereid target-count stealth-target selected-cards pre-chosen {}))
  ([provider-func outereid target-count stealth-target selected-cards pre-chosen uses]
   (let [counter-count (reduce + 0 (map #(:number (second %) 0) selected-cards))
         selected-stealth (filter #(has-subtype? (:card (second %)) "Stealth") selected-cards)
         stealth-count (reduce + 0 (map #(:number (second %) 0) selected-stealth))
         provider-cards (if (= (- counter-count target-count) (- stealth-count stealth-target))
                          (filter #(has-subtype? % "Stealth") (provider-func))
                          (provider-func))
         all-used-up? (fn [cid] (>= (get-in uses [cid :used] 0) (get-in uses [cid :max-uses] 99)))
         provider-cards (filter #(not (all-used-up? (:cid %))) provider-cards)
         provider-cards (filter #(not (get-in (card-def %) [:interactions :pay-credits :cost-reduction])) provider-cards)
         ;; note - this allows holding the shift key while clicking a card to keep picking that card while possible
         ;; ie: taking 5cr from miss bones with one click, instead of waiting for 5 server round-trips
         should-auto-repeat? (fn [state side] (get-in @state [side :shift-key-select] nil))
         pay-rest (req
                    (if (and (<= (- target-count counter-count) (get-in @state [side :credit]))
                             (<= stealth-target stealth-count))
                      (let [remainder (max 0 (- target-count counter-count))
                            remainder-str (when (pos? remainder)
                                            (str remainder " [Credits]"))
                            card-strs (when (pos? (count selected-cards))
                                        (str (enumerate-str (map #(let [{:keys [card number]} %
                                                                        title (:title card)]
                                                                    (str number " [Credits] from " title))
                                                                 (vals selected-cards)))))
                            message (str card-strs
                                         (when (and card-strs remainder-str)
                                           " and ")
                                         remainder-str
                                         (when (and card-strs remainder-str)
                                           " from [their] credit pool"))]
                        (lose state side :credit remainder)
                        (let [cards (->> (vals selected-cards)
                                         (map :card)
                                         (remove #(-> (card-def %) :interactions :pay-credits :cost-reduction)))]
                          (wait-for (trigger-spend-credits-from-cards state side cards)
                                        ; Now we trigger all of the :counter-added events we'd neglected previously
                                    (pick-counter-triggers state side eid selected-cards selected-cards :credit target-count message))))
                      (continue-ability
                        state side
                        (pick-credit-providing-cards provider-func eid target-count stealth-target selected-cards uses)
                        card nil)))]
     (if (or (not (pos? target-count))        ; there is a limit
             (<= target-count counter-count)  ; paid everything
             (zero? (count provider-cards)))  ; no more additional credit sources found
       {:async true
        :effect pay-rest}
       (if (and pre-chosen (in-coll? (map :cid provider-cards) (:cid pre-chosen)))
         {:async true
          :effect (req (let [target pre-chosen
                             pay-credits-type (-> target card-def :interactions :pay-credits :type)
                             pay-function (if (= :custom pay-credits-type)
                                            (-> target card-def :interactions :pay-credits :custom)
                                            (take-counters-of-type pay-credits-type))
                             custom-ability ^:ignore-async-check {:async true
                                                                  :effect pay-function}
                             neweid (make-eid state outereid)
                             providing-card target]
                         (wait-for (resolve-ability state side neweid custom-ability providing-card [card])
                                   (continue-ability state side
                                                     (pick-credit-providing-cards
                                                       provider-func eid target-count stealth-target
                                                       (update selected-cards (:cid providing-card)
                                                               #(assoc % :card providing-card :number (+ (:number % 0) async-result)))
                                                       target)
                                                     card targets))))}
         {:async true
          :prompt (str "Choose a credit providing card ("
                       counter-count (when (and target-count (pos? target-count))
                                       (str " of " target-count))
                       " [Credits]"
                       (if (pos? stealth-target)
                         (str ", " (min stealth-count stealth-target) " of " stealth-target " stealth")
                         "")
                       ")")
          :choices {:card #(in-coll? (map :cid provider-cards) (:cid %))}
          :effect (req (let [pay-credits-type (-> target card-def :interactions :pay-credits :type)
                             pay-function (if (= :custom pay-credits-type)
                                            (-> target card-def :interactions :pay-credits :custom)
                                            (take-counters-of-type pay-credits-type))
                             custom-ability ^:ignore-async-check {:async true
                                                                  :effect pay-function}
                             neweid (make-eid state outereid)
                             providing-card target]
                         (wait-for (resolve-ability state side neweid custom-ability providing-card [card])
                                   (continue-ability state side
                                                     (pick-credit-providing-cards
                                                       provider-func eid target-count stealth-target
                                                       (update selected-cards (:cid providing-card)
                                                               #(assoc % :card providing-card :number (+ (:number % 0) async-result)))
                                                       (when (should-auto-repeat? state side) target)
                                                       (use-card uses providing-card async-result))
                                                     card targets))))
          :cancel {:async true
                   :effect pay-rest}})))))
