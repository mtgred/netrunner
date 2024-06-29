(ns game.core.initializing
  (:require
    [game.core.board :refer [all-active all-active-installed]]
    [game.core.card :refer [get-card map->Card program? runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [break-sub-ability-cost card-ability-cost]]
    [game.core.effects :refer [register-static-abilities unregister-static-abilities]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [is-ability? register-default-events register-events resolve-ability unregister-events]]
    [game.core.finding :refer [find-cid]]
    [game.core.gaining :refer [gain lose]]
    [game.core.ice :refer [add-sub]]
    [game.core.memory :refer [init-mu-cost]]
    [game.core.payment :refer [add-cost-label-to-ability]]
    [game.core.props :refer [add-counter]]
    [game.core.update :refer [update!]]
    [game.macros :refer [req]]
    [game.utils :refer [make-cid server-card to-keyword]]
    [jinteki.utils :refer [make-label]]))

(defn subroutines-init
  "Initialised the subroutines associated with the card, these work as abilities"
  [card cdef]
  (let [no-subs-card (dissoc card :subroutines)]
    (->> (:subroutines cdef)
         (reduce (fn [ice sub] (add-sub ice sub (:cid ice) {:printed true})) no-subs-card)
         :subroutines
         (into []))))

(defn ability-init
  "Gets abilities associated with the card"
  [cdef]
  (into [] (for [ab (:abilities cdef)
                 :let [ab (assoc ab :label (make-label ab))]]
             (add-cost-label-to-ability ab))))

(defn- dissoc-card
  "Dissoc relevant keys in card"
  [card keep-counter]
  (let [cdef (card-def card)
        c (dissoc card
                  :current-strength :current-advancement-requirement :current-points
                  :runner-abilities :corp-abilities :rezzed :new
                  :subtype-target :card-target :extra-advance-counter :special)
        c (assoc c :subroutines (subroutines-init c cdef) :abilities (ability-init cdef))
        c (if keep-counter c (dissoc c :counter :advance-counter))]
    (map->Card c)))

(defn- trigger-leave-effect
  "Triggers leave effects for specified card if relevant"
  [state side {:keys [disabled installed rezzed facedown zone host] :as card}]
  (when-let [leave-effect (:leave-play (card-def card))]
    (when (and (not disabled)
               (not (and (runner? card) host (not installed) (not facedown)))
               (or (and (runner? card) installed (not facedown))
                   rezzed
                   (and host (not facedown))
                   (= (first zone) :current)
                   (= (first zone) :scored)))
      (leave-effect state side (make-eid state) card nil))))

(defn deactivate
  "Deactivates a card, unregistering its events, removing certain attribute keys, and triggering
  some events."
  ([state side card] (deactivate state side card nil))
  ([state side {:keys [cid disabled installed rezzed] :as card} keep-counter]
   (unregister-events state side card)
   (unregister-static-abilities state side card)
   (trigger-leave-effect state side card)
   (when (and (find-cid cid (all-active-installed state side))
              (not disabled)
              (or rezzed
                  installed))
     (when-let [in-play (:in-play (card-def card))]
       (apply lose state side in-play)))
   (dissoc-card card keep-counter)))


;;; Initialising a card
(defn corp-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (into [] (for [ab (:corp-abilities cdef)
                 :let [ab (assoc (select-keys ab [:cost]) :label (make-label ab))]]
             (add-cost-label-to-ability ab))))

(defn runner-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (into [] (for [ab (:runner-abilities cdef)
                 :let [ab (assoc (select-keys ab [:cost :break-cost]) :label (make-label ab))]]
             (add-cost-label-to-ability ab (or (:break-cost ab) (:cost ab))))))

(defn card-init
  "Initializes the abilities and events of the given card."
  ([state side card] (card-init state side card {:resolve-effect true :init-data true}))
  ([state side card args] (card-init state side (make-eid state) card args))
  ([state side eid card {:keys [resolve-effect init-data no-mu]}]
   (let [cdef (card-def card)
         recurring (:recurring cdef)
         run-abs (runner-ability-init cdef)
         corp-abs (corp-ability-init cdef)
         c (update! state side
                    (merge card {:runner-abilities run-abs
                                 :corp-abilities corp-abs}))
         data (merge
                (when init-data (:counter (:data cdef)))
                (when recurring
                  {:recurring
                   (cond
                     (fn? recurring) (recurring state side eid c nil)
                     (number? recurring) recurring
                     :else (throw (Exception. (str (:title card) " - Recurring isn't number or fn"))))}))
         _ (when recurring (update! state side (assoc-in c [:counter :recurring] 0)))
         _ (doseq [[c-type c-num] data]
             (add-counter state side (get-card state c) c-type c-num {:placed true}))
         c (get-card state c)]
     (when recurring
       (let [recurring-fn (req (if (number? recurring) recurring (recurring state side eid card targets)))
             r (req (let [card (update! state side (assoc-in card [:counter :recurring] 0))]
                      (add-counter state side card
                                   :recurring (recurring-fn state side eid card targets)
                                   {:placed true})))]
         (register-events
           state side c
           [{:event (if (= side :corp) :corp-phase-12 :runner-phase-12)
             :req (req (not (:disabled card)))
             :effect r}])))
     (register-default-events state side c)
     (register-static-abilities state side c)
     ;; Facedown cards can't be initialized
     (when (and (program? card)
                (not no-mu))
       (init-mu-cost state c))
     (if (and resolve-effect (is-ability? cdef))
       (resolve-ability state side (assoc eid :source-type :ability) (dissoc cdef :cost :additional-cost) c nil)
       (effect-completed state side eid))
     (when-let [in-play (:in-play cdef)]
       (apply gain state side in-play))
     (get-card state c))))

(defn update-ability-cost-str
  [state side card ability-kw]
  (into [] (for [ab (get card ability-kw)
                 :let [ab-cost (if (:break-cost ab)
                                 (assoc ab :cost (break-sub-ability-cost state side ab card))
                                 ab)]]
             (add-cost-label-to-ability ab (card-ability-cost state side ab-cost card)))))

(defn update-abilities-cost-str
  [state side card]
  (-> card
      (assoc :abilities (update-ability-cost-str state side card :abilities))
      (assoc :corp-abilities (update-ability-cost-str state side card :corp-abilities))
      (assoc :runner-abilities (update-ability-cost-str state side card :runner-abilities))))

(defn update-all-card-labels
  [state]
  (reduce (fn [changed? card]
            (let [side (to-keyword (:side card))
                  new-card (update-abilities-cost-str state side card)]
              (when (not= card new-card)
                (update! state side new-card))
              (or (not= card new-card) changed?)))
          false
          (concat (all-active state :corp) (all-active state :runner))))

(defn card-implemented
  "Checks if the card is implemented. Looks for a valid return from `card-def`.
  If implemented also looks for `:implementation` key which may contain special notes.
  Returns either:
    nil - not implemented
    :full - implemented fully
    msg - string with implementation notes"
  [card]
  (when-let [cdef (card-def card)]
    ;; Card is defined - hence implemented
    (if-let [impl (:implementation cdef)]
      (if (:recurring cdef) (str impl ". Recurring credits usage not restricted") impl)
      (if (:recurring cdef) "Recurring credits usage not restricted" :full))))

(defn make-card
  "Makes or remakes (with current cid) a proper card from a server card"
  ([card] (make-card card (make-cid)))
  ([card cid]
   (let [cdef (card-def card)]
     (-> card
         (assoc :cid cid
                :implementation (card-implemented card)
                :subroutines (subroutines-init (assoc card :cid cid) cdef)
                :abilities (ability-init cdef)
                :expend (:expend cdef)
                :enforce-conditions (:enforce-conditions cdef)
                :trash-when-tagged (:trash-when-tagged cdef)
                :x-fn (:x-fn cdef)
                :poison (:poison cdef)
                :highlight-in-discard (:highlight-in-discard cdef)
                :printed-title (:title card))
         (dissoc :setname :text :_id :influence :number :influencelimit
                 :image_url :factioncost :format :quantity)
         (map->Card)))))

(defn reset-card
  "Resets a card back to its original state - retaining any data in the :persistent key"
  ([state side {:keys [cid persistent previous-zone printed-title seen title zone]}]
   (swap! state update :per-turn dissoc cid)
   (let [s-card (server-card (or printed-title title))
         new-card (make-card s-card cid)]
     (update! state side (assoc new-card
                                :persistent persistent
                                :previous-zone previous-zone
                                :seen seen
                                :zone zone)))))
