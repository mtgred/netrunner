(ns game.core.initializing
  (:require
    [game.core.board :refer [all-active all-active-installed]]
    [game.core.card :refer [get-card runner? map->Card]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [card-ability-cost]]
    [game.core.effects :refer [register-constant-effects unregister-constant-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [is-ability? register-events resolve-ability unregister-events]]
    [game.core.finding :refer [find-cid]]
    [game.core.gaining :refer [free-mu gain lose]]
    [game.core.ice :refer [add-sub]]
    [game.core.payment :refer [add-cost-label-to-ability]]
    [game.core.props :refer [set-prop]]
    [game.core.update :refer [update!]]
    [game.macros :refer [effect req]]
    [game.utils :refer [make-cid server-card]]
    [jinteki.utils :refer [make-label]]))

(defn subroutines-init
  "Initialised the subroutines associated with the card, these work as abilities"
  [card cdef]
  (->> (:subroutines cdef)
       (reduce (fn [ice sub] (add-sub ice sub (:cid ice) {:printed true})) card)
       :subroutines
       (into [])))

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
        c (dissoc card :current-strength :runner-abilities :corp-abilities :rezzed :new
                  :added-virus-counter :subtype-target :server-target :extra-advance-counter)
        c (assoc c :subroutines (subroutines-init c cdef) :abilities (ability-init cdef) :special nil)
        c (if keep-counter c (dissoc c :counter :rec-counter :advance-counter))]
    c))

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
  ([state side {:keys [cid disabled facedown installed memoryunits rezzed] :as card} keep-counter]
   (unregister-events state side card)
   (unregister-constant-effects state side card)
   (trigger-leave-effect state side card)
   (when (and memoryunits
              installed
              (not facedown))
     (free-mu state memoryunits))
   (when (and (find-cid cid (all-active-installed state side))
              (not disabled)
              (or rezzed
                  installed))
     (when-let [in-play (:in-play (card-def card))]
       (apply lose state side in-play)))
   (dissoc-card card keep-counter)))


;;; Initialising a card
(defn- corp-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (for [ab (:corp-abilities cdef)]
    (assoc (select-keys ab [:cost]) :label (make-label ab))))

(defn- runner-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (for [ab (:runner-abilities cdef)]
    (assoc (select-keys ab [:cost]) :label (make-label ab))))

(defn card-init
  "Initializes the abilities and events of the given card."
  ([state side card] (card-init state side card {:resolve-effect true :init-data true}))
  ([state side card args] (card-init state side (make-eid state) card args))
  ([state side eid card {:keys [resolve-effect init-data]}]
   (let [cdef (card-def card)
         recurring (:recurring cdef)
         run-abs (runner-ability-init cdef)
         corp-abs (corp-ability-init cdef)
         c (merge card
                  (when init-data (:data cdef))
                  {:runner-abilities run-abs
                   :corp-abilities corp-abs})
         c (if (number? recurring) (assoc c :rec-counter recurring) c)
         c (if (string? (:strength c)) (assoc c :strength 0) c)]
     (when recurring
       (let [r (if (number? recurring)
                 (effect (set-prop card :rec-counter recurring))
                 recurring)]
         (register-events
           state side c
           [{:event (if (= side :corp) :corp-phase-12 :runner-phase-12)
             :req (req (not (:disabled card)))
             :effect r}])))
     (update! state side c)
     (register-events state side c)
     (register-constant-effects state side c)
     (if (and resolve-effect (is-ability? cdef))
       (resolve-ability state side eid (dissoc cdef :cost :additional-cost) c nil)
       (effect-completed state side eid))
     (when-let [in-play (:in-play cdef)]
       (apply gain state side in-play))
     (get-card state c))))

(defn update-ability-cost-str
  [state side card ability-kw]
  (into [] (for [ab (get card ability-kw)
                 :let [ab-cost (if (:break-cost ab)
                                 (assoc ab :cost (:break-cost ab))
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
  (doseq [side [:corp :runner]
          card (all-active state side)]
    (update! state side (update-abilities-cost-str state side card))))

(defn- card-implemented
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
   (-> card
       (assoc :cid cid
              :implementation (card-implemented card)
              :subroutines (subroutines-init (assoc card :cid cid) (card-def card))
              :abilities (ability-init (card-def card)))
       (dissoc :setname :text :_id :influence :number :influencelimit
               :image_url :factioncost :format :quantity)
       (map->Card))))

(defn reset-card
  "Resets a card back to its original state - retaining any data in the :persistent key"
  ([state side {:keys [cid persistent previous-zone seen title zone]}]
   (swap! state update-in [:per-turn] dissoc cid)
   (let [new-card (make-card (server-card title) cid)]
     (update! state side (assoc new-card
                                :persistent persistent
                                :previous-zone previous-zone
                                :seen seen
                                :zone zone)))))
