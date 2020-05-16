(in-ns 'game.core)

(declare free-mu host set-prop)

;;; Deactivate a card
(defn- dissoc-card
  "Dissoc relevant keys in card"
  [card keep-counter]
  (let [c (dissoc card :current-strength :abilities :subroutines :runner-abilities :corp-abilities :rezzed :special :new
                  :added-virus-counter :subtype-target :sifr-used :sifr-target :pump :server-target)
        c (assoc c :subroutines (subroutines-init c (card-def card)))
        c (if keep-counter c (dissoc c :counter :rec-counter :advance-counter :extra-advance-counter))]
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
(defn- ability-init
  "Gets abilities associated with the card"
  [cdef]
  (let [abilities (if (:recurring cdef)
                    (conj (:abilities cdef) {:msg "Take 1 [Recurring Credits]"})
                    (:abilities cdef))]
    (for [ab abilities]
      (assoc (dissoc ab :req :effect) :label (make-label ab)))))

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
  ([state side eid card {:keys [resolve-effect init-data] :as args}]
   (let [cdef (card-def card)
         recurring (:recurring cdef)
         abilities (ability-init cdef)
         run-abs (runner-ability-init cdef)
         corp-abs (corp-ability-init cdef)
         c (merge card
                  (when init-data (:data cdef))
                  {:abilities abilities
                   :runner-abilities run-abs
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
