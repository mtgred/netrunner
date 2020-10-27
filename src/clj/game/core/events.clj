(ns game.core.events
  (:require
    [game.core.card :refer [installed?]]
    [game.utils :refer [side-str]]))

;; Functions for event parsing
(defn turn-events
  "Returns the targets vectors of each event with the given key that was triggered this turn."
  [state _ ev]
  (mapcat rest (filter #(= ev (first %)) (:turn-events @state))))

(defn last-turn?
  [state side event]
  (get-in @state [side :register-last-turn event]))

(defn not-last-turn?
  [state side event]
  (cond
    ; Return false if no previous turn (i.e. turn 1).
    (-> @state side :register-last-turn nil?) false
    (-> @state side :register-last-turn event) false
    :else true))

(defn no-event?
  "Returns true if the given event has not happened yet this turn.
  Filters on events satisfying (pred targets) if given pred."
  ([state side ev] (no-event? state side ev (constantly true)))
  ([state side ev pred]
   (empty? (filter pred (turn-events state side ev)))))

(defn event-count
  "Returns the number of an event this turn."
  ([state side ev] (event-count state side ev (constantly true)))
  ([state side ev pred]
   (count (filter pred (turn-events state side ev)))))

(defn first-event?
  "Returns true if the given event has only occured once this turn.
  Includes itself if this is checked in the requirement for an event ability.
  Filters on events satisfying (pred targets) if given pred."
  ([state side ev] (first-event? state side ev (constantly true)))
  ([state side ev pred]
   (= 1 (event-count state side ev pred))))

(defn second-event?
  "Returns true if the given event has occurred twice this turn.
  Includes itself if this is checked in the requirement for an event ability.
  Filters on events satisfying (pred targets) if given pred."
  ([state side ev] (second-event? state side ev (constantly true)))
  ([state side ev pred]
   (= 2 (event-count state side ev pred))))

(defn first-successful-run-on-server?
  "Returns true if the active run is the first succesful run on the given server"
  [state server]
  (first-event? state :runner :successful-run #(= [server] (:server (first %)))))

(defn first-trash?
  "Returns true if cards have been trashed by either player only once this turn.
  Includes itself if this is checked in the requirement for an event ability.
  Filters on trash events satisfying (pred targets) if given pred.
  Note that trash event targets may be optionally followed by a reason for the trash, or nil."
  ([state] (first-trash? state (constantly true)))
  ([state pred]
   (= 1 (+ (event-count state nil :runner-trash pred)
           (event-count state nil :corp-trash pred)))))

(defn get-turn-damage
  "Returns the value of damage take this turn"
  [state _]
  (apply + (map #(nth % 2) (turn-events state :runner :damage))))

(defn get-installed-trashed
  "Returns list of cards trashed this turn owned by side that were installed"
  [state side]
  (->> (turn-events state side (keyword (str (name side) "-trash")))
       (mapcat (fn [targets] (filter #(installed? (:card %)) targets)))))

(defn first-installed-trash?
  "Returns true if this is the first trash of an installed card this turn by this side"
  [state side]
  (= 1 (count (get-installed-trashed state side))))

(defn first-installed-trash-own?
  "Returns true if this is the first trash of an owned installed card this turn by this side"
  [state side]
  (= 1 (count (filter #(= (:side (:card %)) (side-str side)) (get-installed-trashed state side)))))


;; Functions for run event parsing
(defn run-events
  "Returns the targets vectors of each run event with the given key that was triggered this run."
  [state _ ev]
  (when (:run @state)
    (mapcat rest (filter #(= ev (first %)) (get-in @state [:run :events])))))

(defn no-run-event?
  "Returns true if the given run event has not happened yet this run.
  Filters on run events satisfying (pred targets) if given pred."
  ([state side ev] (no-run-event? state side ev (constantly true)))
  ([state side ev pred]
   (empty? (filter pred (run-events state side ev)))))

(defn run-event-count
  "Returns the number of times a run event has happened this run."
  ([state side ev] (run-event-count state side ev (constantly true)))
  ([state side ev pred]
   (count (filter pred (run-events state side ev)))))

(defn first-run-event?
  "Returns true if the given run event has only occured once this run.
  Includes itself if this is checked in the requirement for a run event ability.
  Filters on run events satisfying (pred targets) if given pred."
  ([state side ev] (first-run-event? state side ev (constantly true)))
  ([state side ev pred]
   (= 1 (run-event-count state side ev pred))))
