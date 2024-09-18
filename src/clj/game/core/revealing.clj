(ns game.core.revealing
  (:require
   [clojure.string :as string]
   [game.core.engine :refer [trigger-event-sync]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [name-zone]]
   [game.utils :refer [enumerate-str]]
   [jinteki.utils :refer [other-side]]))

(defn reveal-hand
  "Reveals a side's hand to opponent and spectators."
  [state side]
  (swap! state assoc-in [side :openhand] true))

(defn conceal-hand
  "Conceals a side's revealed hand from opponent and spectators."
  [state side]
  (swap! state update side dissoc :openhand))

(defn reveal
  "Trigger the event for revealing one or more cards."
  [state side eid & targets]
  (apply trigger-event-sync state side eid (if (= :corp side) :corp-reveal :runner-reveal) (flatten targets)))

(defn reveal-explicit
  "Trigger the event for revealing one or more cards, and also handle the log printout"
  [state side eid card {:keys [forced] :as args} & targets]
  (let [cards-by-zone (group-by #(select-keys % [:side :zone]) (flatten targets))
        strs (map #(str (enumerate-str (map :title (get cards-by-zone %)))
                        " from " (name-zone (:side %) (:zone %)))
                  (keys cards-by-zone))]
    (if forced
      (system-msg state (other-side side) (str " uses " (:title card) " to force the "
                                               (string/capitalize (name side)) " to reveal "
                                               (enumerate-str strs)))
      (system-msg state side (str " uses " (:title card) " to reveal " (enumerate-str strs))))
    (reveal state side eid targets)))
