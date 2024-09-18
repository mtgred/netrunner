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

(defn reveal-loud
  "Trigger the event for revealing one or more cards, and also handle the log printout"
  [state side eid card {:keys [forced and-then] :as args} & targets]
  (let [cards-by-zone (group-by #(select-keys % [:side :zone]) (flatten targets))
        strs (map #(str (enumerate-str (map :title (get cards-by-zone %)))
                        " from " (name-zone (:side %) (:zone %)))
                  (keys cards-by-zone))
        ;; it's awkward to template a string that could refer to one or many
        ;; like "add it to the top of the stack" vs "add them to the top of the stack"
        ;; so I'm choosing to match the tokens [it] and [them] for this purpose
        plural-repr (if (< 1 (count (flatten targets))) "them" "it")
        follow-up (when and-then (string/replace and-then #"(\[it\])|(\[them\])" plural-repr))]
    (if forced
      (system-msg state (other-side side) (str " uses " (:title card) " to force the "
                                               (string/capitalize (name side)) " to reveal "
                                               (enumerate-str strs) follow-up))
      (system-msg state side (str " uses " (:title card) " to reveal " (enumerate-str strs) follow-up)))
    (reveal state side eid targets)))
