(ns game.core.revealing
  (:require
   [clojure.string :as string]
   [game.core.eid :refer [effect-completed]]
   [game.core.engine :refer [queue-event checkpoint]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [name-zone]]
   [game.utils :refer [enumerate-str enumerate-cards]]
   [jinteki.utils :refer [other-side]]))

(defn reveal-hand
  "Reveals a side's hand to opponent and spectators."
  [state side]
  (swap! state assoc-in [side :openhand] true))

(defn conceal-hand
  "Conceals a side's revealed hand from opponent and spectators."
  [state side]
  (swap! state update side dissoc :openhand))

;; TODO - find a way to condense these into one fn
(defn reveal-and-queue-event
  [state side & targets]
  (let [cards (flatten targets)]
    (swap! state assoc :last-revealed cards)
    (queue-event state (if (= :corp side) :corp-reveal :runner-reveal) {:cards cards})))

(defn reveal
  "Trigger the event for revealing one or more cards."
  [state side eid & targets]
  (reveal-and-queue-event state side [targets])
  (checkpoint state side eid))

(defn reveal-loud
  "Trigger the event for revealing one or more cards, and also handle the log printout"
  [state side eid card {:keys [forced and-then no-event] :as args} & targets]
  (let [cards-by-zone (group-by #(select-keys % [:side :zone]) (flatten targets))
        strs (map #(str (enumerate-cards (get cards-by-zone %) :sorted)
                        " from " (name-zone (:side %) (:zone %)))
                  (keys cards-by-zone))
        ;; it's awkward to template a string that could refer to one or many
        ;; like "add it to the top of the stack" vs "add them to the top of the stack"
        ;; so I'm choosing to match the tokens [it] and [them] for this purpose
        plural-repr (if (< 1 (count (flatten targets))) "them" "it")
        follow-up (when and-then (string/replace and-then #"(\[it\])|(\[them\])" plural-repr))]
    (if forced
      (system-msg state (other-side side) (str "uses " (:title card) " to force the "
                                               (string/capitalize (name side)) " to reveal "
                                               (enumerate-str strs) follow-up))
      (system-msg state side (str "uses " (:title card) " to reveal " (enumerate-str strs) follow-up)))
    (if-not no-event
      (reveal state side eid targets)
      (effect-completed state side eid))))
