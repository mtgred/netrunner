(ns game.core.to-string
  (:require [game.core.card :refer :all]
            [game.core.card-defs :refer [card-def]]
            [game.utils :refer :all]))

(defn card-str
  "Gets a string description of an installed card, reflecting whether it is rezzed,
  in/protecting a server, facedown, or hosted."
  ([state card] (card-str state card nil))
  ([state {:keys [zone host title facedown] :as card} {:keys [visible] :as args}]
  (str (if (corp? card)
         ; Corp card messages
         (str (if (or (rezzed? card)
                      visible)
                title
                (if (ice? card) "ICE" "a card"))
              ; Hosted cards do not need "in server 1" messages, host has them
              (if-not host
                (str (cond
                       (ice? card) " protecting "
                       (is-root? zone) " in the root of "
                       :else " in ")
                     ;TODO add naming of scoring area of corp/runner
                     (zone->name (or (second zone) zone)) ;; handles [:hand] as well as [:servers :hq]
                     (when (ice? card)
                       (str " at position " (card-index state card))))))
         ; Runner card messages
         (if (or facedown visible)
           "a facedown card"
           title))
       (when host (str " hosted on " (card-str state host))))))

(defn name-zone
  "Gets a string representation for the given zone."
  [side zone]
  (let [zone (vec zone)]
  (cond
    (= zone [:hand]) (if (= side "Runner") "Grip" "HQ")
    (= zone [:discard]) (if (= side "Runner") "Heap" "Archives")
    (= zone [:deck]) (if (= side "Runner") "Stack" "R&D")
    (= (take 1 zone) [:rig]) "Rig"
    (= (take 2 zone) [:servers :hq]) "the root of HQ"
    (= (take 2 zone) [:servers :rd]) "the root of R&D"
    (= (take 2 zone) [:servers :archives]) "the root of Archives"
    :else (zone->name (second zone)))))
