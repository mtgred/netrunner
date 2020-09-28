(ns game.core.to-string
  (:require [game.core.card :refer [rezzed? ice? corp? card-index]]
            [game.core.servers :refer [is-root? zone->name]]))

(defn card-str
  "Gets a string description of an installed card, reflecting whether it is rezzed,
  in/protecting a server, facedown, or hosted."
  ([state card] (card-str state card nil))
  ([state {:keys [zone host title facedown] :as card} {:keys [visible]}]
  (str (if (corp? card)
         ; Corp card messages
         (str (if (or (rezzed? card)
                      visible)
                title
                (if (ice? card) "ICE" "a card"))
              ; Hosted cards do not need "in server 1" messages, host has them
              (when-not host
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
