(ns game.core.to-string
  (:require [game.core.card :refer [get-card rezzed? ice? installed? corp? card-index get-title]]
            [game.core.servers :refer [is-root? zone->name]]
            [stringer.core :as s]))

(defn card-str
  "Gets a string description of an installed card, reflecting whether it is rezzed,
  in/protecting a server, facedown, or hosted."
  ([state card] (card-str state card nil))
  ([state {:keys [zone host facedown] :as card} {:keys [visible]}]
  (s/strcat (if (corp? card)
         (let [installed-ice (and (ice? card) (installed? card))]
           ; Corp card messages
           (s/strcat (if (or (rezzed? card)
                        visible)
                  (get-title card)
                  (if installed-ice "ice" "a card"))
                ; Hosted cards do not need "in server 1" messages, host has them
                (when-not host
                  (s/strcat (cond
                         installed-ice " protecting "
                         (is-root? zone) " in the root of "
                         :else " in ")
                       ;;TODO add naming of scoring area of corp/runner
                       (zone->name (or (second zone) zone)) ;; handles [:hand] as well as [:servers :hq]
                       (when installed-ice
                         (s/strcat " at position " (card-index state card)))))))
         ; Runner card messages
         (if (or facedown visible)
           "a facedown card"
           (get-title card)))
       (when host (s/strcat " hosted on " (card-str state (get-card state host)))))))
