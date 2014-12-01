(ns game.main
  (:require-macros [game.macros :refer [effect]])
  (:require [cljs.nodejs :as node]
            [game.core :refer [game-states do! system-msg pay gain draw end-run] :as core]))

(aset js/exports "main" game.main)
(enable-console-print!)
(defn noop [])
(set! *main-cli-fn* noop)

(def commands
  {"say" core/say
   "system-msg" #(system-msg %1 %2 (:msg %3))
   "change" core/change
   "move" core/move-card
   "mulligan" core/mulligan
   "keep" core/keep-hand
   "start-turn" core/start-turn
   "end-turn" core/end-turn
   "draw" core/click-draw
   "credit" core/click-credit
   "purge" (do! {:cost [:click 3] :effect (effect (core/purge) (system-msg "purges viruses"))})
   "remove-tag" (do! {:cost [:click 1 :credit 2 :tag 1] :effect (effect (system-msg "removes 1 tag"))})
   "play" core/play
   "rez" core/rez
   "run" core/click-run
   "no-action" core/no-action
   "continue" core/continue
   "access" core/successful-run
   "jack-out" core/jack-out
   "advance" core/advance
   "score" core/score
   "choice" core/resolve-prompt
   "shuffle" core/shuffle-deck
   "ability" core/play-ability})

(defn convert [args]
  (let [params (js->clj args :keywordize-keys true)]
    (if (or (get-in params [:args :card]))
      (update-in params [:args :card :zone] #(map (fn [k] (if (string? k) (keyword k) k)) %))
      params)))

(defn exec [action args]
  (let [params (convert args)
        gameid (:gameid params)
        state (@game-states (:gameid params))]
    (case action
      "init" (core/init-game params)
      "do" ((commands (:command params)) state (keyword (:side params)) (:args params)))
    (clj->js @(@game-states gameid))))
