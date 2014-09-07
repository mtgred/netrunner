(ns game.main
  (:require-macros [game.macros :refer [effect]])
  (:require [cljs.nodejs :as node]
            [game.core :refer [game-states do! system-msg pay gain draw run] :as core]))

(aset js/exports "main" game.main)
(enable-console-print!)
(defn noop [])
(set! *main-cli-fn* noop)

(def commands
  {"say" core/say
   "change" core/change
   "mulligan" core/mulligan
   "keep" core/keep-hand
   "draw" (do! {:cost [:click 1] :effect (effect (draw) (system-msg "draws 1 card."))})
   "credit" (do! {:cost [:click 1] :effect (effect (gain :credit 1) (system-msg "gains 1 credit."))})
   "purge" (do! {:cost [:click 3] :effect (effect (core/purge) (system-msg "purges viruses."))})
   "remove-tag" (do! {:cost [:click 1 :credit 2 :tag 1] :effect (effect (system-msg "removes 1 tag."))})
   "play" core/play
   "run" core/run
   "ability" core/play-ability})

(defn convert [args]
  (let [params (js->clj args :keywordize-keys true)]
    (if (get-in params [:args :card])
      (update-in params [:args :card :zone] #(map (fn [k] (keyword k)) %))
      params)))

(defn exec [action args]
  (let [params (convert args)
        gameid (:gameid params)
        state (@game-states (:gameid params))]
    (case action
      "init" (core/init-game params)
      "do" ((commands (:command params)) state (keyword (:side params)) (:args params)))
    (clj->js @(@game-states gameid))))
