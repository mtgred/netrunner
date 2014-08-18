(ns game.main
  (:require-macros [game.macros :refer [do!]])
  (:require [cljs.nodejs :as node]
            [game.core :refer [game-states system-msg pay gain draw] :as core]))

(aset js/exports "main" game.main)
(enable-console-print!)
(defn noop [])
(set! *main-cli-fn* noop)

(def commands
  {"say" core/say
   "mulligan" core/mulligan
   "keep" core/keep-hand
   "draw" (do! {:cost [:click 1] :effect [(draw) (system-msg "draw 1 card.")]})
   "credit" (do! {:cost [:click 1] :effect [(gain :credit 1) (system-msg "take 1 credit.")]})
   "purge" (do! {:cost [:click 3] :effect [(core/purge) (system-msg "purges viruses.")]})
   "remove-tag" (do! {:cost [:click 1 :credit 2 :tag 1] :effect [(system-msg "removes 1 tag.")]})})

(defn exec [action args]
  (let [params (js->clj args :keywordize-keys true)
        gameid (:gameid params)
        state (@game-states (:gameid params))]
    (case action
      "init" (core/init-game params)
      "do" ((commands (:command params)) state (keyword (:side params)) (:args params)))
    (clj->js @(@game-states gameid))))
