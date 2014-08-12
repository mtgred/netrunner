(ns game.main
  (:require [cljs.nodejs :as node]))

(aset js/exports "main" game.main)
(enable-console-print!)
(defn noop [])
(set! *main-cli-fn* noop)

(def game-state
  (atom {:gameid 0
         :log []
         :corp {:user {:username "" :emailhash ""}
                :identity {}
                :deck []
                :hand []
                :discard []
                :rfg []
                :remote-servers []
                :click 3
                :credit 5
                :bad-publicity 0
                :agenda-point 0
                :max-hand-size 5}
         :runner {:user {:username "" :emailhash ""}
                  :identity {}
                  :deck []
                  :hand []
                  :discard []
                  :rfg []
                  :rig []
                  :click 4
                  :credit 5
                  :memory 4
                  :link 0
                  :tag 0
                  :agenda-point 0
                  :max-hand-size 5
                  :brain-damage 0}}{}))

(defn create-deck [deck]
  {:identity (:identity deck)
   :deck (shuffle (mapcat #(repeat (:qty %) (:card %)) (:cards deck)))})

(defn init-game [game]
  (let [players (:players game)
        corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)]
    (swap! game-state assoc :gameid (:id game))
    (swap! game-state assoc-in [:runner :user] (:user runner))
    (swap! game-state update-in [:runner] merge (create-deck (:deck runner)))
    (swap! game-state assoc-in [:corp :user] (:user corp))
    (swap! game-state update-in [:corp] merge (create-deck (:deck corp)))))

(defn draw!
  ([side] (draw! side 1))
  ([side n]
     (let [deck (get-in @game-state [side :deck])]
       (swap! game-state update-in [side :hand] #(concat % (take n deck))))
     (swap! game-state update-in [side :deck] (partial drop n))))

(defn pay! [side resource n]
  (swap! game-state update-in [side resource] #(- % n)))

(def commands
  {"draw" (fn [side & args]
            (draw! side)
            (pay! side :click 1))})

(defn exec [action & args]
  (let [params (js->clj args :keywordize-keys true)]
    (case action
      "init" (init-game (first params))
      "do" ((commands (first params)) (keyword (second params)) (rest (rest params)))))
  (clj->js @game-state))
