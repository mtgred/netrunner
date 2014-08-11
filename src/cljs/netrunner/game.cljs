(ns netrunner.game
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]))

(def game-state
  (atom {:gameid 0
         :log []
         :side :corp
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

(defn load-deck [deck]
  {:identity (:identity deck)
   :deck (shuffle (mapcat #(repeat (:qty %) (:card %)) (:cards deck)))})

(defn init-game [gameid side corp runner]
  (swap! game-state assoc :gameid gameid :side side)
  (swap! game-state assoc-in [:runner :user] (:user runner))
  (swap! game-state update-in [:runner] merge (load-deck (:deck runner)))
  (swap! game-state assoc-in [:corp :user] (:user corp))
  (swap! game-state update-in [:corp] merge (load-deck (:deck corp))))

(defn draw
  ([side] (draw side 1))
  ([side n]
     (let [deck (get-in @game-state [side :deck])]
       (swap! game-state update-in [side :hand] #(concat % (take n deck))))
     (swap! game-state update-in [side :deck] (partial drop n))))
