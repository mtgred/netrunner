(ns game.main
  (:require [cljs.nodejs :as node]))

(aset js/exports "main" game.main)
(enable-console-print!)
(defn noop [])
(set! *main-cli-fn* noop)

(def game-states (atom {}))

(defn create-deck [deck]
  (shuffle (mapcat #(repeat (:qty %) (:card %)) (:cards deck))))

(defn init-game [{:keys [players gameid log] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        state {:gameid gameid
               :log log
               :corp {:user (:user corp)
                      :identity (get-in corp [:deck :identity])
                      :deck (create-deck (:deck corp))
                      :hand []
                      :discard []
                      :rfg []
                      :remote-servers []
                      :click 3
                      :credit 5
                      :bad-publicity 0
                      :agenda-point 0
                      :max-hand-size 5}
               :runner {:user (:user runner)
                        :identity (get-in runner [:deck :identity])
                        :deck (create-deck (:deck runner))
                        :hand []
                        :discard []
                        :rfg []
                        :rig {:programs [] :resources [] :hardware []}
                        :click 4
                        :credit 5
                        :memory 4
                        :link 0
                        :tag 0
                        :agenda-point 0
                        :max-hand-size 5
                        :brain-damage 0}}]
    (swap! game-states assoc gameid (atom state))))

(defn draw!
  ([state side] (draw! state side 1))
  ([state side n]
     (let [deck (get-in @state [side :deck])]
       (swap! state update-in [side :hand] #(concat % (take n deck))))
     (swap! state update-in [side :deck] (partial drop n))))

(defn gain!
  ([state side resource] (gain! state side resource 1))
  ([state side resource n]
     (swap! state update-in [side resource] #(+ % n))))

(defn pay! [state side & args]
  (let [resources (partition 2 args)]
    (if (every? #(>= (- (get-in @state [side (first %)]) (last %)) 0) resources)
      (not (doseq [r resources]
             (swap! state update-in [side (first r)] #(- % (last r)))))
      false)))

(defn purge! [state side]
  (let [cards (get-in state [:runner :rig :programs])]
    ;; (filter (fn [card] (some #(= % "virus") (:subtype card))) cards)
    ))

(def commands
  {"draw" (fn [state side & args]
            (when (pay! state side :click 1) (draw! state side)))
   "credit" (fn [state side & args]
              (when (pay! state side :click 1) (gain! state side :credit)))
   "purge" (fn [state side & args]
             (when (pay! state side :click 3) (purge! state side)))
   "remove-tag" (fn [state side & args]
                  (pay! state side :click 1 :credit 2 :tag 1))})

(defn exec [action args]
  (let [params (js->clj args :keywordize-keys true)
        gameid (:gameid params)
        state (@game-states (:gameid params))]
    (case action
      "init" (init-game params)
      "do" ((commands (:command params)) state (keyword (:side params)) (:args params)))
    (clj->js @(@game-states gameid))))
