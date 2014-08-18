(ns game.core
  (:require-macros [game.macros :refer [do!]]))

(def game-states (atom {}))

(defn create-deck [deck]
  (shuffle (mapcat #(repeat (:qty %) (:card %)) (:cards deck))))

(defn init-game [{:keys [players gameid log] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        corp-identity (get-in corp [:deck :identity])
        runner-identity (get-in runner [:deck :identity])
        state (atom {:gameid gameid
                     :log log
                     :corp {:user (:user corp)
                            :identity corp-identity
                            :deck (drop 5 corp-deck)
                            :hand (take 5 corp-deck)
                            :discard []
                            :rfg []
                            :remote-servers []
                            :click 3
                            :credit 5
                            :bad-publicity 0
                            :agenda-point 0
                            :max-hand-size 5
                            :keep false}
                     :runner {:user (:user runner)
                              :identity runner-identity
                              :deck (drop 5 runner-deck)
                              :hand (take 5 runner-deck)
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
                              :brain-damage 0
                              :keep false}})]
    (when-let [corp-init (get-in game.cards/cards [(:title corp-identity) :game-init])]
      (corp-init state :corp nil))
    (when-let [runner-init (get-in game.cards/cards [(:title runner-identity) :game-init])]
      (runner-init state :runner nil))
    (swap! game-states assoc gameid state)))

(def reset-value
  {:corp {:credit 5 :bad-publicity 0 :max-hand-size 5}
   :runner {:credit 5 :link 0 :memory 4 :max-hand-size 5}})

(defn say [state side args]
  (let [user (or (:user args) (get-in @state [side :user]))]
    (swap! state update-in [:log] #(conj % {:user user :text (:text args)}))))

(defn system-msg [state side text]
  (let [username (get-in @state [side :user :username])]
    (say state side {:user "__system__" :text (str username " " text)})))

(defn mulligan [state side args]
  (let [player (side @state)
        deck (shuffle (concat (:deck player) (:hand player)))]
    (swap! state update-in [side] #(merge % (side reset-value)))
    (swap! state assoc-in [side :hand] (take 5 deck))
    (swap! state assoc-in [side :deck] (drop 5 deck))
    (swap! state assoc-in [side :keep] true)
    (when-let [init-fn (get-in game.cards/cards [(get-in player [:identity :title]) :game-init])]
      (init-fn state side nil))
    (system-msg state side  "takes a mulligan.")))

(defn keep-hand [state side args]
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "keeps his or her hand."))

(defn draw
  ([state side] (draw state side 1))
  ([state side n]
     (let [deck (get-in @state [side :deck])]
       (swap! state update-in [side :hand] #(concat % (take n deck))))
     (swap! state update-in [side :deck] (partial drop n))))

(defn gain [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(+ % (last r)))))

(defn pay [state side & args]
  (let [resources (partition 2 args)]
    (if (every? #(>= (- (get-in @state [side (first %)]) (last %)) 0) resources)
      (not (doseq [r resources]
             (swap! state update-in [side (first r)] #(- % (last r)))))
      false)))

(defn purge [state side]
  (let [cards (get-in state [:runner :rig :programs])]
    ;; (filter (fn [card] (some #(= % "virus") (:subtype card))) cards)
    ))
