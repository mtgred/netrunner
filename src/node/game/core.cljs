(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs]]))

(def game-states (atom {}))

(defn pay [state side & args]
  (let [costs (merge-costs args)]
    (if (every? #(>= (- (get-in @state [side (first %)]) (last %)) 0) costs)
      (not (doseq [c costs]
             (swap! state update-in [side (first c)] #(- % (last c)))))
      false)))

(defn do! [{:keys [cost effect]}]
  (fn [state side args]
    (if cost
     (when (apply pay (concat [state side] cost))
       (effect state side args))
     (effect state side args))))

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
                            :scored []
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
                              :scored []
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
    (when-let [corp-init (game.cards/cards (:title corp-identity))]
      ((do! corp-init) state :corp nil))
    (when-let [runner-init (game.cards/cards (:title runner-identity))]
      ((do! runner-init) state :runner nil))
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

(defn shuffle-into-deck [state side & args]
  (let [player (side @state)
        deck (shuffle (reduce concat (:deck player) (for [p args] (p player))))]
    (swap! state assoc-in [side :deck] deck))
  (doseq [p args] (swap! state assoc-in [side p])))

(defn draw
  ([state side] (draw state side 1))
  ([state side n]
     (let [deck (get-in @state [side :deck])]
       (swap! state update-in [side :hand] #(concat % (take n deck))))
     (swap! state update-in [side :deck] (partial drop n))))

(defn mulligan [state side args]
  (swap! state update-in [side] #(merge % (side reset-value)))
  (shuffle-into-deck state side :hand)
  (draw state side 5)
  (when-let [init-fn (get-in game.cards/cards [(get-in @state [side :identity :title])])]
    ((do! init-fn) state side nil))
  (swap! state assoc-in [side :keep] true)
  (system-msg state side  "takes a mulligan."))

(defn keep-hand [state side args]
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "keeps his or her hand."))

(defn gain [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(+ % (last r)))))

(defn lose [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(max (- % (last r)) 0))))

(defn purge [state side])

(defn move [state side card from to]
  (swap! state update-in [side to] #(conj % card))
  (swap! state update-in [side from] (fn [coll] (remove-once #(not= % card) coll))))

(defn play-instant [state side card]
  (when (pay state side :click 1 :credit (:cost card) (when (has? card :subtype "Double") [:click 1]))
    (if (= (:title card) "Levy AR Lab Access")
      (move state side card :hand :rfg)
      (move state side card :hand :discard))
    ((get-in game.cards/cards [(:title card) :effect]) state side nil)
    (system-msg state side (str "plays " (:title card) "."))))

(defmulti play #(get-in %3 [:card :type]))

(defmethod play "Event" [state side {:keys [card]}]
  (play-instant state side card))

(defmethod play "Operation" [state side {:keys [card]}]
  (play-instant state side card))

(defmethod play :hardware [state side card])
(defmethod play :resource [state side card])
(defmethod play :program [state side card])

(defmethod play :ICE [state side card])
(defmethod play :agenda [state side card])
(defmethod play :asset [state side card])
(defmethod play :upgrade [state side card])
