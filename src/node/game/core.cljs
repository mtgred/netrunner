(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs zone]]))

(def game-states (atom {}))

(defn pay [state side & args]
  (let [costs (merge-costs args)]
    (if (every? #(>= (- (get-in @state [side (first %)]) (last %)) 0) costs)
      (not (doseq [c costs]
             (swap! state update-in [side (first c)] #(- % (last c)))))
      false)))

(defn move [state side card to]
  (when card
    (let [dest (if (sequential? to) to [to])]
      (swap! state update-in (cons side dest) #(conj % (assoc card :zone dest)))
      (swap! state update-in (cons side (:zone card)) (fn [coll] (remove-once #(not= (:title %) (:title card)) coll))))))

(defn draw
  ([state side] (draw state side 1))
  ([state side n]
     (let [drawn (zone :hand (take n (get-in @state [side :deck])))]
       (swap! state update-in [side :hand] #(concat % drawn)))
     (swap! state update-in [side :deck] (partial drop n))))

(defn do! [{:keys [cost effect]}]
  (fn [state side args]
    (if cost
     (if (apply pay (concat [state side] cost))
       (effect state side args)
       false)
     (effect state side args))))

(defn create-deck [deck]
  (shuffle (mapcat #(repeat (:qty %) (:card %)) (:cards deck))))

(defn init-game [{:keys [players gameid] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        corp-identity (get-in corp [:deck :identity])
        runner-identity (get-in runner [:deck :identity])
        state (atom {:gameid gameid
                     :log []
                     :corp {:user (:user corp)
                            :identity corp-identity
                            :deck (zone :deck (drop 5 corp-deck))
                            :hand (zone :hand (take 5 corp-deck))
                            :discard []
                            :scored []
                            :rfg []
                            :play-area []
                            :servers {:hq {} :rd{} :archive {} :remotes []}
                            :click 3
                            :credit 5
                            :bad-publicity 0
                            :agenda-point 0
                            :max-hand-size 5
                            :keep false}
                     :runner {:user (:user runner)
                              :identity runner-identity
                              :deck (zone :deck (drop 5 runner-deck))
                              :hand (zone :hand (take 5 runner-deck))
                              :discard []
                              :scored []
                              :rfg []
                              :play-area []
                              :rig {:program [] :resource [] :hardware []}
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
      ((:effect corp-init) state :corp nil))
    (when-let [runner-init (game.cards/cards (:title runner-identity))]
      ((:effect runner-init) state :runner nil))
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
        deck (shuffle (reduce concat (:deck player) (for [p args] (zone :deck (p player)))))]
    (swap! state assoc-in [side :deck] deck))
  (doseq [p args] (swap! state assoc-in [side p])))

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

(defn play-ability [state side {:keys [card ability]}]
  (let [ab (get-in game.cards/cards [(:title card) :abilities ability])]
    (when ((do! ab) state side nil)
      (system-msg state side (str "uses " (:title card) " to " (:msg ab) ".")))))

(defn play-instant [state side card]
  (when (pay state side :click 1 :credit (:cost card) (when (has? card :subtype "Double") [:click 1]))
    (move state side card :play-area)
    (when-let [effect (get-in game.cards/cards [(:title card) :effect])]
      (effect state side card))
    (move state side (first (get-in @state [side :play-area])) :discard)
    (system-msg state side (str "plays " (:title card) "."))))

(defn runner-install [state side card]
  (when (pay state side :click 1 :credit (:cost card) :memory (:memoryunits card))
    (move state side card [:rig (keyword (.toLowerCase (:type card)))])
    (when-let [effect (get-in game.cards/cards [(:title card) :effect])]
      (effect state side card))
    (system-msg state side (str "installs " (:title card) "."))))

(defmulti play #(get-in %3 [:card :type]))

(defmethod play "Event" [state side {:keys [card]}]
  (play-instant state side card))

(defmethod play "Operation" [state side {:keys [card]}]
  (play-instant state side card))

(defmethod play "Hardware" [state side {:keys [card]}]
  (runner-install state side card))

(defmethod play "Resource" [state side {:keys [card]}]
  (runner-install state side card))

(defmethod play "Program" [state side {:keys [card]}]
  (runner-install state side card))

(defmethod play :ICE [state side card])
(defmethod play :agenda [state side card])
(defmethod play :asset [state side card])
(defmethod play :upgrade [state side card])
