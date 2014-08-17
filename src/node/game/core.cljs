(ns game.core)

(def game-states (atom {}))

(defn create-deck [deck]
  (shuffle (mapcat #(repeat (:qty %) (:card %)) (:cards deck))))

(defn init-game [{:keys [players gameid log] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        state {:gameid gameid
               :log log
               :corp {:user (:user corp)
                      :identity (get-in corp [:deck :identity])
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
                        :identity (get-in runner [:deck :identity])
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
                        :keep false}}]
    (swap! game-states assoc gameid (atom state))))

(defn say! [state side args]
  (let [user (or (:user args) (get-in @state [side :user]))]
    (swap! state update-in [:log] #(conj % {:user user :text (:text args)}))))

(defn mulligan! [state side args]
  (let [player (side @state)
        deck (shuffle (concat (:deck player) (:hand player)))]
    (swap! state assoc-in [side :hand] (take 5 deck))
    (swap! state assoc-in [side :deck] (drop 5 deck))
    (swap! state assoc-in [side :keep] true)
    (say! state side {:user "__system__"
                      :text (str (get-in player [:user :username]) " takes a mulligan.")})))

(defn keep! [state side args]
  (swap! state assoc-in [side :keep] true)
  (say! state side {:user "__system__"
                    :text (str (get-in @state [side :user :username]) " keeps his or her hand.")}))

(defn draw!
  ([state side] (draw! state side 1))
  ([state side n]
     (let [deck (get-in @state [side :deck])]
       (swap! state update-in [side :hand] #(concat % (take n deck))))
     (swap! state update-in [side :deck] (partial drop n))
     (say! state side {:user "__system__"
                       :text (str (get-in @state [side :user :username]) " draws " n " card" (if (> n 1) "s." "."))})))

(defn gain!
  ([state side resource] (gain! state side resource 1))
  ([state side resource n]
     (swap! state update-in [side resource] #(+ % n))
     (say! state side {:user "__system__"
                       :text (str (get-in @state [side :user :username])
                                  " gains " n (name resource) (if (> n 1) "s." "."))})))

(defn pay!
  ([state side & args]
     (let [resources (partition 2 args)]
       (if (every? #(>= (- (get-in @state [side (first %)]) (last %)) 0) resources)
         (not (doseq [r resources]
                (swap! state update-in [side (first r)] #(- % (last r)))))
         false))))

(defn purge! [state side]
  (let [cards (get-in state [:runner :rig :programs])]
    ;; (filter (fn [card] (some #(= % "virus") (:subtype card))) cards)
    (say! state side {:user "__system__"
                      :text (str (get-in @state [side :user :username]) " purges viruses.")})))
