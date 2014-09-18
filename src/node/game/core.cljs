(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid]]
            [clojure.string :refer [split-lines split]]))

(def game-states (atom {}))

(defn card-def [card]
  (when-let [title (:title card)]
    (game.cards/cards (.replace title "'" ""))))

(defn say [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))]
    (swap! state update-in [:log] #(conj % {:user author :text text}))))

(defn system-msg [state side text]
  (let [username (get-in @state [side :user :username])]
    (say state side {:user "__system__" :text (str username " " text ".")})))

(defn pay [state side & args]
  (let [costs (merge-costs args)]
    (if (every? #(>= (- (get-in @state [side (first %)]) (last %)) 0) costs)
      (not (doseq [c costs]
             (when (= (first c) :click)
               (swap! state assoc-in [side :register :spent-click] true))
             (swap! state update-in [side (first c)] #(- % (last c)))))
      false)))

(defn move [state side {:keys [zone cid] :as card} to]
  (when card
    (let [dest (if (sequential? to) to [to])
          moved-card (assoc card :zone dest)]
      (swap! state update-in (cons side dest) #(conj % moved-card))
      (swap! state update-in (cons side zone)
             (fn [coll] (remove-once #(not= (:cid %) cid) coll)))
      moved-card)))

(defn trash [state side {:keys [zone] :as card}]
  (when (#{:servers :rig} (first zone))
    (when-let [effect (:leave-play (card-def card))]
      (effect state side card)))
  (move state side card :discard))

(defn draw
  ([state side] (draw state side 1))
  ([state side n]
     (let [drawn (zone :hand (take n (get-in @state [side :deck])))]
       (swap! state update-in [side :hand] #(concat % drawn)))
     (swap! state update-in [side :deck] (partial drop n))))

(defn flatline [state]
  (system-msg state :runner "is flatlined"))

(defn damage [state side type n]
  (let [hand (get-in @state [:runner :hand])]
    (when (< (count hand) n)
      (flatline state))
    (do (when (= type :brain)
          (swap! state update-in [:runner :brain-damage] inc)
          (swap! state update-in [:runner :max-hand-size] dec))
        (let [shuffled-hand (shuffle hand)
              discarded (zone :discard (take n shuffled-hand))]
          (swap! state update-in [:runner :discard] #(concat % discarded))
          (swap! state assoc-in [:runner :hand] (drop n shuffled-hand))))))

(defn do! [{:keys [cost effect]}]
  (fn [state side args]
    (if (apply pay (concat [state side] cost))
      (effect state side args)
      false)))

(defn once-per-turn
  ([state side card f] (once-per-turn state side card f (:cid card)))
  ([state side card f key]
     (when-not (get-in @state [:once-per-turn key])
       (swap! state assoc-in [:once-per-turn key] true)
       (f state side card))))

(defn change [state side {:keys [key delta]}]
  (let [kw (keyword (.toLowerCase key))]
    (swap! state update-in [side kw] (partial + delta))
    (system-msg state side (str "sets " key " to " (get-in @state [side kw])
                                " (" (if (> delta 0) (str "+" delta) delta) ")"))))

(defn create-deck [deck]
  (shuffle (mapcat #(map (fn [c] (assoc c :cid (make-cid))) (repeat (:qty %) (:card %)))
                   (:cards deck))))

(defn init-game [{:keys [players gameid] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        corp-identity (or (get-in corp [:deck :identity]) {:side "Corp"})
        runner-identity (or (get-in runner [:deck :identity]) {:side "Runner"})
        state (atom {:gameid gameid
                     :log []
                     :active-player :runner
                     :end-turn true
                     :corp {:user (:user corp)
                            :identity corp-identity
                            :deck (zone :deck (drop 5 corp-deck))
                            :hand (zone :hand (take 5 corp-deck))
                            :discard []
                            :scored []
                            :rfg []
                            :play-area []
                            :servers {:hq {} :rd{} :archive {} :remote []}
                            :click 3
                            :credit 5
                            :bad-publicity 0
                            :agenda-point 0
                            :max-hand-size 5
                            :click-per-turn 3
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
                              :click-per-turn 4
                              :keep false}})]
    (when-let [corp-init (card-def corp-identity)]
      ((:effect corp-init) state :corp nil))
    (when-let [runner-init (card-def runner-identity)]
      ((:effect runner-init) state :runner nil))
    (swap! game-states assoc gameid state)))

(def reset-value
  {:corp {:credit 5 :bad-publicity 0 :max-hand-size 5}
   :runner {:credit 5 :link 0 :memory 4 :max-hand-size 5}})

(defn shuffle-into-deck [state side & args]
  (let [player (side @state)
        deck (shuffle (reduce concat (:deck player) (for [p args] (zone :deck (p player)))))]
    (swap! state assoc-in [side :deck] deck))
  (doseq [p args] (swap! state assoc-in [side p])))

(defn mulligan [state side args]
  (swap! state update-in [side] #(merge % (side reset-value)))
  (shuffle-into-deck state side :hand)
  (draw state side 5)
  (when-let [init-fn (card-def (get-in @state [side :identity]))]
    ((do! init-fn) state side nil))
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "takes a mulligan"))

(defn keep-hand [state side args]
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "keeps his or her hand"))

(defn gain [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(+ % (last r)))))

(defn lose [state side & args]
  (doseq [r (partition 2 args)]
    (if (= (last r) :all)
      (swap! state assoc-in [side (first r)] 0)
      (when (>= (get-in @state [side (first r)]) (last r))
        (swap! state update-in [side (first r)] #(- % (last r)))))))

(defn run [state side {:keys [server] :as args}]
  (pay state :runner :click 1)
  (swap! state assoc-in [:runner :register :made-run] true)
  (system-msg state :runner (str "makes a run on " server)))

(defn update! [state side card]
  (let [zone (cons side (:zone card))
        [head tail] (split-with #(not= (:cid %) (:cid card)) (get-in @state zone))]
    (swap! state assoc-in zone (concat head [card] (rest tail)))))

(defn resolve-ability [state side {:keys [counter-cost cost effect msg req] :as ability}
                       {:keys [title cid counter] :as card}]
  (when (and (not (get-in @state [:once-per-turn cid]))
             (or (not req) (req state))
             (<= counter-cost counter)
             (apply pay (concat [state side] cost)))
    (let [c (update-in card [:counter] #(- % counter-cost))]
      (update! state side c)
      (effect state side c))
    (when msg
      (let [desc (if (string? msg) msg (msg state side card))]
        (system-msg state side (str "uses " title (when desc (str " to " desc))))))))

(defn play-ability [state side {:keys [card ability] :as args}]
  (resolve-ability state side (get-in (card-def card) [:abilities ability]) card))

(defn get-card [state card]
  (some #(when (= (:cid card) (:cid %)) %)
        (get-in @state (cons (keyword (.toLowerCase (:side card))) (:zone card)))))

(defn start-turn [state side]
  (system-msg state side (str "started his or her turn"))
  (swap! state assoc :active-player side :once-per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)
  (swap! state assoc-in [side :click] (get-in @state [side :click-per-turn]))
  (doseq [e (get-in @state [side :events :turn-begins])]
    (resolve-ability state side (:ability e) (get-card state (:card e))))
  (when (= side :corp) (draw state :corp)))

(defn end-turn [state side]
  (swap! state assoc :end-turn true)
  (system-msg state side (str "is ending his or her turn")))

(defn add-prop [state side card key n]
  (update! state side (update-in card [key] #(+ % n))))

(defn set-prop [state side card & args]
  (update! state side (apply assoc (cons card args))))

(defn purge [state side]
  (doseq [card (get-in @state [:runner :rig :program])]
    (when (has? card :subtype "Virus")
      (set-prop state :runner card :counter 0))))

(defn play-instant [state side {:keys [title] :as card}]
  (let [cdef (card-def card)]
    (when (and (if-let [req (:req cdef)] (req state) true)
               (pay state side :click 1 :credit (:cost card) (when (has? card :subtype "Double") [:click 1])))
     (system-msg state side (str "plays " title))
     (move state side card :play-area)
     (when-let [effect (:effect cdef)]
       (effect state side card))
     (move state side (first (get-in @state [side :play-area])) :discard))))

(defn in-play? [state card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn register-events [state side events card]
  (doseq [e events]
    (swap! state update-in [side :events (first e)] #(conj % {:ability (last e) :card card}))))

(defn runner-install [state side {:keys [title type memoryunits uniqueness] :as card}]
  (when (and (or (not uniqueness) (not (in-play? state card)))
             (pay state side :click 1 :credit (:cost card) :memory memoryunits))
    (let [cdef (card-def card)
          abilities (for [ab (split-lines (:text card))
                          :let [matches (re-matches #".*: (.*)" ab)] :when matches]
                      (second matches))
          c (merge card (:data cdef) {:abilities abilities})
          moved-card (move state side c [:rig (keyword (.toLowerCase type))])]
      (when-let [effect (:effect cdef)]
        (effect state side moved-card))
      (when-let [events (:events cdef)]
        (register-events state side events moved-card)))
    (system-msg state side (str "installs " title))))

(defn corp-install [state side card server]
  (let [dest (case server
              "HQ" [:servers :hq]
              "R&D" [:servers :rd]
              "Archives" [:servers :archive]
              "New remote" [:servers :remote (count (get-in @state [:corp :servers :remote]))]
              [:servers :remote (-> (split server " ") last js/parseInt)])]
    (if (= (:type card) "ICE")
      (let [slot (conj dest :ices)]
        (when (pay state side :click 1 :credit (count (get-in @state (cons :corp slot))))
          (move state side card slot)
          (system-msg state side (str "install an ICE on " server))))
      (when (pay state side :click 1)
        (let [slot (conj dest :content)]
          (when (#{"Asset" "Agenda"} (:type card))
            (doseq [c (get-in @state (cons :corp slot))]
              (when (#{"Asset" "Agenda"} (:type c))
                (trash state side c)
                (system-msg state side (str "trash a card in " server)))))
          (move state side card slot))
        (system-msg state side (str "installs a card in " server))))))

(defn play [state side {:keys [card server]}]
  (case (:type card)
    ("Event" "Operation") (play-instant state side card)
    ("Hardware" "Resource" "Program") (runner-install state side card)
    ("ICE" "Upgrade" "Asset" "Agenda") (corp-install state side card server)))

(defn rez [state side {:keys [card]}]
  (when (pay state side :credit (:cost card))
    (let [cdef (card-def card)
          abilities (if (= (:type card) "ICE")
                      (map :label (:abilities cdef))
                      (for [ab (split-lines (:text card))
                            :let [matches (re-matches #".*: (.*)" ab)] :when matches]
                        (second matches)))
          c (merge card (:data cdef) {:abilities abilities :rezzed true})]
      (update! state side c)
      (when-let [effect (:effect cdef)]
        (effect state side c))
      (when-let [events (:events cdef)]
        (register-events state side events c)))
    (system-msg state side (str "rez " (:title card)))))
