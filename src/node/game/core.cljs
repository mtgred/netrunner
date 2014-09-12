(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid]]
            [clojure.string :refer [split-lines split]]))

(def game-states (atom {}))

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

(defn move [state side card to]
  (when card
    (let [dest (if (sequential? to) to [to])]
      (swap! state update-in (cons side dest) #(vec (conj % (assoc card :zone dest))))
      (swap! state update-in (cons side (:zone card))
             (fn [coll] (remove-once #(not= (:cid %) (:cid card)) coll))))))

(defn trash [state side card]
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
    (when-let [corp-init (game.cards/cards (:title corp-identity))]
      ((:effect corp-init) state :corp nil))
    (when-let [runner-init (game.cards/cards (:title runner-identity))]
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
  (when-let [init-fn (get-in game.cards/cards [(get-in @state [side :identity :title])])]
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
      (swap! state update-in [side (first r)] #(max (- % (last r)) 0)))))

(defn run [state side {:keys [server] :as args}]
  (pay state :runner :click 1)
  (let [kw (keyword server)]
    (swap! state assoc-in [:runner :register :made-run] kw))
  (system-msg state :runner (str "runs on " server)))

(defn start-turn [state side]
  (when (= side :corp)
    (draw state :corp))
  (swap! state assoc-in [side :click] (get-in @state [side :click-per-turn]))
  (swap! state assoc :active-player side :once-per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)
  (system-msg state side (str "started his or her turn")))

(defn end-turn [state side]
  (swap! state assoc :end-turn true)
  (system-msg state side (str "is ending his or her turn")))

(defn update! [state side card]
  (let [zone (cons side (:zone card))
        [head tail] (split-with #(not= (:cid %) (:cid card)) (get-in @state zone))]
    (swap! state assoc-in zone (concat head [card] (rest tail)))))

(defn add-counters [state side card n]
  (update! state side (update-in card [:counter] #(+ % n))))

(defn set-counters [state side card n]
  (update! state side (assoc card :counter n)))

(defn purge [state side]
  (doseq [card (get-in @state [:runner :rig :program])]
    (when (has? card :subtype "Virus")
      (set-counters state :runner card 0))))

(defn play-ability [state side {:keys [card ability :as args]}]
  (let [ab (get-in game.cards/cards [(:title card) :abilities ability])
        counter-cost (:counter-cost ab)]
    (when (and (not (get-in @state [:once-per-turn (:cid card)]))
               (<= counter-cost (:counter card))
               (apply pay (concat [state side] (:cost ab))))
      (let [c (update-in card [:counter] #(- % counter-cost))]
        (update! state side c)
        ((:effect ab) state side c))
      (let [msg (:msg ab)
            desc (if (string? msg) msg (msg state side card))]
        (system-msg state side (str "uses " (:title card) (when desc (str " to " desc))))))))

(defn play-instant [state side {:keys [title] :as card}]
  (let [card-def (game.cards/cards title)]
    (when (and (if-let [req (:req card-def)] (req state) true)
               (pay state side :click 1 :credit (:cost card) (when (has? card :subtype "Double") [:click 1])))
     (system-msg state side (str "plays " (:title card)))
     (move state side card :play-area)
     (when-let [effect (:effect card-def)]
       (effect state side card))
     (move state side (first (get-in @state [side :play-area])) :discard))))

(defn in-play? [state card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn runner-install [state side {:keys [title type memoryunits uniqueness] :as card}]
  (when (and (or (not uniqueness) (not (in-play? state card)))
             (pay state side :click 1 :credit (:cost card) :memory memoryunits))
    (let [card-def (game.cards/cards title)
          abilities (for [ab (split-lines (:text card))
                          :let [matches (re-matches #".*: (.*)" ab)] :when matches]
                      (second matches))
          c (merge card (:data card-def) {:abilities abilities})]
      (when-let [effect (:effect card-def)]
        (effect state side c))
      (move state side c [:rig (keyword (.toLowerCase type))]))
    (system-msg state side (str "installs " title))))

(defn create-server [state side card]
  (let [server (case (:type card)
                 ("ICE") {:ices [card]}
                 ("Upgrade") {:upgrades [card]}
                 ("Agenda" "Asset") {:content card})]
    (swap! state update-in [:corp :servers :remote] server)))

(defn corp-install [state side card server]
  (let [dest (case server
              "HQ" [:servers :hq]
              "R&D" [:servers :rd]
              "Archives" [:servers :archive]
              "New remote" [:servers :remote (count (get-in @state [:corp :servers :remote]))]
              [:servers :remote (-> (split server " ") last js/parseInt)])
        card-type (:type card)
        slot (case card-type
               "ICE" :ices
               "Upgrade" :upgrades
               ("Agenda" "Asset") :content)
        cost (if (= card-type "ICE") [:credit (count (get-in @state (cons :corp dest)))])]
    (when (apply pay (concat [state side :click 1] cost))
      (system-msg state side (str "installs a" (if (= card-type "ICE") "n ICE" " card") " in " server))
      (move state side card (conj dest slot)))))

(defn play [state side {:keys [card server]}]
  (case (:type card)
    ("Event" "Operation") (play-instant state side card)
    ("Hardware" "Resource" "Program") (runner-install state side card)
    ("ICE" "Upgrade" "Asset" "Agenda") (corp-install state side card server)))
