(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid to-keyword]]
            [clojure.string :refer [split-lines split join capitalize]]))

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

(defn can-move? [state side {:keys [cid] :as card} to]
  (let [dest (if (sequential? to) to [to])]
    (not (some #(= (:cid %) cid) (get-in @state (cons side to))))))

(defn move [state side {:keys [zone cid] :as card} to]
  (when card
    (let [dest (if (sequential? to) to [to])
          moved-card (assoc card :zone dest)]
      (swap! state update-in (cons side dest) #(vec (conj % moved-card)))
      (swap! state update-in (cons (keyword (.toLowerCase (:side card))) zone)
             (fn [coll] (remove-once #(not= (:cid %) cid) coll)))
      moved-card)))

(defn trash [state side {:keys [zone] :as card}]
  (let [c (assoc card :counter nil :advance-counter nil)]
    (when (#{:servers :rig} (first zone))
      (when-let [effect (:leave-play (card-def c))]
        (effect state side c nil)))
    (move state side c :discard)))

(defn draw
  ([state side] (draw state side 1))
  ([state side n]
     (let [drawn (zone :hand (take n (get-in @state [side :deck])))]
       (swap! state update-in [side :hand] #(concat % drawn)))
     (swap! state update-in [side :deck] (partial drop n))))

(defn mill
  ([state side] (mill state side 1))
  ([state side n]
     (let [milled (zone :discard (take n (get-in @state [side :deck])))]
       (swap! state update-in [side :discard] #(concat % milled)))
     (swap! state update-in [side :deck] (partial drop n))))

(defn get-card [state {:keys [cid zone side] :as card}]
  (if zone
    (some #(when (= cid (:cid %)) %) (get-in @state (cons (to-keyword side) zone)))
    card))

(defn update! [state side card]
  (if (= (:type card) "Identity")
    (swap! state assoc-in [side :identity] card)
    (let [zone (cons side (:zone card))
          [head tail] (split-with #(not= (:cid %) (:cid card)) (get-in @state zone))]
      (swap! state assoc-in zone (vec (concat head [card] (rest tail)))))))

(defn resolve-ability [state side {:keys [counter-cost advance-counter-cost cost effect msg req once
                                          once-key] :as ability}
                       {:keys [title cid counter advance-counter] :as card} targets]
  (when (and effect
             (not (get-in @state [once (or once-key cid)]))
             (or (not req) (req state card targets))
             (<= counter-cost counter)
             (<= advance-counter-cost advance-counter)
             (apply pay (concat [state side] cost)))
    (let [c (-> card
                (update-in [:counter] #(- % counter-cost))
                (update-in [:advance-counter] #(- % advance-counter-cost)))]
      (when msg
        (let [desc (if (string? msg) msg (msg state side card targets))]
          (system-msg state side (str "uses " title (when desc (str " to " desc))))))
      (update! state side c)
      (effect state side c targets))
    (when once (swap! state assoc-in [once (or once-key cid)] true))))

(defn register-events [state side events card]
  (doseq [e events]
    (swap! state update-in [:events (first e)] #(conj % {:ability (last e) :card card}))))

(defn unregister-event [state side event card]
  (swap! state update-in [:events event] #(remove (fn [effect] (= (:card effect) card)) %)))

(defn trigger-event [state side event target]
  (doseq [e (get-in @state [:events event])]
    (let [card (get-card state (:card e))]
      (resolve-ability state (keyword (.toLowerCase (:side card))) (:ability e) card [target]))))

(defn card-init [state side card]
  (let [cdef (card-def card)
        abilities (if (= (:type card) "ICE")
                    (for [ab (:abilities cdef)]
                      (or (:label ab) (capitalize (:msg ab))))
                    (for [ab (split-lines (:text card))
                          :let [matches (re-matches #".*: (.*)" ab)] :when (second matches)]
                      (second matches)))
        c (merge card (:data cdef) {:abilities abilities :rezzed true})]
    (update! state side c)
    (resolve-ability state side cdef c nil)
    (when-let [events (:events cdef)] (register-events state side events c))
    (get-card state c)))

(defn flatline [state]
  (system-msg state :runner "is flatlined"))

(defn damage [state side type n]
  (let [hand (get-in @state [:runner :hand])]
    (when (< (count hand) n)
      (flatline state))
    (when (= type :brain)
      (swap! state update-in [:runner :brain-damage] inc)
      (swap! state update-in [:runner :max-hand-size] dec))
    (let [shuffled-hand (shuffle hand)
          discarded (zone :discard (take n shuffled-hand))]
      (swap! state update-in [:runner :discard] #(concat % discarded))
      (swap! state assoc-in [:runner :hand] (drop n shuffled-hand)))
    (trigger-event state side :damage type)))

(defn do! [{:keys [cost effect]}]
  (fn [state side args]
    (if (apply pay (concat [state side] cost))
      (effect state side args)
      false)))

(defn change [state side {:keys [key delta]}]
  (let [kw (keyword (.toLowerCase key))]
    (swap! state update-in [side kw] (partial + delta))
    (system-msg state side
                (str "sets " (.replace key "-" " ") " to " (get-in @state [side kw])
                     " (" (if (> delta 0) (str "+" delta) delta) ")"))))

(defn create-deck [deck]
  (shuffle (mapcat #(map (fn [c] (assoc c :cid (make-cid))) (repeat (:qty %) (:card %)))
                   (:cards deck))))

(defn init-game [{:keys [players gameid] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        corp-identity (or (get-in corp [:deck :identity]) {:side "Corp" :type "Identity"})
        runner-identity (or (get-in runner [:deck :identity]) {:side "Runner" :type "Identity"})
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
                            :servers {:hq {} :rd{} :archives {} :remote []}
                            :click 3
                            :credit 5
                            :bad-publicity 0
                            :agenda-point 0
                            :max-hand-size 5
                            :click-per-turn 3
                            :agenda-point-req 7
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
                              :agenda-point-req 7
                              :keep false}})]
    (card-init state :corp corp-identity)
    (card-init state :runner runner-identity)
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
  (when-let [cdef (card-def (get-in @state [side :identity]))]
    (when-let [effect (:effect cdef)] (effect state side nil)))
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

(defn gain-agenda-point [state side n]
  (gain state side :agenda-point n)
  (when (>= (get-in @state [side :agenda-point]) (get-in @state [side :agenda-point-req]))
    (system-msg state side "wins the game")))

(defn add-prop [state side card key n]
  (update! state side (update-in card [key] #(+ % n))))

(defn set-prop [state side card & args]
  (update! state side (apply assoc (cons card args))))

(defn score [state side {:keys [card]}]
  (when (>= (:advance-counter card) (:advancementcost card))
    (let [moved-card (move state side card :scored)
          c (card-init state side moved-card)]
      (system-msg state side (str "scores " (:title c) " and gains " (:agendapoints c) " agenda points"))
      (gain-agenda-point state side (:agendapoints c))
      (set-prop state side c :advance-counter 0)
      (trigger-event state side :agenda-scored c))))

(defn steal [state side card]
  (let [c (move state side card :scored)]
    (resolve-ability state side (:stolen (card-def c)) c nil)
    (system-msg state side (str "steals " (:title c) " and gains " (:agendapoints c) " agenda poitns"))
    (gain-agenda-point state side (:agendapoints c))
    (set-prop state side c :advance-counter 0)
    (trigger-event state side :agenda-stolen c)))

(defn run [state side {:keys [server] :as args}]
  (pay state :runner :click 1)
  (let [s (case server
            "HQ" [:hq]
            "R&D" [:rd]
            "Archives" [:archives]
            [:remote (last (split server " "))])]
    (swap! state assoc :run {:server s :position 0} :per-run nil)
    (swap! state update-in [:runner :register :made-run] #(conj % server)))
  (system-msg state :runner (str "makes a run on " server)))

(defn handle-access [state side cards]
  (doseq [c cards]
    (resolve-ability state (to-keyword (:side c)) (:access (card-def c)) c nil)
    (when (= (:type c) "Agenda") (steal state side c))))

(defmulti access (fn [state side server] (first server)))

(defmethod access :hq [state side server n]
  (take n (shuffle (get-in @state [:corp :hand]))))

(defmethod access :rd [state side server n]
  (take n (get-in @state [:corp :deck])))

(defmethod access :archives [state side server]
  (get-in @state [:corp :discard]))

(defmethod access :remote [state side server]
  (get-in @state [:corp :servers :remote (js/parseInt (last server)) :content]))

(defn successful-run [state side]
  (let [server (get-in @state [:run :server])]
    (swap! state update-in [:runner :register :sucessful-run] #(conj % (first server)))
    (trigger-event state side :successful-run (first server))
    (let [cards (access state side server 1)]
      (when-not (empty? cards)
        (system-msg state side (str "accesses " (join ", "(map :title cards))))
        (handle-access state side cards)))
    (trigger-event state side :successful-run-ends (first server))
    (swap! state assoc :run nil)))

(defn end-run [state side]
  (let [server (first (get-in @state [:run :server]))]
    (swap! state update-in [:runner :register :unsuccessful-run] #(conj % server))
    (trigger-event state side :unsuccessful-run nil)
    (swap! state assoc :run nil)
    (trigger-event state side :run-ends nil)))

(defn no-action [state side]
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action"))

(defn continue [state side]
  (when (get-in @state [:run :no-action])
    (swap! state update-in [:run :position] inc)
    (swap! state assoc-in [:run :no-action] false)
    (system-msg state side "continues the run")))

(defn play-ability [state side {:keys [card ability targets] :as args}]
  (resolve-ability state side (get-in (card-def card) [:abilities ability]) card targets))

(defn start-turn [state side]
  (system-msg state side (str "started his or her turn"))
  (swap! state assoc :active-player side :per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)
  (swap! state assoc-in [side :click] (get-in @state [side :click-per-turn]))
  (trigger-event state side (if (= side :corp) :corp-turn-begins :runner-turn-begins) nil)
  (when (= side :corp) (draw state :corp)))

(defn end-turn [state side]
  (system-msg state side (str "is ending his or her turn"))
  (when (and (= side :runner) (< (get-in @state [:runner :max-hand-size]) 0))
    (flatline state))
  (trigger-event state side :turn-ends nil)
  (swap! state assoc :end-turn true))

(defn purge [state side]
  (doseq [card (get-in @state [:runner :rig :program])]
    (when (has? card :subtype "Virus")
      (set-prop state :runner card :counter 0)))
  (trigger-event state side :purge nil))

(defn play-instant [state side {:keys [title] :as card} & args]
  (let [cdef (card-def card)]
    (when (and (if-let [req (:req cdef)] (req state card (first args)) true)
               (pay state side :click 1 :credit (:cost card) (when (has? card :subtype "Double") [:click 1])))
      (let [c (move state side card :play-area)]
        (system-msg state side (str "plays " title))
        (trigger-event state side (if (= side :corp) :play-operation :play-event) c)
        (when-let [effect (:effect cdef)] (effect state side c))
        (move state side (first (get-in @state [side :play-area])) :discard)))))

(defn in-play? [state card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn runner-install [state side {:keys [title type cost memoryunits uniqueness] :as card}]
  (let [dest [:rig (keyword (.toLowerCase type))]]
    (when (and (can-move? state side card dest)
               (or (not uniqueness) (not (in-play? state card)))
               (if-let [req (:req (card-def card))] (req state card) true)
               (pay state side :click 1 :credit cost :memory memoryunits))
      (let [c (move state side card dest)
            installed-card (card-init state side c)]
        (trigger-event state side :runner-install installed-card)
        (system-msg state side (str "installs " title))))))

(defn corp-install [state side card server]
  (let [dest (case server
              "HQ" [:servers :hq]
              "R&D" [:servers :rd]
              "Archives" [:servers :archives]
              "New remote" [:servers :remote (count (get-in @state [:corp :servers :remote]))]
              [:servers :remote (-> (split server " ") last js/parseInt)])]
    (when (= server "New remote")
      (trigger-event state side :server-created card))
    (let [c (assoc card :advanceable (:advanceable (card-def card)))]
      (if (= (:type c) "ICE")
        (let [slot (conj dest :ices)]
          (when (pay state side :click 1 :credit (count (get-in @state (cons :corp slot))))
            (system-msg state side (str "install an ICE on " server))
            (let [moved-card (move state side c slot)]
              (trigger-event state side :corp-install moved-card))))
        (let [slot (conj dest :content)]
          (when (and (can-move? state side c slot)
                     (pay state side :click 1))
            (when (#{"Asset" "Agenda"} (:type c))
              (doseq [installed-card (get-in @state (cons :corp slot))]
                (when (#{"Asset" "Agenda"} (:type installed-card))
                  (trash state side installed-card)
                  (system-msg state side (str "trash a card in " server)))))
            (system-msg state side (str "installs a card in " server))
            (let [moved-card (move state side c slot)]
              (trigger-event state side :corp-install moved-card))))))))

(defn play [state side {:keys [card server]}]
  (case (:type card)
    ("Event" "Operation") (play-instant state side card)
    ("Hardware" "Resource" "Program") (runner-install state side card)
    ("ICE" "Upgrade" "Asset" "Agenda") (corp-install state side card server)))

(defn rez [state side {:keys [card]}]
  (when (pay state side :credit (:cost card))
    (card-init state side card)
    (system-msg state side (str "rez " (:title card)))))

(defn advance [state side {:keys [card]}]
  (when (pay state side :click 1 :credit 1)
    (add-prop state side card :advance-counter 1)
    (system-msg state side "advance a card")))

(defn click-draw [state side]
  (when (pay state side :click 1)
    (draw state side)
    (trigger-event state side (if (= side :corp) :corp-click-draw :runner-click-draw))))

(defn click-credit [state side]
  (when (pay state side :click 1)
    (gain state side :credit 1)
    (trigger-event state side (if (= side :corp) :corp-click-credit :runner-click-credit))))
