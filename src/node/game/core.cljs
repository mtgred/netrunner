(ns game.core
  (:require-macros [game.macros :refer [effect req msg]])
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid to-keyword capitalize
                                costs-to-symbol vdissoc]]
            [clojure.string :refer [split-lines split join]]))

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

(defn gain [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(+ % (last r)))))

(defn desactivate [state side card]
  (let [c (dissoc card :counter :advance-counter :current-strength :abilities :rezzed)
        cdef (card-def card)]
    (when-let [leave-effect (:leave-play (card-def card))]
      (leave-effect state side card nil))
    (when-let [mu (:memoryunits card)]
      (gain state :runner :memory mu))
    c))

(defmulti move (fn [state side target to front cross] (map? target)))

(defmethod move false [state side server to]
  (let [from-zone (cons side (if (sequential? server) server [server]))
        to-zone (cons side (if (sequential? to) to [to]))]
    (swap! state assoc-in to-zone (concat (get-in @state to-zone)
                                          (zone to (get-in @state from-zone))))
    (swap! state assoc-in from-zone)))

(defmethod move true [state side {:keys [zone cid] :as card} to front cross]
  (when card
    (let [dest (if (sequential? to) to [to])
          c (if (#{:discard :hand :deck} to) (desactivate state side card) card)
          moved-card (assoc c :zone dest)]
      (if front
        (swap! state update-in (cons side dest) #(cons moved-card (vec %)))
        (swap! state update-in (cons side dest) #(conj (vec %) moved-card)))
      (swap! state update-in (cons (if cross (if (= side :corp) :runner :corp) side) (vec zone))
             (fn [coll] (remove-once #(not= (:cid %) cid) coll)))
      (let [z (vec (cons :corp (butlast zone)))
            n (last z)]
        (when (and (number? n)
                   (empty? (get-in @state (conj z :content)))
                   (empty? (get-in @state (conj z :ices))))
          (swap! state update-in [:corp :servers :remote] vdissoc n)
          (swap! state assoc-in [:corp :servers :remote]
                 (vec (map-indexed
                       (fn [i s]
                         {:content (for [c (:content s)] (update-in c [:zone] assoc 2 i))
                          :ices (for [c (:ices s)] (update-in c [:zone] assoc 2 i))})
                       (get-in @state [:corp :servers :remote]))))))
      moved-card)))

(declare trigger-event)

(defn draw
  ([state side] (draw state side 1))
  ([state side n]
     (let [drawn (zone :hand (take n (get-in @state [side :deck])))]
       (swap! state update-in [side :hand] #(concat % drawn)))
     (swap! state update-in [side :deck] (partial drop n))
     (trigger-event state side (if (= side :corp) :corp-draw :runner-draw) n)))

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

(declare prompt!)
(declare optional-ability)

(defn resolve-ability [state side {:keys [counter-cost advance-counter-cost cost effect msg req once
                                          once-key optional prompt choices] :as ability}
                       {:keys [title cid counter advance-counter] :as card} targets]
  (when (and optional
             (or (not (:req optional)) ((:req optional) state side card targets)))
    (optional-ability state side card (:prompt optional) optional nil))
  (if choices
    (let [cs (if (sequential? choices) choices (choices state side card targets))]
      (prompt! state side card prompt cs (dissoc ability :choices)))
    (when (and (not (get-in @state [once (or once-key cid)]))
               (or (not req) (req state side card targets))
               (<= counter-cost counter)
               (<= advance-counter-cost advance-counter)
               (apply pay (concat [state side] cost)))
      (let [c (-> card
                  (update-in [:counter] #(- % counter-cost))
                  (update-in [:advance-counter] #(- % advance-counter-cost)))]
        (when (or counter-cost advance-counter-cost)
          (update! state side c))
        (when msg
          (let [desc (if (string? msg) msg (msg state side card targets))]
            (system-msg state side (str "uses " title (when desc (str " to " desc))))))
        (when effect (effect state side c targets)))
      (when once (swap! state assoc-in [once (or once-key cid)] true)))))

(defn prompt! [state side card msg choices ability]
  (let [prompt (if (string? msg) msg (msg state side card nil))]
    (when (> (count choices) 0)
      (swap! state update-in [side :prompt]
             (fn [p]
               (conj (vec p) {:msg prompt :choices choices
                              :effect #(resolve-ability state side ability card [%])}))))))

(defn optional-ability [state side card msg ability targets]
  (let [prompt (if (string? msg) msg (msg state side card targets))]
    (swap! state update-in [side :prompt]
           (fn [p]
             (conj (vec p) {:msg prompt :choices ["Yes" "No"]
                            :effect #(when (= % "Yes")
                                       (resolve-ability state side ability card targets))})))))

(defn resolve-prompt [state side {:keys [choice card] :as args}]
  (let [effect (:effect (first (get-in @state [side :prompt])))]
    (swap! state update-in [side :prompt] rest)
    (effect (or choice card))
    (when (empty? (:prompt @state))
      (swap! state assoc :access true))))

(defn register-events [state side events card]
  (doseq [e events]
    (swap! state update-in [:events (first e)] #(conj % {:ability (last e) :card card}))))

(defn unregister-event [state side event card]
  (swap! state update-in [:events event] #(remove (fn [effect] (= (:card effect) card)) %)))

(defn trigger-event
  ([state side event] (trigger-event state side event nil))
  ([state side event target]
     (doseq [e (get-in @state [:events event])]
       (when-let [card (get-card state (:card e))]
         (resolve-ability state (to-keyword (:side card)) (:ability e) card [target])))))

(defn add-prop [state side card key n]
  (update! state side (update-in card [key] #(+ % n))))

(defn set-prop [state side card & args]
  (update! state side (apply assoc (cons card args))))

(defn card-init [state side card]
  (let [cdef (card-def card)
        abilities (if (:recurring cdef)
                    (conj (:abilities cdef) "Use [Recurring Credits]")
                    (:abilities cdef))
        abilities (for [ab abilities]
                    (or (:label ab) (and (string? (:msg ab)) (capitalize (:msg ab))) ""))
        data (if-let [recurring (:recurring cdef)]
               (assoc (:data cdef) :counter recurring)
               (:data cdef))
        c (merge card data {:abilities abilities})]
    (when-let [recurring (:recurring cdef)]
      (register-events state side
                       {(if (= side :corp) :corp-turn-begins :runner-turn-begins)
                        {:effect (effect (set-prop card :counter recurring))}} c))
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
      (swap! state update-in [:runner :brain-damage] #(+ % n))
      (swap! state update-in [:runner :max-hand-size] #(- % n)))
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

(defn shuffle! [state side kw]
  (swap! state update-in [side kw] shuffle))

(defn change [state side {:keys [key delta]}]
  (let [kw (to-keyword key)]
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
                              :hq-access 1
                              :rd-access 1
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

(defn lose [state side & args]
  (doseq [r (partition 2 args)]
    (if (= (last r) :all)
      (swap! state assoc-in [side (first r)] 0)
      (swap! state update-in [side (first r)] #(max (- % (last r)) 0)))))

(defn gain-agenda-point [state side n]
  (gain state side :agenda-point n)
  (when (>= (get-in @state [side :agenda-point]) (get-in @state [side :agenda-point-req]))
    (system-msg state side "wins the game")))

(defn trash [state side {:keys [zone] :as card}]
  (let [cdef (card-def card)
        moved-card (move state (to-keyword (:side card)) card :discard)]
    (when-let [trash-effect (:trash-effect cdef)]
      (resolve-ability state side trash-effect moved-card nil))
    (trigger-event state side :trash moved-card)))

(defn pump
  ([state side card n] (pump state side card n false))
  ([state side {:keys [strength current-strength] :as card} n all-run]
     (let [c (if current-strength
               card
               (assoc card :current-strength strength :all-run all-run))]
       (update! state side (update-in c [:current-strength] #(+ % n))))))

(defn score [state side {:keys [card]}]
  (when (>= (:advance-counter card) (:advancementcost card))
    (let [moved-card (move state side card :scored)
          c (card-init state side moved-card)]
      (system-msg state side (str "scores " (:title c) " and gains " (:agendapoints c) " agenda points"))
      (swap! state update-in [:corp :register :scored-agenda] #(+ % (:agendapoints c)))
      (gain-agenda-point state side (:agendapoints c))
      (set-prop state side c :advance-counter 0)
      (trigger-event state side :agenda-scored c))))

(defn steal [state side card]
  (let [c (move state :runner card :scored false true)]
    (resolve-ability state :runner (:stolen (card-def c)) c nil)
    (system-msg state :runner (str "steals " (:title c) " and gains " (:agendapoints c) " agenda poitns"))
    (swap! state update-in [:runner :register :stole-agenda] #(+ % (:agendapoints c)))
    (gain-agenda-point state :runner (:agendapoints c))
    (set-prop state :runner c :advance-counter 0)
    (trigger-event state :runner :agenda-stolen c)))

(defn run
  ([state side server] (run state side server nil))
  ([state side server run-effect]
     (let [s (cond
              (= server "HQ") [:hq]
              (= server "R&D") [:rd]
              (= server "Archives") [:archives]
              (keyword? server) [server]
              :else [:remote (last (split server " "))])
           ices (get-in @state (concat [:corp :servers] s [:ices]))]
       (swap! state assoc :per-run nil
              :run {:server s :position 0 :ices ices :access-bonus 0 :run-effect run-effect})
       (swap! state update-in [:runner :register :made-run] #(conj % (first s))))))

(defn handle-access [state side cards]
  (swap! state assoc :access true)
  (doseq [c cards]
    (when-let [name (:title c)]
      (when-let [access-effect (:access (card-def c))]
        (resolve-ability state (to-keyword (:side c)) access-effect c nil))
      (when (not= (:zone c) [:discard])
        (if-let [trash-cost (:trash c)]
          (let [card (assoc c :seen true)]
            (optional-ability state side card (str "Pay " trash-cost "[Credits] to trash " name "?")
                              {:cost [:credit trash-cost]
                               :effect (effect (trash :corp card)
                                               (system-msg (str "pays " trash-cost "[Credits] to trash "
                                                                (:title card))))} nil))
          (when-not (= (:type c) "Agenda")
            (prompt! state side c (str "You accessed " (:title c)) ["OK"] {}))))
      (when (= (:type c) "Agenda")
        (if-let [cost (:steal-cost (card-def c))]
          (optional-ability state side c (str "Pay " (costs-to-symbol cost) " to steal " name "?")
                            {:cost cost
                             :effect (effect (steal c)
                                             (system-msg (str "pays " (costs-to-symbol cost)
                                                              " to steal " (:title c))))} nil)
          (steal state side c))))))

(defmulti access (fn [state side server] (first server)))

(defmethod access :hq [state side server]
  (let [n (+ (get-in @state [:runner :hq-access]) (get-in @state [:run :access-bonus]))]
    (concat (take n (shuffle (get-in @state [:corp :hand])))
            (get-in @state [:corp :servers :hq :content]))))

(defmethod access :rd [state side server]
  (let [n (+ (get-in @state [:runner :rd-access]) (get-in @state [:run :access-bonus]))]
    (concat (take n (get-in @state [:corp :deck]))
            (get-in @state [:corp :servers :rd :content]))))

(defmethod access :archives [state side server]
  (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
  (concat (get-in @state [:corp :discard]) (get-in @state [:corp :servers :archives :content])))

(defmethod access :remote [state side server]
  (get-in @state [:corp :servers :remote (js/parseInt (last server)) :content]))

(defn access-bonus [state side n]
  (swap! state update-in [:run :access-bonus] #(+ % n)))

(defn handle-end-run [state side]
  (let [server (get-in @state [:run :server])]
    (trigger-event state side :run-ends (first server))
    (swap! state assoc-in [:runner :rig :program]
           (for [p (get-in @state [:runner :rig :program])]
             (assoc p :current-strength nil)))
    (when-let [end-run-effect (get-in @state [:run :run-effect :end-run])]
      (resolve-ability state side end-run-effect nil [(first server)])))
  (swap! state assoc :run nil))

(defn do-access [state side server]
  (let [cards (access state side server)]
    (when-not (empty? cards)
      (if (= (first server) :rd)
        (let [n (count cards)]
          (system-msg state side (str "accesses " n " card" (when (> n 1) "s"))))
        (system-msg state side (str "accesses " (join ", "(map :title cards)))))
      (handle-access state side cards)))
  (handle-end-run state side))

(defn successful-run [state side]
  (when-let [successful-run-effect (get-in @state [:run :run-effect :successful-run])]
    (resolve-ability state side successful-run-effect nil nil))
  (let [server (get-in @state [:run :server])]
    (swap! state update-in [:runner :register :successful-run] #(conj % (first server)))
    (swap! state assoc-in [:run :successful] true)
    (trigger-event state side :successful-run (first server))
    (if-let [replace-access (get-in @state [:run :run-effect :replace-access])]
      (swap! state update-in [side :prompt]
         (fn [p]
           (conj (vec p) {:msg "Use Run ability instead of accessing cards?"
                          :choices ["Run ability" "Access"]
                          :effect #(if (= % "Run ability")
                                     (do (resolve-ability state side replace-access nil nil)
                                         (handle-end-run state side))
                                     (do-access state side server))})))
      (do-access state side server))))

(defn end-run [state side]
  (let [server (first (get-in @state [:run :server]))]
    (swap! state update-in [:runner :register :unsuccessful-run] #(conj % server))
    (swap! state assoc-in [:run :unsuccessful] true)
    (trigger-event state side :unsuccessful-run)
    (handle-end-run state side)))

(defn no-action [state side]
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action"))

(defn continue [state side]
  (when (get-in @state [:run :no-action])
    (swap! state update-in [:run :position] inc)
    (swap! state assoc-in [:run :no-action] false)
    (swap! state assoc-in [:runner :rig :program]
           (for [p (get-in @state [:runner :rig :program])]
             (if (or (not (:current-strength p)) (:all-run p))
               p (assoc p :current-strength nil))))
    (system-msg state side "continues the run")))

(defn play-ability [state side {:keys [card ability targets] :as args}]
  (let [cdef (card-def card)
        abilities (:abilities cdef)
        ab (if (= ability (count abilities))
             {:msg "take 1 [Recurring Credits]" :counter-cost 1 :effect (effect (gain :credit 1))}
             (get-in cdef [:abilities ability]))]
    (resolve-ability state side ab card targets)))

(defn start-turn [state side]
  (system-msg state side (str "started his or her turn"))
  (swap! state assoc :active-player side :per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)
  (swap! state assoc-in [side :click] (get-in @state [side :click-per-turn]))
  (trigger-event state side (if (= side :corp) :corp-turn-begins :runner-turn-begins))
  (when (= side :corp) (draw state :corp)))

(defn end-turn [state side]
  (let [max-hand-size (get-in @state [side :max-hand-size])]
    (when (<= (count (get-in @state [side :hand])) max-hand-size)
      (system-msg state side (str "is ending his or her turn"))
      (if (= side :runner)
        (do (when (< (get-in @state [:runner :max-hand-size]) 0)
              (flatline state))
            (trigger-event state side :runner-turn-ends))
        (trigger-event state side :corp-turn-ends))
      (swap! state assoc :end-turn true))))

(defn purge [state side]
  (doseq [card (get-in @state [:runner :rig :program])]
    (when (has? card :subtype "Virus")
      (set-prop state :runner card :counter 0)))
  (trigger-event state side :purge))

(defn play-instant [state side {:keys [title] :as card} & args]
  (let [cdef (card-def card)]
    (when (and (if-let [req (:req cdef)] (req state card (first args)) true)
               (pay state side :click 1 :credit (:cost card) (when (has? card :subtype "Double") [:click 1])))
      (let [c (move state side (assoc card :seen true) :play-area)]
        (system-msg state side (str "plays " title))
        (trigger-event state side (if (= side :corp) :play-operation :play-event) c)
        (resolve-ability state side cdef card nil)
        (move state side (first (get-in @state [side :play-area])) :discard)))))

(defn in-play? [state card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @state [:runner :rig (to-keyword (:type card))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn runner-install [state side {:keys [title type cost memoryunits uniqueness] :as card}]
  (let [dest [:rig (to-keyword type)]]
    (when (and (or (not uniqueness) (not (in-play? state card)))
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
          (when (pay state side :click 1)
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
    (card-init state side (assoc card :rezzed true))
    (system-msg state side (str "rez " (:title card)))
    (trigger-event state side :rez card)))

(defn derez [state side {:keys [card]}]
  (system-msg state side (str "derez " (:title card)))
  (update! state side (desactivate state side card)))

(defn advance [state side {:keys [card]}]
  (when (pay state side :click 1 :credit 1)
    (add-prop state side card :advance-counter 1)
    (system-msg state side "advances a card")))

(defn forfeit [state side {:keys [card]}]
  (system-msg state side (str "forfeits " (:title card)))
  (move state side (desactivate state side card) :rfg)
  (gain state side :agenda-point (- (:agendapoints card))))

(defn move-card [state side {:keys [card server]}]
  (let [label (if (or (= (:side card) "Runner") (:rezzed card) (:seen card))
                (:title card) "a card")
        s (if (#{"HQ" "R&D" "Archives"} server) :corp :runner)]
    (case server
      ("Heap" "Archives")
      (do (trash state s card)
          (system-msg state side (str "trashes " label)))
      ("HQ" "Grip")
      (do (move state s (dissoc card :seen :rezzed) :hand false (not= s side))
          (system-msg state side (str "moves " label " to " server)))
      ("Stack" "R&D")
      (do (move state s (dissoc card :seen :rezzed) :deck true (not= s side))
          (system-msg state side (str "moves " label " to the top of " server)))
      nil)))

(defn click-run [state side {:keys [server] :as args}]
  (when (pay state :runner :click 1)
    (system-msg state :runner (str "makes a run on " server))
    (run state side server)))

(defn click-draw [state side]
  (when (pay state side :click 1)
    (system-msg state side "spends [Click] to draw a card")
    (draw state side)
    (trigger-event state side (if (= side :corp) :corp-click-draw :runner-click-draw))))

(defn click-credit [state side]
  (when (pay state side :click 1)
    (system-msg state side "spends [Click] to gain 1 [Credits]")
    (gain state side :credit 1)
    (trigger-event state side (if (= side :corp) :corp-click-credit :runner-click-credit))))

(defn jack-out [state side]
  (end-run state side)
  (system-msg state side "jacks out")
  (trigger-event state side :jack-out))

(defn shuffle-deck [state side {:keys [close] :as args}]
  (swap! state update-in [side :deck] shuffle)
  (if close
    (system-msg state side "stops looking at his deck and shuffles it")
    (system-msg state side "shuffles his deck")))
