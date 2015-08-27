(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid to-keyword capitalize
                                costs-to-symbol vdissoc distinct-by]]
            [game.macros :refer [effect req msg]]
            [clojure.string :refer [split-lines split join]]))

(declare cards)

(def game-states (atom {}))

(defn card-def [card]
  (when-let [title (:title card)]
    (cards (.replace title "'" ""))))

(defn say [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))]
    (swap! state update-in [:log] #(conj % {:user author :text text}))))

(defn system-msg
  ([state side text] (system-msg state side text nil))
  ([state side text {:keys [hr]}]
   (let [username (get-in @state [side :user :username])]
    (say state side {:user "__system__" :text (str username " " text "." (when hr "[hr]"))}))))

(declare prompt! forfeit trigger-event handle-end-run trash update-advancement-cost update-all-advancement-costs
         update-all-ice update-ice-strength update-breaker-strength all-installed resolve-steal-events)

(defn can-pay? [state side & args]
  (let [costs (merge-costs (remove #(or (nil? %) (= % [:forfeit])) args))
        forfeit-cost (some #{[:forfeit] :forfeit} args)
        scored (get-in @state [side :scored])]
    (if (and (every? #(or (>= (- (get-in @state [side (first %)]) (last %)) 0) 
                          (= (first %) :memory)) ;; memoryunits may be negative
                     costs)
             (or (not forfeit-cost) (not (empty? scored))))
      {:costs costs, :forfeit-cost forfeit-cost, :scored scored})))

(defn deduce [state side [attr value]]
  (swap! state update-in [side attr] (if (= attr :memory)
                                       #(- % value) ;; memoryunits may be negative
                                       #(max 0 (- % value))))
  (when (and (= attr :credit) (= side :runner) (get-in @state [:runner :run-credit]))
    (swap! state update-in [:runner :run-credit] #(max 0 (- % value)))))

(defn pay [state side card & args]
  (when-let [{:keys [costs forfeit-cost scored]} (apply can-pay? state side args)]
    (when forfeit-cost
         (if (= (count scored) 1)
           (forfeit state side (first scored))
           (prompt! state side card "Choose an Agenda to forfeit" scored
                    {:effect (effect (forfeit target))})))
    (not (doseq [c costs]
           (when (= (first c) :click)
             (trigger-event state side (if (= side :corp) :corp-spent-click :runner-spent-click) nil)
             (swap! state assoc-in [side :register :spent-click] true))
           (deduce state side c)))))

(defn gain [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(+ (or % 0) (last r)))))

(defn lose [state side & args]
  (doseq [r (partition 2 args)]
    (trigger-event state side (if (= side :corp) :corp-loss :runner-loss) r)
    (if (= (last r) :all)
      (swap! state assoc-in [side (first r)] 0)
      (deduce state side r))))

(defn register-suppress [state side events card]
  (doseq [e events]
    (swap! state update-in [:suppress (first e)] #(conj % {:ability (last e) :card card}))))

(defn register-events [state side events card]
  (doseq [e events]
    (swap! state update-in [:events (first e)] #(conj % {:ability (last e) :card card})))
  (register-suppress state side (:suppress (card-def card)) card))

(defn unregister-suppress [state side card]
  (doseq [e (:suppress (card-def card))]
    (swap! state update-in [:suppress (first e)]
           #(remove (fn [effect] (= (get-in effect [:card :cid]) (:cid card))) %))))

(defn unregister-events [state side card]
  (doseq [e (:events (card-def card))]
    (swap! state update-in [:events (first e)]
           #(remove (fn [effect] (= (get-in effect [:card :cid]) (:cid card))) %)))
  (unregister-suppress state side card))

(defn desactivate
  ([state side card] (desactivate state side card nil))
  ([state side card keep-counter]
   (let [c (dissoc card :current-strength :abilities :rezzed :special :facedown)
         c (if (= (:side c) "Runner") (dissoc c :installed :counter :rec-counter :pump) c)
         c (if keep-counter c (dissoc c :counter :rec-counter :advance-counter))]
     (when-let [leave-effect (:leave-play (card-def card))]
       (when (or (and (= (:side card) "Runner") (:installed card))
                 (:rezzed card)
                 (= (first (:zone card)) :current))
         (leave-effect state side card nil)))
     (when-let [prevent (:prevent (card-def card))]
       (doseq [[ptype pvec] prevent]
         (doseq [psub pvec]
           (swap! state update-in [:prevent ptype psub] (fn [pv] (remove #(= (:cid %) (:cid card)) pv))))))
     (unregister-events state side card)
     (when-let [mu (:memoryunits card)]
       (gain state :runner :memory mu))
     c)))

(defn get-card [state {:keys [cid zone side host type] :as card}]
  (if (= type "Identity")
    (get-in @state [(to-keyword side) :identity])
    (if zone
      (if host
        (let [h (get-card state host)]
          (some #(when (= cid (:cid %)) %) (:hosted h)))
        (some #(when (= cid (:cid %)) %)
              (get-in @state (cons (to-keyword side) (map to-keyword zone)))))
      card)))

(defn update! [state side {:keys [type zone cid host] :as card}]
  (if (= type "Identity")
    (when (= side (to-keyword (:side card)))
      (swap! state assoc-in [side :identity] card))
    (if-let [h (get-card state host)]
      (let [[head tail] (split-with #(not= (:cid %) cid) (:hosted h))]
        (update! state side (assoc h :hosted (vec (concat head [card] (rest tail))))))
      (let [z (cons (to-keyword (:side card)) zone)
            [head tail] (split-with #(not= (:cid %) cid) (get-in @state z))]
        (when-not (empty? tail)
          (swap! state assoc-in z (vec (concat head [card] (rest tail)))))))))

(defn move-zone [state side server to]
  (let [from-zone (cons side (if (sequential? server) server [server]))
        to-zone (cons side (if (sequential? to) to [to]))]
    (swap! state assoc-in to-zone (concat (get-in @state to-zone)
                                          (zone to (get-in @state from-zone))))
    (swap! state assoc-in from-zone [])))

(defn move
  ([state side card to] (move state side card to nil))
  ([state side {:keys [zone cid host installed] :as card} to {:keys [front keep-server-alive] :as options}]
   (let [zone (if host (map to-keyword (:zone host)) zone)]
     (when (and card (or host
                         (some #(when (= cid (:cid %)) %) (get-in @state (cons :runner (vec zone))))
                         (some #(when (= cid (:cid %)) %) (get-in @state (cons :corp (vec zone))))))
       (doseq [h (:hosted card)]
         (trash state side (dissoc (update-in h [:zone] #(map to-keyword %)) :facedown)))
       (let [dest (if (sequential? to) (vec to) [to])
             c (if (and (= side :corp) (= (first dest) :discard) (:rezzed card))
                 (assoc card :seen true) card)
             c (if (or (and (= dest [:rig :facedown]) installed)
                       (and (or installed host (#{:servers :scored :current} (first zone)))
                            (#{:hand :deck :discard} (first dest))
                            (not (:facedown c))))
                 (desactivate state side c) c)
             c (if (= dest [:rig :facedown])(assoc c :facedown true) (dissoc c :facedown))
             moved-card (assoc c :zone dest :host nil :hosted nil :previous-zone (:zone c))]
         (if front
           (swap! state update-in (cons side dest) #(cons moved-card (vec %)))
           (swap! state update-in (cons side dest) #(conj (vec %) moved-card)))
         (doseq [s [:runner :corp]]
           (if host
             (when-let [host-card (some #(when (= (:cid host) (:cid %)) %)
                                        (get-in @state (cons s (vec zone))))]
               (update! state side (update-in host-card [:hosted]
                                              (fn [coll] (remove-once #(not= (:cid %) cid) coll)))))
             (swap! state update-in (cons s (vec zone))
                    (fn [coll] (remove-once #(not= (:cid %) cid) coll)))))
         (let [z (vec (cons :corp (butlast zone)))
               n (last z)]
           (when (and (not keep-server-alive)
                      (number? n)
                      (empty? (get-in @state (conj z :content)))
                      (empty? (get-in @state (conj z :ices))))
             (when-let [run (:run @state)]
               (when (= (last (:server run)) n)
                 (handle-end-run state side)))
             (swap! state update-in [:corp :servers :remote] vdissoc n)
             (swap! state assoc-in [:corp :servers :remote]
                    (vec (map-indexed
                          (fn [i s]
                            (if (< i n) s
                                {:content (vec (for [c (:content s)]
                                                 (let [c (update-in c [:zone] #(assoc (vec %) 2 i))]
                                                   (assoc c :hosted
                                                            (for [h (:hosted c)]
                                                               (assoc-in h [:host :zone] (:zone c)))))))
                                 :ices (vec (for [c (:ices s)]
                                              (update-in c [:zone] #(assoc (vec %) 2 i))))}))
                          (get-in @state [:corp :servers :remote]))))
             (doseq [s (drop n (get-in @state [:corp :servers :remote]))
                     c (concat (:content s) (:ices s))]
               (when (:rezzed c)
                 (when-let [events (:events (card-def c))]
                   (unregister-events state side c)
                   (register-events state side events c)))
               (doseq [h (:hosted c)]
                 (when-let [events (:events (card-def h))]
                   (unregister-events state side h)
                   (register-events state side events h))))))
         (trigger-event state side :card-moved card moved-card)
         moved-card)))))

(defn draw
  ([state side] (draw state side 1))
  ([state side n]
   (let [active-player (get-in @state [:active-player])]
     (when-not (get-in @state [active-player :register :cannot-draw])
       (let [drawn (zone :hand (take n (get-in @state [side :deck])))]
         (swap! state update-in [side :hand] #(concat % drawn)))
       (swap! state update-in [side :deck] (partial drop n))
       (trigger-event state side (if (= side :corp) :corp-draw :runner-draw) n)))))

(defn mill
  ([state side] (mill state side 1))
  ([state side n]
     (let [milled (zone :discard (take n (get-in @state [side :deck])))]
       (swap! state update-in [side :discard] #(concat % milled)))
     (swap! state update-in [side :deck] (partial drop n))))

(declare resolve-ability)

(defn show-prompt
  ([state side card msg choices f] (show-prompt state side card msg choices f nil))
  ([state side card msg choices f {:keys [priority prompt-type show-discard] :as args}]
   (let [prompt (if (string? msg) msg (msg state side card nil))]
     (when (or (:number choices) (#{:credit :counter} choices) (> (count choices) 0))
       (swap! state update-in [side :prompt]
              (if priority
                #(cons {:msg prompt :choices choices :effect f :card card
                        :prompt-type prompt-type :show-discard show-discard} (vec %))
                #(conj (vec %) {:msg prompt :choices choices :effect f :card card
                                :prompt-type prompt-type :show-discard show-discard})))))))

(defn resolve-psi [state side card psi bet]
  (swap! state assoc-in [:psi side] bet)
  (let [opponent (if (= side :corp) :runner :corp)]
    (when-let [opponent-bet (get-in @state [:psi opponent])]
      (lose state opponent :credit opponent-bet)
      (system-msg state opponent (str "spends " opponent-bet " [Credits]"))
      (lose state side :credit bet)
      (system-msg state side (str "spends " bet " [Credits]"))
      (trigger-event state side :psi-game nil)
      (when-let [ability (if (= bet opponent-bet) (:equal psi) (:not-equal psi))]
        (resolve-ability state (:side card) ability card nil)))))

(defn psi-game [state side card psi]
  (swap! state assoc :psi {})
  (doseq [s [:corp :runner]]
    (show-prompt state s card (str "Choose an amount to spend for " (:title card))
                 (map #(str % " [Credits]") (range (min 3 (inc (get-in @state [s :credit])))))
                 #(resolve-psi state s card psi (Integer/parseInt (first (split % #" ")))))))

(defn prompt!
  ([state side card msg choices ability] (prompt! state side card msg choices ability nil))
  ([state side card msg choices ability {:keys [priority prompt-type] :as args}]
    (show-prompt state side card msg choices #(resolve-ability state side ability card [%]) args)))

(defn optional-ability [state side card msg ability targets]
  (show-prompt state side card msg ["Yes" "No"] #(if (= % "Yes")
                                                   (when-let [yes-ability (:yes-ability ability)]
                                                     (resolve-ability state side yes-ability card targets))
                                                   (when-let [no-ability (:no-ability ability)]
                                                     (resolve-ability state side no-ability card targets)))))

(defn resolve-trace [state side boost]
  (let [runner (:runner @state)
        {:keys [strength ability card]} (:trace @state)]
    (system-msg state :runner (str " spends " boost " [Credits] to increase link strength to "
                                   (+ (:link runner) boost)))
    (let [succesful (> strength (+ (:link runner) boost))
          ability (if succesful ability (:unsuccessful ability))]
      (resolve-ability state :corp ability card [strength (+ (:link runner) boost)])
      (trigger-event state :corp (if succesful :successful-trace :unsuccessful-trace)))
    (when-let [kicker (:kicker ability)]
      (when (>= strength (:min kicker))
        (resolve-ability state :corp kicker card [strength (+ (:link runner) boost)])))))

(defn init-trace [state side card {:keys [base] :as ability} boost]
  (trigger-event state side :pre-init-trace card)
  (let [bonus (or (get-in @state [:bonus :trace]) 0)
        base (if (fn? base) (base state side card nil) base) 
        total (+ base boost bonus)]
    (system-msg state :corp (str "uses " (:title card) 
                                 " to initiate a trace with strength " total 
                                 " (" base
                                 (when (> bonus 0) (str " + " bonus " bonus")) 
                                 " + " boost " [Credits])"))
    (swap! state update-in [:bonus] dissoc :trace)
    (show-prompt state :runner card (str "Boost link strength?") :credit #(resolve-trace state side %))
    (swap! state assoc :trace {:strength total :ability ability :card card})
    (trigger-event state side :trace nil)))

(defn resolve-select [state side]
  (let [selected (get-in @state [side :selected 0])
        cards (map #(dissoc % :selected) (:cards selected))
        curprompt (first (get-in @state [side :prompt]))]
    (when-not (empty? cards)
      (doseq [card cards]
        (update! state side card))
      (resolve-ability state side (:ability selected) (:card curprompt) cards))
    (swap! state update-in [side :selected] #(vec (rest %)))
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % curprompt) pr)))))

(defn show-select
  ([state side card ability] (show-select state side card ability nil))
  ([state side card ability {:keys [priority] :as args}]
   (let [ability (update-in ability [:choices :max] #(if (fn? %) (% state side card nil) %))]
     (swap! state update-in [side :selected]
            #(conj (vec %) {:ability (dissoc ability :choices) :req (get-in ability [:choices :req])
                            :max (get-in ability [:choices :max])}))
     (show-prompt state side card
                  (if-let [msg (:prompt ability)]
                    msg
                    (if-let [m (get-in ability [:choices :max])]
                      (str "Select up to " m " targets for " (:title card))
                      (str "Select a target for " (:title card))))
                  ["Done"] (fn [choice] (resolve-select state side))
                  (assoc args :prompt-type :select :show-discard (:show-discard ability))))))

(defn resolve-ability [state side {:keys [counter-cost advance-counter-cost cost effect msg req once
                                          once-key optional prompt choices end-turn player psi trace
                                          not-distinct priority] :as ability}
                       {:keys [title cid counter advance-counter] :as card} targets]
  (when ability
    (when (and optional
               (not (get-in @state [(:once optional) (or (:once-key optional) cid)]))
               (or (not (:req optional)) ((:req optional) state side card targets)))
      (optional-ability state (or (:player optional) side) card (:prompt optional) optional targets))
    (when (and psi (or (not (:req psi)) ((:req psi) state side card targets)))
      (psi-game state side card psi))
    (when (and trace (or (not (:req trace)) ((:req trace) state side card targets)))
      (show-prompt state :corp card "Boost trace strength?" :credit
                   #(init-trace state :corp card trace %)))
    (when (and (not (get-in @state [once (or once-key cid)]))
               (or (not req) (req state side card targets)))
      (if choices
        (if (map? choices)
          (if (:req choices)
            (show-select state (or player side) card ability {:priority priority})
            (let [n ((:number choices) state side card targets)]
              (prompt! state (or player side) card prompt {:number n} (dissoc ability :choices))))
          (let [cs (if-not (fn? choices)
                     choices
                     (let [cards (choices state side card targets)]
                             (if not-distinct
                               cards (distinct-by :title cards))))]
            (prompt! state (or player side) card prompt cs (dissoc ability :choices) {:priority priority})))
        (when (and (or (not counter-cost) (<= counter-cost (or counter 0)))
                   (or (not advance-counter-cost) (<= advance-counter-cost (or advance-counter 0)))
                   (apply pay (concat [state side card] cost)))
          (let [c (-> card
                      (update-in [:advance-counter] #(- (or % 0) (or advance-counter-cost 0)))
                      (update-in [:counter] #(- (or % 0) (or counter-cost 0))))]
            (when (or counter-cost advance-counter-cost)
              (update! state side c)
              (when (= (:type card) "Agenda") (trigger-event state side :agenda-counter-spent card)))
            (when msg
              (let [desc (if (string? msg) msg (msg state side card targets))]
                (system-msg state (to-keyword (:side card))
                            (str "uses " title (when desc (str " to " desc))))))
            (when effect (effect state side c targets))
            (when end-turn
              (swap! state update-in [side :register :end-turn]
                     #(conj % {:ability end-turn :card card :targets targets}))))
          (when once (swap! state assoc-in [once (or once-key cid)] true)))))))

(defn handle-end-run [state side]
  (if-not (empty? (get-in @state [:runner :prompt]))
    (swap! state assoc-in [:run :ended] true)
    (do (let [server (get-in @state [:run :server])]
          (swap! state assoc-in [:run :ending] true)
          (trigger-event state side :run-ends (first server))
          (when (get-in @state [:run :successful])
            (trigger-event state side :successful-run-ends (first server)))
          (when (get-in @state [:run :unsuccessful])
            (trigger-event state side :unsuccessful-run-ends (first server)))
          (update-all-ice state side)
          (doseq [p (filter #(has? % :subtype "Icebreaker") (all-installed state :runner))]
            (update! state side (update-in (get-card state p) [:pump] dissoc :all-run))
            (update! state side (update-in (get-card state p) [:pump] dissoc :encounter ))
            (update-breaker-strength state side p))
          (let [run-effect (get-in @state [:run :run-effect])]
            (when-let [end-run-effect (:end-run run-effect)]
              (resolve-ability state side end-run-effect (:card run-effect) [(first server)]))))
        (swap! state update-in [:runner :credit] - (get-in @state [:runner :run-credit]))
        (swap! state assoc-in [:runner :run-credit] 0)
        (swap! state assoc :run nil))))

(defn add-prop [state side card key n]
  (update! state side (update-in card [key] #(+ (or % 0) n)))
  (if (= key :advance-counter)
    (do (trigger-event state side :advance (get-card state card))
        (when (and (#{"ICE"} (:type card)) (:rezzed card)) (update-ice-strength state side card)))
    (trigger-event state side :counter-added (get-card state card))))

(defn set-prop [state side card & args]
  (update! state side (apply assoc (cons card args))))

(defn resolve-prompt [state side {:keys [choice card] :as args}]
  (let [prompt (first (get-in @state [side :prompt]))
        choice (if (= (:choices prompt) :credit)
                 (min choice (get-in @state [side :credit]))
                 choice)]
    (when (= (:choices prompt) :credit)
      (pay state side card :credit choice))
    (when (= (:choices prompt) :counter)
      (add-prop state side (:card prompt) :counter (- choice)))
    ((:effect prompt) (or choice card))
    (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % prompt) pr)))
    (when (empty? (get-in @state [:runner :prompt]))
      (when-let [run (:run @state)]
        (when (:ended run)
          (handle-end-run state :runner)))
      (swap! state dissoc :access))))

(defn trash-no-cost [state side]
  (when-let [card (:card (first (get-in @state [side :prompt])))]
    (when (= (:type card) "Agenda") ; trashing before the :access events actually fire; fire them manually
      (resolve-steal-events state side card))
    (trash state side card)
    (swap! state update-in [side :prompt] rest)
    (when-let [run (:run @state)]
      (when (and (:ended run) (empty? (get-in @state [:runner :prompt])) )
        (handle-end-run state :runner)))))

(defn select [state side {:keys [card] :as args}]
  (let [r (get-in @state [side :selected 0 :req])]
    (when (or (not r) (r card))
      (let [c (assoc card :selected (not (:selected card)))]
        (update! state side c)
        (if (:selected c)
          (swap! state update-in [side :selected 0 :cards] #(conj % c))
          (swap! state update-in [side :selected 0 :cards]
                 (fn [coll] (remove-once #(not= (:cid %) (:cid card)) coll))))
        (let [selected (get-in @state [side :selected 0])]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side)))))))

(defn trigger-suppress [state side event & targets]
  (reduce #(or %1 ((:req (:ability %2)) state side (:card %2) targets)) false (get-in @state [:suppress event])))

(defn trigger-event [state side event & targets]
  (doseq [{:keys [ability] :as e} (get-in @state [:events event])]
    (when-let [card (get-card state (:card e))]
      (when (and (not (apply trigger-suppress state side event (cons card targets)))
                 (or (not (:req ability)) ((:req ability) state side card targets)))
        (resolve-ability state side ability card targets))))
  (swap! state update-in [:turn-events] #(cons [event targets] %)))

(defn card-init
  ([state side card] (card-init state side card true))
  ([state side card resolve]
  (let [cdef (card-def card)
        abilities (if (:recurring cdef)
                    (conj (:abilities cdef) {:msg "Take 1 [Recurring Credits]"})
                    (:abilities cdef))
        abilities (for [ab abilities]
                    (assoc (select-keys ab [:cost :pump :breaks])
                      :label (or (:label ab) (and (string? (:msg ab)) (capitalize (:msg ab))) "")))
        c (merge card (:data cdef) {:abilities abilities})
        c (if-let [r (:recurring cdef)]
            (if (number? r) (assoc c :rec-counter r) c) c)]
    (when-let [recurring (:recurring cdef)]
      (let [r (if (number? recurring)
                (effect (set-prop card :rec-counter recurring))
                recurring)]
        (register-events state side
                         {(if (= side :corp) :corp-turn-begins :runner-turn-begins)
                          {:effect r}} c)))
    (when-let [prevent (:prevent cdef)]
      (doseq [[ptype pvec] prevent]
        (doseq [psub pvec]
          (swap! state update-in [:prevent ptype psub] #(conj % card)))))
    (update! state side c)
    (when-let [events (:events cdef)]
      (register-events state side events c))
    (when resolve
      (resolve-ability state side cdef c nil))
    (get-card state c))))

(defn ice-strength-bonus [state side n]
  (swap! state update-in [:bonus :ice-strength] (fnil #(+ % n) 0)))

(defn ice-strength [state side {:keys [strength] :as card}]
  (-> (if-let [strfun (:strength-bonus (card-def card))]
              (+ strength (strfun state side card nil))
              strength)
      (+ (or (get-in @state [:bonus :ice-strength]) 0))))

(defn update-ice-strength [state side ice]
  (let [ice (get-card state ice) oldstren (or (:current-strength ice) (:strength ice))]
    (when (:rezzed ice)
      (swap! state update-in [:bonus] dissoc :ice-strength)
      (trigger-event state side :pre-ice-strength ice)
      (update! state side (assoc ice :current-strength (ice-strength state side ice)))
      (trigger-event state side :ice-strength-changed (get-card state ice) oldstren))))

(defn update-ice-in-server [state side server]
  (doseq [ice (:ices server)] (update-ice-strength state side ice) ))

(defn update-all-ice [state side]
  (doseq [central `(:archives :rd :hq)]
    (update-ice-in-server state side (get-in @state [:corp :servers central])))
  (doseq [remote (get-in @state [:corp :servers :remote])]
    (update-ice-in-server state side remote)))

(defn rez-cost-bonus [state side n]
  (swap! state update-in [:bonus :cost] (fnil #(+ % n) 0)))

(defn init-trace-bonus [state side n]
  (swap! state update-in [:bonus :trace] (fnil #(+ % n) 0)))  
  
(defn rez-cost [state side {:keys [cost] :as card}]
  (if (nil? cost)
    nil
    (-> (if-let [rezfun (:rez-cost-bonus (card-def card))]
          (+ cost (rezfun state side card nil))
          cost)
        (+ (or (get-in @state [:bonus :cost]) 0))
        (max 0))))

(defn trash-cost-bonus [state side n]
  (swap! state update-in [:bonus :trash] (fnil #(+ % n) 0)))

(defn trash-cost [state side {:keys [trash] :as card}]
  (if (nil? trash)
    nil
    (-> trash
        (+ (or (get-in @state [:bonus :trash]) 0))
        (max 0))))

(defn install-cost-bonus [state side n]
  (swap! state update-in [:bonus :install-cost] #(merge-costs (concat % n))))

(defn install-cost [state side card all-cost]
  (vec (map #(if (keyword? %) % (max % 0))
            (-> (concat (get-in @state [:bonus :install-cost]) all-cost
                        (when-let [instfun (:install-cost-bonus (card-def card))] (instfun state side card nil)))
            merge-costs flatten))))

(defn damage-count [state side dtype n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when (not unboostable) (get-in @state [:damage :damage-bonus dtype])) 0))
      (- (or (when (not unpreventable) (get-in @state [:damage :damage-prevent dtype])) 0))
      (max 0)))

(defn damage-bonus [state side dtype n]
  (swap! state update-in [:damage :damage-bonus dtype] (fnil #(+ % n) 0)))

(defn damage-prevent [state side dtype n]
  (swap! state update-in [:damage :damage-prevent dtype] (fnil #(+ % n) 0)))

(defn flatline [state]
  (system-msg state :runner "is flatlined"))

(defn resolve-damage [state side type n {:keys [unpreventable unboostable card] :as args}]
  (let [hand (get-in @state [:runner :hand])]
       (when (< (count hand) n)
             (flatline state))
       (when (= type :brain)
             (swap! state update-in [:runner :brain-damage] #(+ % n))
             (swap! state update-in [:runner :max-hand-size] #(- % n)))
       (doseq [c (take n (shuffle hand))]
              (trash state side c {:unpreventable true :cause type} type))
       (trigger-event state side :damage type card)))

(defn damage
  ([state side type n] (damage state side type n nil))
  ([state side type n {:keys [unpreventable unboostable card] :as args}]
    (swap! state update-in [:damage :damage-bonus] dissoc type)
    (swap! state update-in [:damage :damage-prevent] dissoc type)
    (trigger-event state side :pre-damage type card)
    (let [n (damage-count state side type n args)]
         (let [prevent (get-in @state [:prevent :damage type])]
              (if (and (not unpreventable) prevent (> (count prevent) 0))
                (do (system-msg state :runner "has the option to prevent damage")
                    (show-prompt
                      state :runner nil (str "Prevent any of the " n " " (name type) " damage?") ["Done"]
                      (fn [choice]
                          (let [prevent (get-in @state [:damage :damage-prevent type])]
                               (system-msg state :runner
                                           (if prevent
                                             (str "prevents " (if (= prevent Integer/MAX_VALUE) "all" prevent )
                                                  " " (name type) " damage")
                                             "will not prevent damage"))
                               (resolve-damage state side type (max 0 (- n (or prevent 0))) args)))))
                (resolve-damage state side type n args))))))

(defn shuffle! [state side kw]
  (swap! state update-in [side kw] shuffle))

(defn change [state side {:keys [key delta]}]
  (let [kw (to-keyword key)]
    (if (< delta 0)
      (deduce state side [kw (- delta)])
      (swap! state update-in [side kw] (partial + delta)))
    (system-msg state side
                (str "sets " (.replace key "-" " ") " to " (get-in @state [side kw])
                     " (" (if (> delta 0) (str "+" delta) delta) ")"))))

(defn create-deck [deck]
  (shuffle (mapcat #(map (fn [card]
                           (let [c (assoc card :cid (make-cid))
                                 c (dissoc c :setname :text :_id :influence :number :uniqueness :influencelimit
                                           :faction :factioncost)]
                             (if-let [init (:init (card-def c))] (merge c init) c)))
                         (repeat (:qty %) (:card %)))
                   (:cards deck))))

(defn init-game [{:keys [players gameid] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        corp-identity (or (get-in corp [:deck :identity]) {:side "Corp" :type "Identity"})
        runner-identity (or (get-in runner [:deck :identity]) {:side "Runner" :type "Identity"})
        state (atom
               {:gameid gameid :log [] :active-player :runner :end-turn true
                :corp {:user (:user corp) :identity corp-identity
                       :deck (zone :deck (drop 5 corp-deck))
                       :hand (zone :hand (take 5 corp-deck))
                       :discard [] :scored [] :rfg [] :play-area []
                       :servers {:hq {} :rd{} :archives {} :remote []}
                       :click 0 :credit 5 :bad-publicity 0 :agenda-point 0 :max-hand-size 5
                       :click-per-turn 3 :agenda-point-req 7 :keep false}
                :runner {:user (:user runner) :identity runner-identity
                         :deck (zone :deck (drop 5 runner-deck))
                         :hand (zone :hand (take 5 runner-deck))
                         :discard [] :scored [] :rfg [] :play-area []
                         :rig {:program [] :resource [] :hardware []}
                         :click 0 :credit 5 :run-credit 0 :memory 4 :link 0 :tag 0 :agenda-point 0 :max-hand-size 5
                         :hq-access 1 :rd-access 1 :tagged 0
                         :brain-damage 0 :click-per-turn 4 :agenda-point-req 7 :keep false}})]
    (card-init state :corp corp-identity)
    (card-init state :runner runner-identity)
    (swap! game-states assoc gameid state)))

(def reset-value
  {:corp {:credit 5 :bad-publicity 0 :max-hand-size 5}
   :runner {:credit 5 :run-credit 0 :link 0 :memory 4 :max-hand-size 5}})

(defn shuffle-into-deck [state side & args]
  (let [player (side @state)
        deck (shuffle (reduce concat (:deck player) (for [p args] (zone :deck (p player)))))]
    (swap! state assoc-in [side :deck] deck))
  (doseq [p args]
    (swap! state assoc-in [side p] [])))

(defn mulligan [state side args]
  (shuffle-into-deck state side :hand)
  (draw state side 5)
  (let [card (get-in @state [side :identity])]
    (when-let [cdef (card-def card)]
      (when-let [mul (:mulligan cdef)]
        (mul state side card nil))))
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "takes a mulligan")
  (trigger-event state side :pre-first-turn))

(defn keep-hand [state side args]
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "keeps their hand")
  (trigger-event state side :pre-first-turn))

(defn gain-agenda-point [state side n]
  (gain state side :agenda-point n)
  (when (>= (get-in @state [side :agenda-point]) (get-in @state [side :agenda-point-req]))
    (system-msg state side "wins the game")))

(defn resolve-trash [state side {:keys [zone type] :as card} {:keys [unpreventable cause keep-server-alive] :as args} & targets]
  (let [cdef (card-def card)
        moved-card (move state (to-keyword (:side card)) card :discard {:keep-server-alive keep-server-alive})]
    (when-let [trash-effect (:trash-effect cdef)]
      (resolve-ability state side trash-effect moved-card (cons cause targets)))))

(defn trash-resource [state side args]
  (when (pay state side nil :click 1 :credit 2)
    (resolve-ability state side
                     {:prompt "Choose a resource to trash"
                      :choices {:req #(= (:type %) "Resource")}
                      :effect (effect (trash target)
                                      (system-msg (str "spends [Click] and 2 [Credits] to trash "
                                                       (:title target))))} nil nil)))

(defn trash-prevent [state side type n]
  (swap! state update-in [:trash :trash-prevent type] (fnil #(+ % n) 0)))

(defn trash
  ([state side {:keys [zone type] :as card}] (trash state side card nil))
  ([state side {:keys [zone type] :as card} {:keys [unpreventable cause] :as args} & targets]
   (when (not (some #{:discard} zone))
     (let [ktype (keyword (clojure.string/lower-case type))]
        (when (and (not unpreventable) (not= cause :ability-cost))
          (swap! state update-in [:trash :trash-prevent] dissoc ktype))
        (when (not= (last zone) :current)
          (apply trigger-event state side (keyword (str (name side) "-trash")) card cause targets))
        (let [prevent (get-in @state [:prevent :trash ktype])]
          (if (and (not unpreventable) (not= cause :ability-cost) (> (count prevent) 0))
            (do
              (system-msg state :runner "has the option to prevent trash effects")
              (show-prompt
                state :runner nil (str "Prevent the trashing of " (:title card) "?") ["Done"]
                (fn [choice]
                  (if-let [prevent (get-in @state [:trash :trash-prevent ktype])]
                    (do
                      (system-msg state :runner (str "prevents the trashing of " (:title card)))
                      (swap! state update-in [:trash :trash-prevent] dissoc ktype))
                    (do
                      (system-msg state :runner (str "will not prevent the trashing of " (:title card)))
                      (apply resolve-trash state side card args targets))))))
            (apply resolve-trash state side card args targets)))))))

(defn trash-cards [state side cards]
  (doseq [c cards] (trash state side c)))

(defn all-installed [state side]
  (if (= side :runner)
    (let [installed (flatten (for [t [:program :hardware :resource]] (get-in @state [:runner :rig t])))]
      (concat installed (filter #(:installed %) (mapcat :hosted installed))))
    (let [servers (->> (:corp @state) :servers seq flatten)]
      (concat (mapcat :content servers) (mapcat :ices servers)))))

(defn breaker-strength-bonus [state side n]
  (swap! state update-in [:bonus :breaker-strength] (fnil #(+ % n) 0)))

(defn breaker-strength [state side {:keys [strength] :as card}]
  (if (nil? strength)
    nil
    (-> (if-let [strfun (:strength-bonus (card-def card))]
          (+ strength (strfun state side card nil))
          strength)
        (+ (or (get-in card [:pump :encounter]) 0)
           (or (get-in card [:pump :all-run]) 0)
           (or (get-in @state [:bonus :breaker-strength]) 0)))))

(defn update-breaker-strength [state side breaker]
  (let [breaker (get-card state breaker) oldstren (or (:current-strength breaker) (:strength breaker))]
    (swap! state update-in [:bonus] dissoc :breaker-strength)
    (trigger-event state side :pre-breaker-strength breaker)
    (update! state side (assoc breaker :current-strength (breaker-strength state side breaker)))
    (trigger-event state side :breaker-strength-changed (get-card state breaker) oldstren)))

(defn pump
  ([state side card n] (pump state side card n :encounter))
  ([state side {:keys [strength current-strength] :as card} n duration]
    (update! state side (update-in card [:pump duration] (fnil #(+ % n) 0)))
    (update-breaker-strength state side (get-card state card))))

(defn score [state side args]
  (let [card (or (:card args) args)]
    (when (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card)))
      (let [moved-card (move state :corp card :scored)
            c (card-init state :corp moved-card)]
        (system-msg state :corp (str "scores " (:title c) " and gains " (:agendapoints c)
                                    " agenda point" (when (> (:agendapoints c) 1) "s")))
        (swap! state update-in [:corp :register :scored-agenda] #(+ (or % 0) (:agendapoints c)))
        (gain-agenda-point state :corp (:agendapoints c))
        (set-prop state :corp c :advance-counter 0)
        (when-let [current (first (get-in @state [:runner :current]))]
          (say state side {:user "__system__" :text (str (:title current) " is trashed.")})
          (trash state side current))
        (trigger-event state :corp :agenda-scored (assoc c :advance-counter 0))))))

(defn as-agenda [state side card n]
  (move state side (assoc card :agendapoints n) :scored)
  (gain-agenda-point state side n))

(defn steal [state side card]
  (let [c (move state :runner card :scored)]
    (resolve-ability state :runner (:stolen (card-def c)) c nil)
    (system-msg state :runner (str "steals " (:title c) " and gains " (:agendapoints c)
                                   " agenda point" (when (> (:agendapoints c) 1) "s")))
    (swap! state update-in [:runner :register :stole-agenda] #(+ (or % 0) (:agendapoints c)))
    (gain-agenda-point state :runner (:agendapoints c))
    (set-prop state :runner c :advance-counter 0)
    (when-let [current (first (get-in @state [:corp :current]))]
      (say state side {:user "__system__" :text (str (:title current) " is trashed.")})
      (trash state side current))
    (trigger-event state :runner :agenda-stolen c)))

(defn card->server [state card]
  (let [z (:zone card)]
       (if (= (second z) :remote)
         (nth (get-in @state [:corp :servers :remote]) (nth z 2))
         (get-in @state [:corp :servers (second z)]))))

(defn server->zone [state server]
  (if (sequential? server)
    (vec (cons :servers server))
    (case server
      "HQ" [:servers :hq]
      "R&D" [:servers :rd]
      "Archives" [:servers :archives]
      "New remote" [:servers :remote (count (get-in @state [:corp :servers :remote]))]
      [:servers :remote (-> (split server #" ") last Integer/parseInt)])))

(defn gain-run-credits [state side n]
  (swap! state update-in [:runner :run-credit] + n)
  (gain state :runner :credit n))

(defn run
  ([state side server] (run state side server nil nil))
  ([state side server run-effect card]
     (when-not (get-in @state [:runner :register :cannot-run])
       (let [s (cond
                (= server "HQ") [:hq]
                (= server "R&D") [:rd]
                (= server "Archives") [:archives]
                (keyword? server) [server]
                :else [:remote (-> (split server #" ") last Integer/parseInt)])
             ices (get-in @state (concat [:corp :servers] s [:ices]))]
         (swap! state assoc :per-run nil
                :run {:server s :position (count ices) :ices ices :access-bonus 0
                      :run-effect (assoc run-effect :card card)})
         (gain-run-credits state side (get-in @state [:corp :bad-publicity]))
         (swap! state update-in [:runner :register :made-run] #(conj % (first s)))
         (trigger-event state :runner :run s)))))

(defn steal-cost-bonus [state side costs]
  (swap! state update-in [:bonus :steal-cost] #(merge-costs (concat % costs))))

(defn steal-cost [state side card]
  (-> (if-let [costfun (:steal-cost-bonus (card-def card))]
        (costfun state side card nil)
        nil)
      (concat (get-in @state [:bonus :steal-cost]))
      merge-costs flatten vec))

(defn resolve-steal-events [state side c]
  (let [cdef (card-def c)]
    (when-let [access-effect (:access cdef)]
      (resolve-ability state (to-keyword (:side c)) access-effect c nil))
    (trigger-event state side :access c)))

(defn resolve-steal [state side c]
  (let [cdef (card-def c)]
    (resolve-steal-events state side c)
    (when (or (not (:steal-req cdef)) ((:steal-req cdef) state :runner c nil))
      (steal state :runner c))))

(defn handle-access [state side cards]
  (swap! state assoc :access true)
  (doseq [c cards]
    (swap! state update-in [:bonus] dissoc :trash)
    (swap! state update-in [:bonus] dissoc :steal-cost)
    (let [cdef (card-def c)
          c (assoc c :seen true)]
      (when-let [name (:title c)]
        (if (= (:type c) "Agenda")
          (do (trigger-event state side :pre-steal-cost c)
              (if (not (get-in @state [:runner :register :cannot-steal]))
                (let [cost (steal-cost state side c)]
                  (if (pos? (count cost))
                    (optional-ability state :runner c (str "Pay " (costs-to-symbol cost) " to steal " name "?")
                                      {:yes-ability {:cost cost
                                                     :effect (effect (system-msg (str "pays " (costs-to-symbol cost)
                                                                                      " to steal " (:title c)))
                                                                     (resolve-steal c))}
                                       :no-ability {:effect (effect (resolve-steal-events c))}} nil)
                    (resolve-ability state :runner
                                     {:prompt (str "You access " (:title c)) :choices ["Steal"]
                                      :effect (req (resolve-steal state :runner c))} c nil)))
                (do (resolve-steal-events state side c)
                    (prompt! state :runner c (str "You accessed but cannot steal " (:title c)) ["OK"] {}))))
        (do (when-let [access-effect (:access cdef)]
              (resolve-ability state (to-keyword (:side c)) access-effect c nil))
            (trigger-event state side :access c)
            (trigger-event state side :pre-trash c)
            (when (not= (:zone c) [:discard])
              (if-let [trash-cost (trash-cost state side c)]
                (let [card (assoc c :seen true)]
                  (if (and (get-in @state [:runner :register :force-trash])
                           (can-pay? state :runner :credit trash-cost))
                    (resolve-ability state :runner {:cost [:credit trash-cost]
                                                    :effect (effect (trash card)
                                                            (system-msg (str "is forced to pay " trash-cost
                                                                             " [Credits] to trash " (:title card))))} card nil)
                    (optional-ability state :runner card (str "Pay " trash-cost "[Credits] to trash " name "?")
                                      {:yes-ability {:cost [:credit trash-cost]
                                                     :effect (effect (trash card)
                                                                     (system-msg (str "pays " trash-cost " [Credits] to trash "
                                                                                      (:title card))))}} nil)))
                (prompt! state :runner c (str "You accessed " (:title c)) ["OK"] {})))))))))

(defn max-access [state side n]
  (swap! state assoc-in [:run :max-access] n))

(defn access-count [state side kw]
  (let [run (:run @state)
        accesses (+ (get-in @state [:runner kw]) (:access-bonus run))]
    (if-let [max-access (:max-access run)]
      (min max-access accesses) accesses)))

(defmulti access (fn [state side server] (first server)))
(defmulti choose-access (fn [cards server] (first server)))

(defn access-helper-remote [cards]
  {:prompt "Click a card to access it. You must access all cards in this server."
   :choices {:req #(some (fn [c] (= (:cid %) (:cid c))) cards)}
   :effect (req (handle-access state side [target])
                (when (< 1 (count cards))
                  (resolve-ability state side (access-helper-remote (filter #(not= (:cid %) (:cid target)) cards))
                                   card nil)))})

(defmethod choose-access :remote [cards server]
  {:effect (req (if (>= 1 (count cards))
                  (handle-access state side cards)
                  (resolve-ability state side (access-helper-remote cards) card nil)))})

(defn access-helper-hq [cards]
  {:prompt "Select a card to access."
   :choices (concat (when (some #(= (first (:zone %)) :hand) cards) ["Card from hand"])
                    (map #(if (:rezzed %) (:title %) "Unrezzed upgrade in HQ")
                         (filter #(= (last (:zone %)) :content) cards)))
   :effect (req (case target
                "Unrezzed upgrade in HQ"
                  ; accessing an unrezzed upgrade
                  (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %))) cards)]
                    (if (= 1 (count unrezzed))
                      ; only one unrezzed upgrade; access it and continue
                      (do (system-msg state side (str "accesses " (:title (first unrezzed))))
                          (handle-access state side unrezzed)
                          (when (< 1 (count cards))
                            (resolve-ability
                              state side (access-helper-hq (filter #(not= (:cid %) (:cid (first unrezzed))) cards))
                              card nil)))
                    ; more than one unrezzed upgrade. allow user to select with mouse.
                    (resolve-ability state side
                                     {:prompt "Choose an upgrade in HQ to access."
                                      :choices {:req #(= (second (:zone %)) :hq)}
                                      :effect (effect (system-msg (str "accesses " (:title target)))
                                                      (handle-access [target])
                                                      (resolve-ability (access-helper-hq
                                                                         (remove-once #(not= (:cid %) (:cid target))
                                                                                      cards)) card nil))} card nil)))
                  ; accessing a card in hand or a rezzed upgade
                  "Card from hand"
                  (do (system-msg state side (str "accesses " (:title (first cards))))
                      (handle-access state side [(first cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-hq (rest cards)) card nil)))
                  ; accessing a rezzed upgrade
                  (do (system-msg state side (str "accesses " target))
                      (handle-access state side [(some #(when (= (:title %) target) %) cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-hq
                                                      (remove-once #(not= (:title %) target) cards)) card nil)))))})

(defmethod choose-access :hq [cards server]
  {:effect (req (if (pos? (count cards))
                  (if (= 1 (count cards))
                    (do (when (pos? (count cards)) (system-msg state side (str "accesses " (:title (first cards)))))
                      (handle-access state side cards))
                    (resolve-ability state side (access-helper-hq cards) card nil))))})

(defn access-helper-rd [cards]
  {:prompt "Select a card to access."
   :choices (concat (when (some #(= (first (:zone %)) :deck) cards) ["Card from deck"])
                    (map #(if (:rezzed %) (:title %) "Unrezzed upgrade in R&D")
                         (filter #(= (last (:zone %)) :content) cards)))
   :effect (req (case target
                  "Unrezzed upgrade in R&D"
                  ; accessing an unrezzed upgrade
                  (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %))) cards)]
                    (if (= 1 (count unrezzed))
                      ; only one unrezzed upgrade; access it and continue
                      (do (handle-access state side unrezzed)
                          (when (< 1 (count cards))
                            (resolve-ability
                              state side (access-helper-rd (filter #(not= (:cid %) (:cid (first unrezzed))) cards))
                              card nil)))
                      ; more than one unrezzed upgrade. allow user to select with mouse.
                      (resolve-ability state side
                                       {:prompt "Choose an upgrade in R&D to access."
                                        :choices {:req #(= (second (:zone %)) :rd)}
                                        :effect (effect (handle-access [target])
                                                        (resolve-ability (access-helper-rd
                                                                           (remove-once #(not= (:cid %) (:cid target))
                                                                                        cards)) card nil))} card nil)))
                  ; accessing a card in deck or a rezzed upgade
                  "Card from deck"
                  (do (handle-access state side [(first cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-rd (rest cards)) card nil)))
                  ; accessing a rezzed upgrade
                  (do (handle-access state side [(some #(when (= (:title %) target) %) cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-rd
                                                      (remove-once #(not= (:title %) target) cards)) card nil)))))})

(defmethod choose-access :rd [cards server]
  {:effect (req (if (pos? (count cards))
                  (if (= 1 (count cards))
                    (handle-access state side cards)
                    (resolve-ability state side (access-helper-rd cards) card nil))))})

(defn access-helper-archives [cards]
  {:prompt "Select a card to access. You must access all cards."
   :choices (map #(if (= (last (:zone %)) :content)
                   (if (:rezzed %) (:title %) "Unrezzed upgrade in Archives")
                   (:title %)) cards)
   :effect (req (case target
                  "Unrezzed upgrade in Archives"
                  ; accessing an unrezzed upgrade
                  (let [unrezzed (filter #(and (= (last (:zone %)) :content) (not (:rezzed %))) cards)]
                    (if (= 1 (count unrezzed))
                      ; only one unrezzed upgrade; access it and continue
                      (do (system-msg state side (str "accesses " (:title (first unrezzed))))
                          (handle-access state side unrezzed)
                          (when (< 1 (count cards))
                            (resolve-ability
                              state side (access-helper-archives (filter #(not= (:cid %) (:cid (first unrezzed))) cards))
                              card nil)))
                    ; more than one unrezzed upgrade. allow user to select with mouse.
                      (resolve-ability state side
                                       {:prompt "Choose an upgrade in Archives to access."
                                        :choices {:req #(= (second (:zone %)) :archives)}
                                        :effect (effect (system-msg (str "accesses " (:title target)))
                                                        (handle-access [target])
                                                        (resolve-ability (access-helper-archives
                                                                           (remove-once #(not= (:cid %) (:cid target))
                                                                                        cards)) card nil))} card nil)))
                  ; accessing a rezzed upgrade, or a card in archives
                  (do (system-msg state side (str "accesses " target))
                      (handle-access state side [(some #(when (= (:title %) target) %) cards)])
                      (when (< 1 (count cards))
                        (resolve-ability state side (access-helper-archives
                                                      (remove-once #(not= (:title %) target) cards)) card nil)))))})

(defmethod choose-access :archives [cards server]
  {:effect (req (let [; only include agendas and cards with an :access ability whose :req is true
                      ; (or don't have a :req, or have an :optional with no :req, or :optional with a true :req.)
                      cards (filter #(let [cdef (card-def %)]
                                      (or (= (:type %) "Agenda") (= (last (:zone %)) :content)
                                          (and (:access cdef) (not (get-in cdef [:access :optional]))
                                               (or (not (get-in cdef [:access :req]))
                                                   ((get-in cdef [:access :req]) state side % nil)))
                                          (and (get-in cdef [:access :optional])
                                               (or (not (get-in cdef [:access :optional :req]))
                                                   ((get-in cdef [:access :optional :req]) state side % nil)))))
                                    cards)]
                  (if (pos? (count cards))
                    (if (= 1 (count cards))
                      (do (when (pos? (count cards)) (system-msg state side (str "accesses " (:title (first cards)))))
                          (handle-access state side cards))
                      (resolve-ability state side (access-helper-archives cards) card nil)))))})

(defmethod access :hq [state side server]
  (concat (take (access-count state side :hq-access) (shuffle (get-in @state [:corp :hand])))
          (get-in @state [:corp :servers :hq :content])))

(defmethod access :rd [state side server]
  (concat (take (access-count state side :rd-access) (get-in @state [:corp :deck]))
          (get-in @state [:corp :servers :rd :content])))

(defmethod access :archives [state side server]
  (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
  (concat (get-in @state [:corp :discard]) (get-in @state [:corp :servers :archives :content])))

(defmethod access :remote [state side server]
  (get-in @state [:corp :servers :remote (last server) :content]))

(defn access-bonus [state side n]
  (swap! state update-in [:run :access-bonus] #(+ % n)))

(defn do-access [state side server]
  (trigger-event state side :pre-access (first server))
  (let [cards (access state side server)]
    (when-not (or (= (get-in @state [:run :max-access]) 0) (empty? cards))
      (if (= (first server) :rd)
        (let [n (count cards)]
          (system-msg state side (str "accesses " n " card" (when (> n 1) "s")))))
      (resolve-ability state side (choose-access cards server) nil nil)))
  (handle-end-run state side))

(defn replace-access [state side ability card]
  (resolve-ability state side ability card nil)
  (handle-end-run state side))

(defn successful-run [state side args]
  (when-let [successful-run-effect (get-in @state [:run :run-effect :successful-run])]
    (resolve-ability state side successful-run-effect (:card successful-run-effect) nil))
  (let [server (get-in @state [:run :server])]
    (swap! state update-in [:runner :register :successful-run] #(conj % (first server)))
    (swap! state assoc-in [:run :successful] true)
    (trigger-event state side :successful-run (first server))
    (let [run-effect (get-in @state [:run :run-effect])
          r (:req run-effect)
          card (:card run-effect)
          replace-effect (:replace-access run-effect)]
      (if (and replace-effect
               (or (not r) (r state side card [(first server)])))
        (if (:mandatory replace-effect)
          (replace-access state side replace-effect card)
          (swap! state update-in [side :prompt]
                 (fn [p]
                   (conj (vec p) {:msg "Use Run ability instead of accessing cards?"
                                  :choices ["Run ability" "Access"]
                                  :effect #(if (= % "Run ability")
                                             (replace-access state side replace-effect card)
                                             (do-access state side server))}))))
        (do-access state side server)))))

(defn end-run [state side]
  (let [server (first (get-in @state [:run :server]))]
    (swap! state update-in [:runner :register :unsuccessful-run] #(conj % server))
    (swap! state assoc-in [:run :unsuccessful] true)
    (trigger-event state side :unsuccessful-run)
    (handle-end-run state side)))

(defn no-action [state side args]
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action")
  (when-let [pos (get-in @state [:run :position])]
    (when-let [ice (when (and pos (> pos 0)) (get-card state (nth (get-in @state [:run :ices]) (dec pos))))]
      (when (:rezzed ice)
        (trigger-event state side :encounter-ice ice)
        (update-ice-strength state side ice)))))

(defn continue [state side args]
  (when (get-in @state [:run :no-action])
    (when-let [pos (get-in @state [:run :position])]
      (do (if-let [ice (when (and pos (> pos 0)) (get-card state (nth (get-in @state [:run :ices]) (dec pos))))]
            (trigger-event state side :pass-ice ice)
            (trigger-event state side :pass-ice nil))
          (update-ice-in-server state side (get-in @state (concat [:corp :servers] (get-in @state [:run :server]))))))
    (swap! state update-in [:run :position] dec)
    (swap! state assoc-in [:run :no-action] false)
    (system-msg state side "continues the run")
    (let [pos (get-in @state [:run :position])]
      (when (> (count (get-in @state [:run :ices])) 0)
        (update-ice-strength state side (nth (get-in @state [:run :ices]) pos)))
      (when (> pos 0)
        (let [ice (get-card state (nth (get-in @state [:run :ices]) (dec pos)))]
          (trigger-event state side :approach-ice ice))))
    (doseq [p (filter #(has? % :subtype "Icebreaker") (all-installed state :runner))]
      (update! state side (update-in (get-card state p) [:pump] dissoc :encounter))
      (update-breaker-strength state side p))))

(defn play-ability [state side {:keys [card ability targets] :as args}]
  (let [cdef (card-def card)
        abilities (:abilities cdef)
        ab (if (= ability (count abilities))
             {:msg "take 1 [Recurring Credits]" :req (req (> (:rec-counter card) 0))
              :effect (effect (add-prop card :rec-counter -1) (gain :credit 1))}
             (get-in cdef [:abilities ability]))
        cost (:cost ab)]
    (when (or (nil? cost)
              (apply can-pay? state side cost))
        (when-let [activatemsg (:activatemsg ab)] (system-msg state side activatemsg))
        (resolve-ability state side ab card targets))))

(defn turn-message [state side start-of-turn]
  (let [pre (if start-of-turn "started" "is ending")
        hand (if (= side :runner) "their Grip" "HQ")
        cards (count (get-in @state [side :hand]))
        credits (get-in @state [side :credit])
        text (str pre " their turn with " credits " [Credit] and " cards " cards in " hand)]
    (system-msg state side text {:hr (not start-of-turn)})))

(defn start-turn [state side args]
  (turn-message state side true)
  (swap! state assoc :active-player side :per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)
  (swap! state assoc-in [side :click] (get-in @state [side :click-per-turn]))
  (trigger-event state side (if (= side :corp) :corp-turn-begins :runner-turn-begins))
  (when (= side :corp) (do (draw state :corp) (update-all-advancement-costs state side))))

(defn end-turn [state side args]
  (let [max-hand-size (get-in @state [side :max-hand-size])]
    (when (<= (count (get-in @state [side :hand])) max-hand-size)
      (turn-message state side false)
      (if (= side :runner)
        (do (when (< (get-in @state [:runner :max-hand-size]) 0)
              (flatline state))
            (trigger-event state side :runner-turn-ends))
        (trigger-event state side :corp-turn-ends))
      (doseq [a (get-in @state [side :register :end-turn])]
        (resolve-ability state side (:ability a) (:card a) (:targets a)))
      (swap! state assoc :end-turn true)
      (swap! state dissoc :turn-events))))

(defn purge [state side]
  (trigger-event state side :pre-purge)
  (let [rig-cards (apply concat (vals (get-in @state [:runner :rig])))
        hosted-cards (filter :installed (mapcat :hosted rig-cards))
        hosted-on-ice (->> (get-in @state [:corp :servers]) seq flatten (mapcat :ices) (mapcat :hosted))]
    (doseq [card (concat rig-cards hosted-cards hosted-on-ice)]
      (when (or (has? card :subtype "Virus") (= (:counter-type card) "Virus"))
        (set-prop state :runner card :counter 0))))
  (trigger-event state side :purge))

(defn get-virus-counters [state side card]
   (let [hiveminds (filter #(= (:title %) "Hivemind") (all-installed state :runner))]
        (reduce + (map :counter (cons card hiveminds)))))

(defn play-instant
  ([state side card] (play-instant state side card nil))
  ([state side {:keys [title] :as card} {:keys [targets extra-cost no-additional-cost]}]
     (let [cdef (card-def card)
           additional-cost (if (has? card :subtype "Double")
                             (concat (:additional-cost cdef) [:click 1])
                             (:additional-cost cdef))]
       (when (and (if-let [req (:req cdef)]
                    (req state side card targets) true)
                  (not (and (has? card :subtype "Current")
                            (get-in @state [side :register :cannot-play-current])))
                  (not (and (has? card :subtype "Priority")
                            (get-in @state [side :register :spent-click])))
                  (pay state side card :credit (:cost card) extra-cost
                       (when-not no-additional-cost additional-cost)))
         (let [c (move state side (assoc card :seen true) :play-area)]
           (system-msg state side (str "plays " title))
           (trigger-event state side (if (= side :corp) :play-operation :play-event) c)
           (if (has? c :subtype "Current")
             (do (doseq [s [:corp :runner]]
                   (when-let [current (first (get-in @state [s :current]))]
                     (say state side {:user "__system__" :text (str (:title current) " is trashed.")})
                     (trash state side current)))
                 (let [moved-card (move state side (first (get-in @state [side :play-area])) :current)]
                   (card-init state side moved-card)))
             (do
               (resolve-ability state side cdef card nil)
               (move state side (first (get-in @state [side :play-area])) :discard))))))))

(defn in-play? [state card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @state [:runner :rig (to-keyword (:type card))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn host
  ([state side card target] (host state side card target nil))
  ([state side card {:keys [zone cid host installed] :as target} {:keys [facedown] :as options}]
   (when installed
     (unregister-events state side target))
   (doseq [s [:runner :corp]]
     (if host
       (when-let [host-card (some #(when (= (:cid host) (:cid %)) %)
                                  (get-in @state (cons s (vec (map to-keyword (:zone host))))))]
         (update! state side (update-in host-card [:hosted]
                                        (fn [coll] (remove-once #(not= (:cid %) cid) coll)))))
       (swap! state update-in (cons s (vec zone))
              (fn [coll] (remove-once #(not= (:cid %) cid) coll)))))
   (swap! state update-in (cons side (vec zone)) (fn [coll] (remove-once #(not= (:cid %) cid) coll)))
   (let [c (assoc target :host (-> card
                                   (update-in [:zone] #(map to-keyword %))
                                   (dissoc :hosted))
                         :facedown facedown
                         :zone '(:onhost) ;; hosted cards should not be in :discard or :hand etc
                         :previous-zone (:zone target))]
     (update! state side (update-in card [:hosted] #(conj % c)))
     (when-let [events (:events (card-def target))]
       (when installed
         (register-events state side events c)))
     c)))

(defn runner-install
  ([state side card] (runner-install state side card nil))
  ([state side {:keys [title type cost memoryunits uniqueness ] :as card}
    {:keys [extra-cost no-cost host-card facedown custom-message] :as params}]

   (if-let [hosting (and (not host-card) (:hosting (card-def card)))]
     (resolve-ability state side
                      {:choices hosting
                       :effect (effect (runner-install card (assoc params :host-card target)))} card nil)
     (do
       (trigger-event state side :pre-install card)
       (let [cost (install-cost state side card
                                (concat extra-cost (when (and (not no-cost) (not facedown)) [:credit cost])
                                        (when memoryunits [:memory memoryunits])))]
         (when (and (or (not uniqueness) (not (in-play? state card)) facedown)
                    (if-let [req (:req (card-def card))]
                      (or facedown (req state side card nil)) true)
                    (pay state side card cost))
           (let [c (if host-card
                     (host state side host-card card)
                     (move state side card [:rig (if facedown :facedown (to-keyword type))]))
                 installed-card (card-init state side (assoc c :installed true) (not facedown))]
             (if facedown
               (system-msg state side "installs a card facedown" )
             (if custom-message
               (system-msg state side custom-message)
               (system-msg state side (str "installs " title
                                           (when host-card (str " on " (:title host-card)))
                                           (when no-cost " at no cost")))))
             (trigger-event state side :runner-install installed-card)
             (when (has? c :subtype "Icebreaker") (update-breaker-strength state side c)))))))
   (when (has? card :type "Resource") (swap! state assoc-in [:runner :register :installed-resource] true))
   (swap! state update-in [:bonus] dissoc :install-cost)))

(defn server-list [state card]
  (let [remotes (cons "New remote" (for [i (range (count (get-in @state [:corp :servers :remote])))]
                                     (str "Server " i)))]
    (if (#{"Asset" "Agenda"} (:type card))
        remotes
        (concat ["HQ" "R&D" "Archives"] remotes))))

(defn rez
  ([state side card] (rez state side card nil))
  ([state side card {:keys [no-cost] :as args}]
     (trigger-event state side :pre-rez card)
     (when (or (#{"Asset" "ICE" "Upgrade"} (:type card)) (:install-rezzed (card-def card)))
       (trigger-event state side :pre-rez-cost card)
       (let [cdef (card-def card) cost (rez-cost state side card)]
         (when (or no-cost (pay state side card :credit cost (:additional-cost cdef)))
           (card-init state side (assoc card :rezzed true))
           (doseq [h (:hosted card)]
             (update! state side (-> h
                                     (update-in [:zone] #(map to-keyword %))
                                     (update-in [:host :zone] #(map to-keyword %)))))
           (system-msg state side (str "rez " (:title card) (when no-cost " at no cost")))
           (when (#{"ICE"} (:type card)) (update-ice-strength state side card))
           (trigger-event state side :rez card))))
     (swap! state update-in [:bonus] dissoc :cost)))

(defn corp-install
  ([state side card server] (corp-install state side card server nil))
  ([state side card server {:keys [extra-cost no-install-cost install-state] :as args}]
     (if-not server
       (prompt! state side card (str "Choose a server to install " (:title card))
                (server-list state card) {:effect (effect (corp-install card target args))})
       (do (when (= server "New remote")
             (trigger-event state side :server-created card))
           (let [cdef (card-def card)
                 c (assoc card :advanceable (:advanceable cdef))
                 slot (conj (server->zone state server) (if (= (:type c) "ICE") :ices :content))
                 dest-zone (get-in @state (cons :corp slot))
                 install-cost (if (and (= (:type c) "ICE") (not no-install-cost))
                                (count dest-zone) 0)
                 install-state (or install-state (:install-state cdef))]
             (when (and (not (and (has? c :subtype "Region")
                                  (some #(has? % :subtype "Region") dest-zone)))
                        (pay state side card extra-cost :credit install-cost))
               (when (#{"Asset" "Agenda"} (:type c))
                 (when-let [prev-card (some #(when (#{"Asset" "Agenda"} (:type %)) %) dest-zone)]
                   (system-msg state side (str "trashes " (if (:rezzed prev-card)
                                                            (:title prev-card) "a card") " in " server))
                   (trash state side prev-card {:keep-server-alive true})))
               (let [card-name (if (or (= :rezzed install-state) (= :face-up install-state) (:rezzed c))
                                 (:title card) "a card")]
                 (if (> install-cost 0)
                   (system-msg state side (str "pays " install-cost " [Credits] to install "
                                               card-name " in " server))
                   (system-msg state side (str "installs " card-name " in " server))))
               (let [moved-card (move state side c slot)]
                 (trigger-event state side :corp-install moved-card)
                 (when (= (:type c) "Agenda")
                   (update-advancement-cost state side moved-card))
                 (when (= install-state :rezzed)
                   (rez state side moved-card {:no-cost true}))
                 (when (= install-state :face-up)
                   (card-init state side (assoc (get-card state moved-card) :rezzed true) false)))))))))

(defn play [state side {:keys [card server]}]
  (case (:type card)
    ("Event" "Operation") (play-instant state side card {:extra-cost [:click 1]})
    ("Hardware" "Resource" "Program") (runner-install state side card {:extra-cost [:click 1]})
    ("ICE" "Upgrade" "Asset" "Agenda") (corp-install state side card server {:extra-cost [:click 1]}))
  (trigger-event state side :play card))

(defn derez [state side card]
  (system-msg state side (str "derez " (:title card)))
  (update! state :corp (desactivate state :corp card true))
  (when-let [derez-effect (:derez-effect (card-def card))]
    (resolve-ability state side derez-effect (get-card state card) nil))
  (trigger-event state side :derez card))

(defn advancement-cost-bonus [state side n]
  (swap! state update-in [:bonus :advancement-cost] (fnil #(+ % n) 0)))

(defn advancement-cost [state side {:keys [advancementcost] :as card}]
  (if (nil? advancementcost)
    nil
    (-> (if-let [costfun (:advancement-cost-bonus (card-def card))]
          (+ advancementcost (costfun state side card nil))
          advancementcost)
        (+ (or (get-in @state [:bonus :advancement-cost]) 0))
        (max 0))))

(defn update-all-advancement-costs [state side]
  (doseq [ag (->> (mapcat :content (flatten (seq (get-in @state [:corp :servers]))))
                  (filter #(= (:type %) "Agenda")))]
    (update-advancement-cost state side ag)))

(defn update-advancement-cost [state side agenda]
  (swap! state update-in [:bonus] dissoc :advancement-cost)
  (trigger-event state side :pre-advancement-cost agenda)
  (update! state side (assoc agenda :current-cost (advancement-cost state side agenda))))

(defn advance [state side {:keys [card]}]
  (when (pay state side card :click 1 :credit 1)
    (system-msg state side "advances a card")
    (update-advancement-cost state side card)
    (add-prop state side (get-card state card) :advance-counter 1)))

(defn forfeit [state side card]
  (system-msg state side (str "forfeit " (:title card)))
  (gain state side :agenda-point (- (:agendapoints card)))
  (move state :corp card :rfg))

(defn expose [state side target]
  (system-msg state side (str "exposes " (:title target)))
  (when-let [ability (:expose (card-def target))]
    (resolve-ability state side ability target nil))
  (trigger-event state side :expose target))

(defn prevent-run [state side]
  (swap! state assoc-in [:runner :register :cannot-run] true))

(defn prevent-draw [state side]
  (swap! state assoc-in [:runner :register :cannot-draw] true))

(defn prevent-jack-out [state side]
  (swap! state assoc-in [:run :cannot-jack-out] true))

(defn prevent-steal [state side]
  (swap! state assoc-in [:runner :register :cannot-steal] true))

(defn prevent-current [state side]
  (swap! state assoc-in [:runner :register :cannot-play-current] true))

(defn move-card [state side {:keys [card server]}]
  (let [c (update-in card [:zone] #(map to-keyword %))
        last-zone (last (:zone c))
        label (if (and (not (= last-zone :play-area))
                       (or (and (= (:side c)  "Runner") (not (:facedown c)))
                           (:rezzed c)
                           (:seen c)
                           (= last-zone :deck)))
                (:title c) "a card")
        s (if (#{"HQ" "R&D" "Archives"} server) :corp :runner)]
    (case server
      ("Heap" "Archives")
      (do (trash state s c)
          (system-msg state side (str "trashes " label)))
      ("HQ" "Grip")
      (do (move state s (dissoc c :seen :rezzed) :hand)
          (system-msg state side (str "moves " label " to " server)))
      ("Stack" "R&D")
      (do (move state s (dissoc c :seen :rezzed) :deck {:front true})
          (system-msg state side (str "moves " label " to the top of " server)))
      nil)))

(defn click-run [state side {:keys [server] :as args}]
  (when (and (not (get-in @state [:runner :register :cannot-run])) (pay state :runner nil :click 1))
    (system-msg state :runner (str "makes a run on " server))
    (run state side server)))

(defn click-draw [state side args]
  (when (and (not (get-in @state [side :register :cannot-draw])) (pay state side nil :click 1))
    (system-msg state side "spends [Click] to draw a card")
    (draw state side)
    (trigger-event state side (if (= side :corp) :corp-click-draw :runner-click-draw))))

(defn click-credit [state side args]
  (when (pay state side nil :click 1)
    (system-msg state side "spends [Click] to gain 1 [Credits]")
    (gain state side :credit 1)
    (trigger-event state side (if (= side :corp) :corp-click-credit :runner-click-credit))))

(defn do-purge [state side args]
  (when (pay state side nil :click 3)
    (system-msg state side "purges viruses")
    (purge state side)))

(defn remove-tag [state side args]
  (when (pay state side nil :click 1 :credit 2 :tag 1)
    (system-msg state side "spend [Click] and 2 [Credits] to remove 1 tag")))

(defn jack-out [state side args]
  (end-run state side)
  (system-msg state side "jacks out")
  (trigger-event state side :jack-out))

(defn shuffle-deck [state side {:keys [close] :as args}]
  (swap! state update-in [side :deck] shuffle)
  (if close
    (system-msg state side "stops looking at their deck and shuffles it")
    (system-msg state side "shuffles their deck")))

(defn auto-pump [state side args]
  (let [run (:run @state) card (get-card state (:card args))
        current-ice (when (and run (> (or (:position run) 0) 0)) (get-card state ((:ices run) (dec (:position run)))))
        pumpabi (some #(when (:pump %) %) (:abilities (card-def card)))
        pumpcst (when pumpabi (second (drop-while #(and (not= % :credit) (not= % "credit")) (:cost pumpabi))))
        strdif (when current-ice (max 0 (- (or (:current-strength current-ice) (:strength current-ice))
                         (or (:current-strength card) (:strength card)))))
        pumpnum (when strdif (int (Math/ceil (/ strdif (:pump pumpabi)))))]
    (when (and pumpnum pumpcst (>= (get-in @state [:runner :credit]) (* pumpnum pumpcst)))
      (dotimes [n pumpnum] (resolve-ability state side (dissoc pumpabi :msg) (get-card state card) nil))
      (system-msg state side (str "spends " (* pumpnum pumpcst) " [Credits] to increase the strength of "
                                  (:title card) " to " (:current-strength (get-card state card)))))))

(defn turn-events [state side ev]
  (mapcat #(rest %) (filter #(= ev (first %)) (:turn-events @state))))

(defn first-event [state side ev]
  (empty? (turn-events state side ev)))

(load "cards")
