(ns game.replay
  (:require
    [clojure.string :refer [ends-with? join]]
    [differ.core :as differ]))

(defn typing-log?
  "The typing indicator feature was removed, but games recorded before the
  removal can still have typing system messages in their log."
  [msg]
  (and (= "__system__" (:user msg))
       (= "typing" (:text msg))))

(defn replay-prepare-state
  [state replay-side]
  (-> state
      (assoc :side replay-side
             :replay true)
      (assoc-in [:options :spectatorhands] true)))

(defn replay-init-state-from-history
  [{:keys [history replay-shared]} gameid]
  (let [init-state (first history)
        init-state (assoc init-state :gameid gameid :replay-shared replay-shared)
        init-state (assoc-in init-state [:options :spectatorhands] true)
        diffs (rest history)]
    (assoc init-state :replay-diffs diffs)))

(defn replay-reached-end?
  [replay-status replay-timeline]
  (and (empty? (:diffs replay-status))
       (>= (inc (:n replay-status))
           (count replay-timeline))))

(defn replay-reached-start?
  [replay-status replay-timeline]
  (let [n (:n replay-status)
        d (- (count (get-in replay-timeline [n :diffs]))
             (count (:diffs replay-status)))]
    (and (zero? n) (zero? d))))

(defn replay-apply-patch!
  [{:keys [game-state last-state replay-side]} patch]
  (reset! game-state (replay-prepare-state (differ/patch @last-state patch) @replay-side))
  (reset! last-state @game-state))

(defn replay-jump!
  [{:keys [app-state game-state last-state replay-status replay-timeline replay-side load-notes] :as replay-deps} n]
  (cond
    (neg? n)
    (do
      (swap! app-state assoc :start-shown false)
      (replay-jump! replay-deps 0))

    (< n (count @replay-timeline))
    (do
      (swap! app-state assoc :start-shown true)
      (reset! game-state (replay-prepare-state (get-in @replay-timeline [n :state]) @replay-side))
      (reset! last-state @game-state)
      (swap! replay-status merge {:n n :diffs (get-in @replay-timeline [n :diffs])})
      (load-notes))))

(defn replay-forward!
  [{:keys [app-state replay-status replay-timeline]
    :as replay-deps}]
  (swap! app-state assoc :start-shown true)
  (let [{:keys [n diffs]} @replay-status]
    (if (empty? diffs)
      (when (< (inc n) (count @replay-timeline))
        (replay-jump! replay-deps (inc n))
        (replay-forward! replay-deps))
      (do
        (replay-apply-patch! replay-deps (first diffs))
        (if (empty? (rest diffs))
          (replay-jump! replay-deps (inc n))
          (swap! replay-status assoc :diffs (rest diffs)))))))

(defn replay-jump-to-next-bug!
  [replay-deps]
  (replay-forward! replay-deps)
  (while (not (or (ends-with? (-> @(:game-state replay-deps) :log last :text) "uses a command: /bug")
                  (replay-reached-end? @(:replay-status replay-deps) @(:replay-timeline replay-deps))))
    (replay-forward! replay-deps)))

(defn replay-jump-to!
  [replay-deps {:keys [n d bug]}]
  (if bug
    (do
      (replay-jump! replay-deps 0)
      (dotimes [_ (inc bug)] (replay-jump-to-next-bug! replay-deps)))
    (do
      (replay-jump! replay-deps n)
      (dotimes [_ d] (replay-forward! replay-deps)))))

(defn replay-log-forward!
  [replay-deps]
  (let [prev-log (:log @(:game-state replay-deps))]
    (while (and
             (or (= prev-log (:log @(:game-state replay-deps)))
                 (typing-log? (-> @(:game-state replay-deps) :log last)))
             (not (replay-reached-end? @(:replay-status replay-deps) @(:replay-timeline replay-deps))))
      (replay-forward! replay-deps))))

(defn replay-step-forward!
  [replay-deps]
  (replay-jump! replay-deps (inc (:n @(:replay-status replay-deps)))))

(defn replay-step-backward!
  [replay-deps]
  (replay-jump! replay-deps (dec (:n @(:replay-status replay-deps)))))

(defn replay-backward!
  [replay-deps]
  (let [n (:n @(:replay-status replay-deps))
        d (- (count (get-in @(:replay-timeline replay-deps) [n :diffs]))
             (count (:diffs @(:replay-status replay-deps))))]
    (if (zero? d)
      (when (pos? n)
        (replay-jump-to! replay-deps {:n (dec n) :d 0}))
      (replay-jump-to! replay-deps {:n n :d (dec d)}))))

(defn replay-log-backward!
  [replay-deps]
  (let [prev-log (:log @(:game-state replay-deps))]
    (while (and
             (or (= prev-log (:log @(:game-state replay-deps)))
                 (typing-log? (-> @(:game-state replay-deps) :log last)))
             (not (replay-reached-start? @(:replay-status replay-deps) @(:replay-timeline replay-deps))))
      (replay-backward! replay-deps))))

(defn populate-replay-timeline!
  [{:keys [game-state replay-status replay-timeline replay-side get-remote-annotations]} init-state]
  (let [state (replay-prepare-state (dissoc init-state :replay-diffs) @replay-side)
        diffs (:replay-diffs init-state)]
    (reset! replay-timeline [{:type :start-of-game :state state}])
    (swap! replay-status assoc :annotations {:turns {:corp {} :runner {}}
                                             :clicks {}})
    (swap! replay-status assoc :remote-annotations [])
    (when (not= "local-replay" (:gameid state))
      (get-remote-annotations (:gameid state)))
    (dorun (loop [old-state @game-state
                  diffs diffs
                  inter-diffs []]
             (if (empty? diffs)
               (do
                 (reset! replay-timeline (conj (pop @replay-timeline) (assoc (last @replay-timeline) :diffs inter-diffs)))
                 (swap! replay-timeline conj {:type :end-of-game :state old-state}))
               (let [new-state (differ/patch old-state (first diffs))
                     inter-diffs (conj inter-diffs (first diffs))
                     diffs (rest diffs)
                     old-side (keyword (:active-player old-state))
                     new-side (keyword (:active-player new-state))
                     old-click (get-in old-state [old-side :click])
                     new-click (get-in new-state [new-side :click])
                     diff-log-entries (- (count (:log new-state)) (count (:log old-state)))
                     new-logs (join "\n" (map :text (take-last diff-log-entries (:log new-state))))
                     new-step-type (when (not= old-click new-click)
                                     (cond
                                       (not-empty (filter #(= "Game reset to start of turn" (:msg %)) (get-in new-state [:corp :toast])))
                                       :undo-turn

                                       (and (not= old-side new-side)
                                            (= :corp new-side))
                                       :start-of-turn-corp

                                       (and (not= old-side new-side)
                                            (= :runner new-side))
                                       :start-of-turn-runner

                                       (:run new-state)
                                       :run

                                       (some? (re-find (re-pattern #"spends \[Click\] to install")
                                                       new-logs))
                                       :install

                                       (some? (re-find (re-pattern #"spends \[Click\] and pays \d+ \[Credits\] to install")
                                                       new-logs))
                                       :install

                                       (some? (re-find (re-pattern #"spends \[Click\] to use Corp Basic Action Card to draw 1 card")
                                                       new-logs))
                                       :draw

                                       (some? (re-find (re-pattern #"spends \[Click\] to use Runner Basic Action Card to draw 1 card")
                                                       new-logs))
                                       :draw

                                       (some? (re-find (re-pattern #"spends \[Click\] to use Corp Basic Action Card to gain 1 \[Credits\]")
                                                       new-logs))
                                       :credit

                                       (some? (re-find (re-pattern #"spends \[Click\] to use Runner Basic Action Card to gain 1 \[Credits\]")
                                                       new-logs))
                                       :credit

                                       (some? (re-find (re-pattern #"spends \[Click\] and pays 1 \[Credits\] to use Corp Basic Action Card to advance")
                                                       new-logs))
                                       :advance

                                       (some? (re-find (re-pattern #"spends \[Click\]\[Click\]\[Click\] to use Corp Basic Action Card to purge all virus counters")
                                                       new-logs))
                                       :purge

                                       (some? (re-find (re-pattern #"uses a command: /undo-click")
                                                       new-logs))
                                       :undo-click

                                       :else
                                       :click))]
                 (when new-step-type
                   (reset! replay-timeline (conj (pop @replay-timeline) (assoc (last @replay-timeline) :diffs inter-diffs)))
                   (swap! replay-timeline conj {:type new-step-type :turn (:turn new-state) :state new-state}))

                 (when (:run new-state)
                   (reset! replay-timeline (conj (pop @replay-timeline) (assoc (last @replay-timeline) :type :run))))

                 (if new-step-type
                   (recur new-state diffs [])
                   (recur new-state diffs inter-diffs))))))))
