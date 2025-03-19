(ns nr.gameboard.replay
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<! timeout] :as async]
   [clojure.string :as s :refer [blank? capitalize ends-with? join]]
   [differ.core :as differ]
   [nr.ajax :refer [DELETE GET PUT]]
   [nr.appstate :refer [app-state]]
   [nr.gameboard.state :refer [game-state last-state replay-side]]
   [nr.translations :refer [tr]]
   [nr.utils :refer [non-game-toast render-message]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(defonce replay-timeline (atom []))
(defonce replay-status (r/atom {:autoplay false :speed 1600}))
(defonce show-replay-link (r/atom false))

(declare replay-jump replay-jump-to populate-replay-timeline)
(defn init-replay [app-state state]
  (.setItem js/localStorage "gameid" "local-replay")
  (swap! app-state assoc :gameid "local-replay") ;set for main.cljs
  (populate-replay-timeline state)
  (if (:replay-jump-to state)
    (replay-jump-to (:replay-jump-to state))
    (replay-jump 0)))

(defn generate-replay-link [origin]
  (let [n (:n @replay-status)
        d (- (count (get-in @replay-timeline [n :diffs]))
             (count (:diffs @replay-status)))]
    (str origin "/replay/" (:gameid @game-state) "?n=" n "&d=" d)))

(defn set-replay-side [side]
  (reset! replay-side side)
  (swap! game-state assoc :side @replay-side)
  (reset! last-state @last-state))

(defn scroll-timeline []
  (when-let [timeline (-> js/document (.getElementById "timeline"))]
    (let [new-step (-> js/document (.getElementsByClassName "active-step") array-seq first)
          new-step-left (+ (.-left (.getBoundingClientRect new-step))
                           (/ (.-width (.getBoundingClientRect new-step)) 2))
          mid (+ (.-left (.getBoundingClientRect timeline))
                 (/ (.-width (.getBoundingClientRect timeline)) 2))
          diff (- mid new-step-left)]
      (set! (.-scrollLeft timeline)
            (- (.-scrollLeft timeline)
               diff)))))

(defn replay-reached-end? []
  (and (empty? (:diffs @replay-status))
       (>= (inc (:n @replay-status))
          (count @replay-timeline))))

(defn replay-prepare-state
  [state]
  (-> state
    (assoc :side @replay-side
           :replay true)
    (assoc-in [:options :spectatorhands] true)))

(defn replay-apply-patch
  [patch]
  (reset! game-state (replay-prepare-state (differ/patch @last-state patch)))
  (reset! ws/lock false)
  (reset! last-state @game-state))

(declare load-notes)
(defn replay-jump [n]
  (cond
    (neg? n)
    (do
      (swap! app-state assoc :start-shown false)
      (replay-jump 0))

    (< n (count @replay-timeline))
    (do
      (swap! app-state assoc :start-shown true)
      (reset! game-state (replay-prepare-state (get-in @replay-timeline [n :state])))
      (reset! ws/lock false)
      (reset! last-state @game-state)
      (swap! replay-status merge {:n n :diffs (get-in @replay-timeline [n :diffs])})
      (load-notes))))

(defn replay-forward []
  (swap! app-state assoc :start-shown true)
  (let [{:keys [n diffs]} @replay-status]
    (if (empty? diffs)
      (do
        (when (< (inc n) (count @replay-timeline))
          (replay-jump (inc n))
          (replay-forward)))
      (do
        (replay-apply-patch (first diffs))
        (if (empty? (rest diffs))
          (replay-jump (inc n))
          (swap! replay-status assoc :diffs (rest diffs)))))))

(defn replay-jump-to-next-bug []
  (replay-forward)
  (while (not (or (ends-with? (-> @game-state :log last :text) "uses a command: /bug")
                  (replay-reached-end?)))
    (replay-forward)))

(defn replay-jump-to [{:keys [n d bug]}]
  (if bug
    (do
      (replay-jump 0)
      (dotimes [i (inc bug)] (replay-jump-to-next-bug)))
    (do
      (replay-jump n)
      (dotimes [i d] (replay-forward)))))

(defn replay-log-forward []
  (let [prev-log (:log @game-state)]
    (while (and
             (or (= prev-log (:log @game-state))
                 (= "typing" (-> @game-state :log last :text)))
             (not (replay-reached-end?)))
      (replay-forward))))

(defn replay-step-forward []
  (replay-jump (inc (:n @replay-status))))

(defn replay-step-backward []
  (replay-jump (dec (:n @replay-status))))

(defn replay-backward []
  (let [n (:n @replay-status)
        d (- (count (get-in @replay-timeline [n :diffs]))
             (count (:diffs @replay-status)))]
    (if (zero? d)
      (when (pos? n)
        (replay-jump-to {:n (dec n) :d 0}))
      (replay-jump-to {:n n :d (dec d)}))))

(defn replay-reached-start? []
  (let [n (:n @replay-status)
        d (- (count (get-in @replay-timeline [n :diffs]))
             (count (:diffs @replay-status)))]
    (and (zero? n) (zero? d))))

(defn replay-log-backward []
  (let [prev-log (:log @game-state)]
    (while (and
             (or (= prev-log (:log @game-state))
                 (= "typing" (-> @game-state :log last :text)))
             (not (replay-reached-start?)))
      (replay-backward))))

(declare get-remote-annotations)
(defn populate-replay-timeline
  [init-state]
  (let [state (replay-prepare-state (dissoc init-state :replay-diffs))
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
                   ; add diffs to last timeline step
                   (reset! replay-timeline (conj (pop @replay-timeline) (assoc (last @replay-timeline) :diffs inter-diffs)))
                   ; create new timeline step
                   (swap! replay-timeline conj {:type new-step-type :turn (:turn new-state) :state new-state}))

                 (when (:run new-state) ; If a card starts a run somewhere during the diffs, change the last step type to :run
                   (reset! replay-timeline (conj (pop @replay-timeline) (assoc (last @replay-timeline) :type :run))))

                 (if new-step-type
                   (recur new-state diffs [])
                   (recur new-state diffs inter-diffs))))))))

(defn toggle-play-pause []
  (swap! replay-status assoc :autoplay (not (:autoplay @replay-status))))

(defn change-replay-speed [v]
  (let [new-step-intervall (min 10000 (max 100 (- (:speed @replay-status) v)))]
    (swap! replay-status assoc :speed new-step-intervall)))

(defn handle-keydown [e]
  (when-not (= "textarea" (.-type (.-activeElement js/document)))
    (case (.-key e)
      " " (toggle-play-pause)
      "+" (change-replay-speed 200)
      "-" (change-replay-speed -200)
      "ArrowLeft" (cond (.-ctrlKey e) (replay-step-backward)
                        (.-shiftKey e) (replay-backward)
                        :else (replay-log-backward))
      "ArrowRight" (cond (.-ctrlKey e) (replay-step-forward)
                         (.-shiftKey e) (replay-forward)
                         :else (replay-log-forward))
      nil)))

(defn ignore-diff? []
  (let [log (-> @game-state :log last :text)]
    (or (= log "typing")
        (s/includes? log "joined the game"))))

(defn replay-panel []
  (go (while true
        (while (not (:autoplay @replay-status))
          (<! (timeout 100)))
        (while (and (ignore-diff?) (not (replay-reached-end?)))
          (replay-forward))
        (replay-forward)
        (if (s/includes? (-> @game-state :log last :text ) "ending their turn")
          (<! (timeout (* 2 (:speed @replay-status))))
          (<! (timeout (:speed @replay-status))))))

  (r/create-class
    {:display-name "replay-panel"

     :component-did-update
     (fn [this]
       (scroll-timeline))

     :component-did-mount
     (fn [this]
       (-> js/document (.addEventListener "keydown" handle-keydown)))

     :component-will-unmount
     (fn [this]
       (-> js/document (.removeEventListener "keydown" handle-keydown)))

     :reagent-render
     (fn []
       [:div.replay.panel.blue-shade
        [:div#timeline
         (doall (for [[n {step-type :type turn :turn state :state :as step}] (map-indexed #(vector %1 %2) @replay-timeline)]
                  ^{:key (str "step-" n)}
                  [:div.step {:class [(:active-player state) (when (= n (:n @replay-status)) "active-step") (name step-type)]}
                   [:div.step-label {:on-click #(replay-jump n)
                                     :data-turn turn
                                     :title (s/replace (capitalize (subs (str step-type) 1)) #"-" " ")
                                     :class (let [annotation (get-in @replay-status [:annotations :clicks (keyword (str n))] nil)]
                                              [(when (= n (:n @replay-status)) "active-step-label")
                                               (when (= :start-of-turn-corp step-type) :annotated-before)
                                               step-type
                                               (when annotation :annotated-after)
                                               (when annotation :notes-icon)
                                               (when annotation (:type annotation))])}
                    (case step-type
                      :start-of-game "↠"
                      :start-of-turn-corp "C"
                      :start-of-turn-runner "R"
                      :end-of-game "🎉"
                      :undo-click "⮌"
                      :undo-turn "⮰"
                      :run "🏃"
                      :install "▼"
                      :draw [:div.symbol]
                      :credit (render-message "[credit]")
                      :advance "A"
                      :purge "🚨"
                      :click (render-message "[click]")
                      "?")]]))]
        [:div.controls
         [:button.small {:on-click #(change-replay-speed -200) :type "button"
                         :title "Decrease Playback speed (-)"} "-"]
         [:button.small {:on-click #(change-replay-speed 200) :type "button"
                         :title "Increase Playback speed (+)"} "+"]
         [:button.small {:on-click #(replay-step-backward) :type "button"
                         :title "Rewind one click (Ctrl + ← )"} "⏮︎"]
         [:button.small {:on-click #(replay-log-backward) :type "button"
                         :title "Rewind one log entry (←)"} "⏪︎"]
         [:button.small {:on-click #(toggle-play-pause) :type "button"
                         :title (if (:autoplay @replay-status) "Pause (Space)" "Play (Space)")} (if (:autoplay @replay-status) "⏸ " "▶ ")]
         [:button.small {:on-click #(replay-log-forward) :type "button"
                         :title "Forward to next log entry (→)"} "⏩︎"]
         [:button.small {:on-click #(replay-step-forward) :type "button"
                         :title "Forward one click (Ctrl + → )"} "⏭︎"]]
        (when-not (= "local-replay" (:gameid @game-state)) ; when saved replay
          [:div.sharing
           [:input {:style (if @show-replay-link {:display "inline"} {:display "none"})
                    :type "text" :read-only true
                    :value (generate-replay-link (.-origin (.-location js/window)))}]
           [:button {:on-click #(swap! show-replay-link not)} "Share timestamp"]])])}))

(defn get-remote-annotations [gameid]
  (go (let [{:keys [status json]} (<! (GET (str "/profile/history/annotations/" gameid)))]
        (if (= 200 status)
          (swap! replay-status assoc :remote-annotations
                 (for [anno json]
                   (assoc anno :deletable
                          (or
                            ; Author of annotations
                            (= (get-in @app-state [:user :username])
                               (:username anno))
                            ; Player in replay
                            (or (= (get-in @app-state [:user :username])
                                   (get-in @game-state [:corp :user :username]))
                                (= (get-in @app-state [:user :username])
                                   (get-in @game-state [:runner :user :username])))))))
          ; Error handling does not work, as GET tries to parse something despite the connection
          ; timing out -- lostgeek (2021/02/14)
          (non-game-toast (tr [:log_remote-annotations-fail "Could not get remote annotations."])
                          "error" {:time-out 3 :close-button true})))))

(defn load-remote-annotations [pos]
  (let [anno (nth (:remote-annotations @replay-status) pos)]
    (swap! replay-status assoc :annotations anno)))

(defn delete-remote-annotations [pos]
  (let [anno (nth (:remote-annotations @replay-status) pos)]
    (go (let [{:keys [status json]} (<! (DELETE (str "/profile/history/annotations/delete/" (:gameid @game-state) "?date=" (:date anno))))]
          (if (= 200 status)
            (get-remote-annotations (:gameid @game-state)))))))

(defn publish-annotations []
  (go (let [{:keys [status json]} (<! (PUT (str "/profile/history/annotations/publish/" (:gameid @game-state))
                                           (assoc (:annotations @replay-status) :date (.getTime (js/Date.)))
                                           :json))]
        (if (= 200 status)
          (get-remote-annotations (:gameid @game-state))))))

(defn load-annotations-file []
  (let [reader (js/FileReader.)
        file (:annotations-file @replay-status)
        onload (fn [onload-ev] (let [annotations (-> onload-ev .-target .-result)
                                     annotations (js->clj (.parse js/JSON annotations) :keywordize-keys true)
                                     annotations (merge {:turns {:corp {} :runner {}}
                                                         :clicks {}}
                                                        (select-keys annotations [:turns :clicks]))]
                                 (swap! replay-status assoc :annotations annotations)))]
    (when file
      (aset reader "onload" onload)
      (.readAsText reader file))))

(defn save-annotations-file []
  (let [annotations (:annotations @replay-status)
        data-blob (js/Blob. #js [(.stringify js/JSON (clj->js annotations))] #js {:type "application/json"})
        link (.createElement js/document "a")]
    (set! (.-href link) (.createObjectURL js/URL data-blob))
    (.setAttribute link "download" "Annotations.json")
    (.appendChild (.-body js/document) link)
    (.click link)
    (.removeChild (.-body js/document) link)))

(defn load-notes []
  (let [turn-notes-elem (-> js/document (.getElementById "notes-turn"))
        click-notes-elem (-> js/document (.getElementById "notes-click"))
        side (keyword (:active-player @game-state))
        ; We have to use keywords instead of numbers as mongo automatically converts them
        turn (keyword (str (:turn @game-state)))
        click (keyword (str (:n @replay-status)))]
    (when turn-notes-elem
      (set! (.-value turn-notes-elem)
            (get-in @replay-status [:annotations :turns side turn :notes] "")))
    (when click-notes-elem
      (set! (.-value click-notes-elem)
            (get-in @replay-status [:annotations :clicks click :notes] "")))
    (swap! replay-status
          assoc :selected-note-type
          (get-in @replay-status [:annotations :clicks click :type] :none))))

(defn update-notes []
  (let [turn-notes-elem (-> js/document (.getElementById "notes-turn"))
        click-notes-elem (-> js/document (.getElementById "notes-click"))
        turn-notes (.-value turn-notes-elem)
        click-notes (.-value click-notes-elem)
        turn (keyword (str (:turn @game-state)))
        click (keyword (str (:n @replay-status)))
        side (keyword (:active-player @game-state))]
    (if (blank? turn-notes)
      (let [new-turns (dissoc (get-in @replay-status [:annotations :turns side]) turn)]
        (swap! replay-status assoc-in [:annotations :turns side] new-turns))
      (swap! replay-status assoc-in [:annotations :turns side turn] {:notes turn-notes}))
    (if (and (blank? click-notes)
             (= :none (:selected-note-type @replay-status)))
      (let [new-annotations (assoc (:annotations @replay-status)
                                   :clicks
                                   (dissoc (:clicks (:annotations @replay-status)) click))]
        (swap! replay-status assoc :annotations new-annotations))
      (swap! replay-status assoc-in [:annotations :clicks click] {:notes click-notes :type (:selected-note-type @replay-status)}))))


(defn notes-pane []
  [:div.notes
   [:div.turn [:textarea#notes-turn {:placeholder (tr [:annotations_turn-placeholder "Notes for this turn"])
                                     :on-change #(update-notes)}]]
   (letfn
     [(create-buttons [types]
        (doall (for [icon types]
                 ^{:key (str "notes-icon-" icon)}
                 [:div {:class ["notes-icon" icon (when (= icon (:selected-note-type @replay-status)) "selected")]
                        :title (capitalize (subs (str icon) 1))
                        :on-click #(do (swap! replay-status assoc :selected-note-type icon)
                                       (update-notes))}])))]
     [:div.notes-icons
      (create-buttons [:none])
      [:div.notes-separator]
      (create-buttons [:blunder :mistake :inaccuracy :good :brilliant])
      [:div.notes-separator]
      (create-buttons [:a :b :c :d])])
   [:div.click [:textarea#notes-click {:placeholder (tr [:annotations_click-placeholder "Notes for this click"])
                                       :on-change #(update-notes)}]]])
(defn notes-shared-pane []
  (let [annotation-options (r/atom {:file ""})]
    [:div.notes-shared
     (when (not= "local-replay" (:gameid @game-state))
       [:div.remote-annotations
        [:h4 (tr [:annotations_available-annotations "Available annotations"]) " "
         [:button.small {:type "button"
                         :on-click #(get-remote-annotations (:gameid @game-state))} "⟳"]]
        (if (empty? (:remote-annotations @replay-status))
          (tr [:annotations_no-published-annotations "No published annotations."])
          [:ul
           (doall
             (for [[n anno] (map-indexed vector (:remote-annotations @replay-status))]
               ^{:key (str "annotation-" n)}
               [:li
                [:a {:on-click #(load-remote-annotations n)} (:username anno)]
                " - " (.toLocaleDateString (js/Date. (:date anno))) " "
                (when (:deletable anno)
                  [:button.small {:type "button"
                                  :on-click #(delete-remote-annotations n)} "X"])]))])
        [:div.button-row
         [:button {:type "button"
                   :on-click #(publish-annotations)} (tr [:annotations_publish "Publish"])]]
        [:hr]])
     [:h4 (tr [:annotations_import-local "Import local annotation file"])]
     [:input {:field :file
              :type :file
              :on-change #(swap! replay-status assoc :annotations-file (aget (.. % -target -files) 0))}]
     [:div.button-row
      [:button {:type "button" :on-click #(load-annotations-file)}
       (tr [:annotations_load-local "Load"])]
      [:button {:type "button" :on-click #(save-annotations-file)}
       (tr [:annotations_save-local "Save"])]
      [:button {:type "button" :on-click #(swap! replay-status assoc :annotations
                                                 {:turns {:corp {} :runner {}}
                                                  :clicks {}})}
       (tr [:annotations_clear "Clear"])]]]))
