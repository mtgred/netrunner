(ns nr.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :as s :refer [capitalize includes? join lower-case split blank?]]
            [differ.core :as differ]
            [game.core.card :refer [has-subtype? asset? rezzed? ice? corp?
                                    faceup? installed? same-card? in-scored?]]
            [jinteki.utils :refer [str->int is-tagged? add-cost-to-label] :as utils]
            [jinteki.cards :refer [all-cards]]
            [nr.ajax :refer [GET PUT DELETE]]
            [nr.appstate :refer [app-state]]
            [nr.auth :as auth]
            [nr.avatar :refer [avatar]]
            [nr.cardbrowser :refer [card-as-text]]
            [nr.end-of-game-stats :refer [build-game-stats]]
            [nr.gameboard.state :refer [game-state lock last-state parse-state]]
            [nr.translations :refer [tr tr-pronouns tr-side]]
            [nr.utils :refer [banned-span influence-dot influence-dots map-longest
                              toastr-options render-icons render-message
                              checkbox-button cond-button get-image-path
                              non-game-toast image-or-face]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(declare stacked-card-view show-distinct-cards)

(defonce board-dom (atom {}))
(defonce sfx-state (atom {}))

(defonce replay-timeline (atom []))
(defonce replay-status (r/atom {:autoplay false :speed 1600}))
(defonce replay-side (r/atom :spectator))
(defonce show-replay-link (r/atom false))

(defonce log-mode (r/atom :log))

(defn- image-url [{:keys [side code title] :as card}]
  (let [lang (get-in @app-state [:options :language] "en")
        res (get-in @app-state [:options :card-resolution] "default")
        special-user (get-in @game-state [(keyword (lower-case side)) :user :special])
        special-wants-art (get-in @game-state [(keyword (lower-case side)) :user :options :show-alt-art])
        viewer-wants-art (get-in @app-state [:options :show-alt-art])
        show-art (and special-user special-wants-art viewer-wants-art)
        art (if show-art
              (get-in @game-state [(keyword (lower-case side)) :user :options :alt-arts (keyword code)] "stock")
              "stock")
        card (if (or (:face card) (:images card)) card (get @all-cards title))
        images (image-or-face card)]
    (get-image-path images (keyword lang) (keyword res) (keyword art))))

(defn generate-replay-link [origin]
  (let [n (:n @replay-status)
        d (- (count (get-in @replay-timeline [n :diffs]))
             (count (:diffs @replay-status)))]
    (str origin "/replay/" (:gameid @game-state) "?n=" n "&d=" d)))

(defn set-replay-side [side]
  (reset! replay-side side)
  (swap! game-state assoc :side @replay-side)
  (reset! last-state @last-state))

(defn get-side [state]
  (if (:replay state)
    @replay-side
    (let [user-id (:_id (:user @app-state))]
      (cond
        (= (get-in state [:runner :user :_id]) user-id) :runner
        (= (get-in state [:corp :user :_id]) user-id) :corp
        :else :spectator))))

(defn not-spectator? []
  (not= :spectator (get-side @game-state)))

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
  (reset! lock false)
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
      (reset! lock false)
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

(defn replay-jump-to [{:keys [n d]}]
  (replay-jump n)
  (dotimes [i d] (replay-forward)))

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
                        (.-shiftKey e) (replay-log-backward)
                        :else (replay-backward))
      "ArrowRight" (cond (.-ctrlKey e) (replay-step-forward)
                         (.-shiftKey e) (replay-log-forward)
                         :else (replay-forward))
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

     :reagent-render
     (fn []
       [:div.replay.panel.blue-shade
        [:div#timeline
         (doall (for [[n {step-type :type turn :turn state :state :as step}] (map-indexed #(vector %1 %2) @replay-timeline)]
                  ^{:key (str "step-" n)}
                  [:div.step {:class [(:active-player state) (when (= n (:n @replay-status)) "active-step") (name step-type)]}
                   [:div.step-label {:on-click #(replay-jump n)
                                     :data-turn turn
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
                      :purge "☣️"
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
                         :title "Rewind one log entry (Shift + ← )"} "⏪︎"]
         [:button.small {:on-click #(toggle-play-pause) :type "button"
                         :title (if (:autoplay @replay-status) "Pause (Space)" "Play (Space)")} (if (:autoplay @replay-status) "⏸ " "▶ ")]
         [:button.small {:on-click #(replay-log-forward) :type "button"
                         :title "Forward to next log entry (Shift + → )"} "⏩︎"]
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
          (non-game-toast (tr [:log.remote-annotations-fail "Could not get remote annotations."])
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

(defn init-game [state]
  (let [side (get-side state)]
    (.setItem js/localStorage "gameid" (:gameid @app-state))
    (reset! log-mode :log)
    (reset! game-state (dissoc state :replay-diffs :replay-jump-to))
    (swap! game-state assoc :side side)
    (reset! last-state @game-state)
    (reset! lock false)
    (when (:replay-diffs state)
      (.setItem js/localStorage "gameid" "local-replay")
      (swap! app-state assoc :gameid "local-replay") ;set for main.cljs
      (populate-replay-timeline state)
      (if (:replay-jump-to state)
        (replay-jump-to (:replay-jump-to state))
        (replay-jump 0)))))

(defn launch-game [state]
  (init-game state)
  (set! (.-onbeforeunload js/window) #(clj->js "Leaving this page will disconnect you from the game."))
  (-> "#gamelobby" js/$ .fadeOut)
  (-> "#gameboard" js/$ .fadeIn))

(declare toast)

(defn notify
  "Send a notification to the chat, and a toast to the current player of the specified severity"
  [text severity]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text}))
  (toast text severity nil))

(defonce zoom-channel (chan))

(defonce button-channel (chan))

(defn check-lock?
  "Check if we can clear client lock based on action-id"
  []
  (let [aid [(:side @game-state) :aid]]
    (when (not= (get-in @game-state aid)
                (get-in @last-state aid))
      (reset! lock false))))

(defn handle-diff [{:keys [gameid diff]}]
  (when (= gameid (:gameid @game-state))
    (swap! game-state #(differ/patch @last-state diff))
    (check-lock?)
    (reset! last-state @game-state)))

(defn handle-timeout [{:keys [gameid]}]
  (when (= gameid (:gameid @game-state))
    (toast (tr [:game.inactivity "Game closed due to inactivity"]) "error" {:time-out 0 :close-button true})))

(ws/register-ws-handler! :netrunner/state #(init-game (parse-state %)))
(ws/register-ws-handler! :netrunner/start #(launch-game (parse-state %)))
(ws/register-ws-handler! :netrunner/diff #(handle-diff (parse-state %)))
(ws/register-ws-handler! :netrunner/timeout #(handle-timeout (parse-state %)))

(defn send-command
  ([command] (send-command command nil))
  ([command {:keys [no-lock card] :as args}]
   (when (and (not (:replay @game-state))
              (or (not @lock) no-lock))
     (let [card (select-keys card [:cid :zone :side :host :type])
           args (merge args (when (seq card) {:card card}))]
       (try (js/ga "send" "event" "game" command) (catch js/Error e))
       (when-not no-lock (reset! lock true))
       (ws/ws-send! [:netrunner/action {:gameid-str (:gameid @game-state)
                                        :command command
                                        :args args}])))))

(defn mute-spectators [mute-state]
  (when (not (:replay @game-state))
    (ws/ws-send! [:netrunner/mute-spectators {:gameid-str (:gameid @game-state)
                                              :mute-state mute-state}])))

(defn stack-servers []
  (swap! app-state update-in [:options :stacked-servers] not))

(defn flip-runner-board []
  (let [layout (if (= "irl" (get-in @app-state [:options :runner-board-order])) "jnet" "irl")]
    (swap! app-state assoc-in [:options :runner-board-order] layout)))

(defn concede []
  (when (not (:replay @game-state))
    (ws/ws-send! [:netrunner/concede {:gameid-str (:gameid @game-state)}])))

(defn build-exception-msg [msg error]
  (letfn [(build-report-url [error]
            (js/escape (str "Please describe the circumstances of your error here.\n\n\nStack Trace:\n```clojure\n"
                            error
                            "\n```")))]
    (str "<div>"
         msg
         "<br/>"
         "<button type=\"button\" class=\"reportbtn\" style=\"margin-top: 5px\" "
         "onclick=\"window.open('https://github.com/mtgred/netrunner/issues/new?body="
         (build-report-url error)
         "');\">Report on GitHub</button></div>")))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr (if (= "exception" type) "error" type))]
    (f (if (= "exception" type) (build-exception-msg msg (:last-error @game-state)) msg))
    (send-command "toast")))

(defn action-list
  [{:keys [type zone rezzed advanceable advance-counter
           advancementcost current-advancement-requirement] :as card}]
  (cond->> []
    ;; advance
    (or (and (= type "Agenda")
             (#{"servers" "onhost"} (first zone)))
        (= advanceable "always")
        (and rezzed
             (= advanceable "while-rezzed"))
        (and (not rezzed)
             (= advanceable "while-unrezzed")))
    (cons "advance")
    ;; score
    (and (= type "Agenda") (>= advance-counter (or current-advancement-requirement advancementcost)))
    (cons "score")
    ;; trash
    (#{"ICE" "Program"} type)
    (cons "trash")
    ;; rez
    (and (#{"Asset" "ICE" "Upgrade"} type)
         (not rezzed))
    (cons "rez")
    ;; derez
    (and (#{"Asset" "ICE" "Upgrade"} type)
         rezzed)
    (cons "derez")))

(defn handle-abilities
  [side {:keys [abilities corp-abilities runner-abilities facedown type] :as card} c-state]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))
        card-side (keyword (.toLowerCase (:side card)))]
    (when-not (and (= card-side :runner) facedown)
      (cond

        ;; Toggle abilities panel
        (or (< 1 c)
            (pos? (+ (count corp-abilities)
                     (count runner-abilities)))
            (some #{"rez" "derez" "advance" "trash"} actions)
            (and (= type "ICE")
                 (not (:run @game-state)))
            (and (corp? card)
                 (not (faceup? card))))
        (do (when (= side card-side)
              (if (:abilities @c-state)
                (swap! c-state dissoc :abilities)
                (swap! c-state assoc :abilities true)))
            (when (and (= :runner card-side)
                       (= :corp side)
                       (:corp-abilities card))
              (if (:corp-abilities @c-state)
                (swap! c-state dissoc :corp-abilities)
                (swap! c-state assoc :corp-abilities true)))
            (when (and (= :corp card-side)
                       (= :runner side)
                       (:runner-abilities card))
              (if (:runner-abilities @c-state)
                (swap! c-state dissoc :runner-abilities)
                (swap! c-state assoc :runner-abilities true))))

        ;; Trigger first (and only) ability / action
        (and (= c 1)
             (= side card-side))
        (if (= (count abilities) 1)
          (send-command "ability" {:card card :ability 0})
          (send-command (first actions) {:card card}))))))

(defn handle-card-click [{:keys [type zone] :as card} c-state]
  (let [side (:side @game-state)]
    (when (not-spectator?)
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt 0 :prompt-type]) "select")
        (send-command "select" {:card card})

        ;; Card is an identity of player's side
        (and (= (:type card) "Identity")
             (= side (keyword (.toLowerCase (:side card)))))
        (handle-abilities side card c-state)

        ;; Runner side
        (= side :runner)
        (case (first zone)
          "hand" (if (:host card)
                   (when (:installed card)
                     (handle-abilities side card c-state))
                   (send-command "play" {:card card}))
          ("current" "onhost" "play-area" "scored" "servers" "rig")
          (handle-abilities side card c-state)
          nil)

        ;; Corp side
        (= side :corp)
        (case (first zone)
          "hand" (case type
                   ("Agenda" "Asset" "ICE" "Upgrade")
                   (if (:servers @c-state)
                     (do (swap! c-state dissoc :servers)
                         (send-command "generate-install-list" nil))
                     (do (swap! c-state assoc :servers true)
                         (send-command "generate-install-list" {:card card})))
                   (send-command "play" {:card card}))
          ("current" "onhost" "play-area" "scored" "servers" "rig")
          (handle-abilities side card c-state)
          nil)))))

(defn in-play? [card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @game-state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn playable? [{:keys [title side zone cost type uniqueness abilities] :as card}]
  (let [my-side (:side @game-state)
        me (my-side @game-state)]
    (and (= (keyword (.toLowerCase side)) my-side)

         (cond

           (has-subtype? card "Double")
           (if (>= (:click me) 2) true false)

           (has-subtype? card "Triple")
           (if (>= (:click me) 3) true false)

           (= (:code card) "07036") ; Day Job
           (if (>= (:click me) 4) true false)

           (has-subtype? card "Priority")
           (if (get-in @game-state [my-side :register :spent-click]) false true)

           :else
           true)

         (and (= zone ["hand"])
              (or (not uniqueness) (not (in-play? card)))
              (or (#{"Agenda" "Asset" "Upgrade" "ICE"} type) (>= (:credit me) cost))
              (pos? (:click me))))))

(defn spectator-view-hidden?
  "Checks if spectators are allowed to see hidden information, such as hands and face-down cards"
  []
  (and (get-in @game-state [:options :spectatorhands])
       (not (not-spectator?))))

(defn- get-card-data-title [e]
  (let [target (.. e -target)
        title (.getAttribute target "data-card-title")]
    (not-empty title)))

(defn card-preview-mouse-over
  [e channel]
  (.preventDefault e)
  (when-let [title (get-card-data-title e)]
    (when-let [card (get (:all-cards-and-flips @app-state) title)]
      (put! channel card)))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when (get-card-data-title e)
    (put! channel false))
  nil)

(defn card-highlight-mouse-over [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel (select-keys value [:cid])))
  nil)

(defn card-highlight-mouse-out [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel false))
  nil)

(defn scrolled-to-end?
  [el tolerance]
  (> tolerance (- (.-scrollHeight el) (.-scrollTop el) (.-clientHeight el))))

(def should-scroll (r/atom {:update true :send-msg false}))

(defn resize-card-zoom []
  "Resizes the card zoom based on the values in the app-state"
  (let [width (get-in @app-state [:options :log-width])
        top (get-in @app-state [:options :log-top])
        max-card-width (- width 5)
        max-card-height (- top 10)
        card-ratio (/ 418 300)]
    (if (> (/ max-card-height max-card-width) card-ratio)
      (-> ".card-zoom" js/$
          (.css "width" max-card-width)
          (.css "height" (int (* max-card-width card-ratio))))
      (-> ".card-zoom" js/$
          (.css "width" (int (/ max-card-height card-ratio)))
          (.css "height" max-card-height)))
    (-> ".rightpane" js/$ (.css "width" width))
    (-> ".log" js/$
        (.css "left" 0)
        (.css "top" top)
        (.css "width" width))))

(defn log-resize [event ui]
  "Resize the card zoom to fit the available space"
  (let [width (.. ui -size -width)
        top (.. ui -position -top)]
    (swap! app-state assoc-in [:options :log-width] width)
    (swap! app-state assoc-in [:options :log-top] top)
    (.setItem js/localStorage "log-width" width)
    (.setItem js/localStorage "log-top" top)
    (resize-card-zoom)))

(defn log-start-resize [event ui]
  "Display a zoomed card when resizing so the user can visualize how the
  resulting zoom will look."
  (when-let [card (get-in @game-state [:runner :identity])]
    (put! zoom-channel card)))

(defn log-stop-resize [event ui]
  (put! zoom-channel false))

(defn log-selector []
  (fn []
    [:div.panel.panel-top.blue-shade.selector
     [:a {:on-click #(reset! log-mode :log)} (tr [:log.game-log "Game Log"])]
     " | "
     [:a {:on-click #(reset! log-mode :notes)} (tr [:log.annotating "Annotating"])]
     " | "
     [:a {:on-click #(reset! log-mode :notes-shared)} (tr [:log.shared "Shared Annotations"])]]))

(defn log-pane []
  (r/create-class
    (let [log (r/cursor game-state [:log])]
      {:display-name "log-pane"

       :component-did-mount
       (fn [this]
         (-> ".log" js/$ (.resizable #js {:handles "w, n, nw"
                                          :resize log-resize
                                          :start log-start-resize
                                          :stop log-stop-resize})))

       :component-will-update
       (fn [this]
         (let [n (r/dom-node this)]
           (reset! should-scroll {:update (or (:send-msg @should-scroll)
                                                  (scrolled-to-end? n 15))
                                  :send-msg false})))

       :component-did-update
       (fn [this]
         (when (:update @should-scroll)
           (let [n (r/dom-node this)]
             (set! (.-scrollTop n) (.-scrollHeight n)))))

       :reagent-render
       (fn []
         [:div.panel.blue-shade.messages {:class [(when (:replay @game-state)
                                                    "panel-bottom")]
                                          :style (when (not (:replay @game-state)) {:top 0})
                                          :on-mouse-over #(card-preview-mouse-over % zoom-channel)
                                          :on-mouse-out #(card-preview-mouse-out % zoom-channel)}
          (case @log-mode
            :log
            (doall (map-indexed
                     (fn [i msg]
                       (when-not (and (= (:user msg) "__system__") (= (:text msg) "typing"))
                         (if (= (:user msg) "__system__")
                           [:div.system {:key i} (render-message (:text msg))]
                           [:div.message {:key i}
                            [avatar (:user msg) {:opts {:size 38}}]
                            [:div.content
                             [:div.username (get-in msg [:user :username])]
                             [:div (render-message (:text msg))]]])))
                     @log))
            :notes
            [:div.notes
             [:div.turn [:textarea#notes-turn {:placeholder (tr [:annotations.turn-placeholder "Notes for this turn"])
                                               :on-change #(update-notes)}]]
             (letfn
               [(create-buttons [types]
                  (doall (for [icon types]
                           ^{:key (str "notes-icon-" icon)}
                           [:div {:class ["notes-icon" icon (when (= icon (:selected-note-type @replay-status)) "selected")]
                                  :on-click #(do (swap! replay-status assoc :selected-note-type icon)
                                                 (update-notes))}])))]
               [:div.notes-icons
                (create-buttons [:none])
                [:div.notes-separator]
                (create-buttons [:blunder :mistake :inaccuracy :good :brilliant])
                [:div.notes-separator]
                (create-buttons [:a :b :c :d])])
             [:div.click [:textarea#notes-click {:placeholder (tr [:annotations.click-placeholder "Notes for this click"])
                                                 :on-change #(update-notes)}]]]
            :notes-shared
             (let [annotation-options (r/atom {:file ""})]
               [:div.notes-shared
                (when (not= "local-replay" (:gameid @game-state))
                  [:div.remote-annotations
                   [:h4 (tr [:annotations.available-annotations "Available annotations"]) " "
                    [:button.small {:type "button"
                                    :on-click #(get-remote-annotations (:gameid @game-state))} "⟳"]]
                   (if (empty? (:remote-annotations @replay-status))
                     (tr [:annotations-no-published-annotations "No published annotations."])
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
                              :on-click #(publish-annotations)} (tr [:log.notes.publish "Publish"])]]
                   [:hr]])
                [:h4 (tr [:annotations.import-local "Import local annotation file"])]
                [:input {:field :file
                         :type :file
                         :on-change #(swap! replay-status assoc :annotations-file (aget (.. % -target -files) 0))}]
                [:div.button-row
                 [:button {:type "button" :on-click #(load-annotations-file)}
                  (tr [:annotations.load-local "Load"])]
                 [:button {:type "button" :on-click #(save-annotations-file)}
                  (tr [:annotations.save-local "Save"])]
                 [:button {:type "button" :on-click #(swap! replay-status assoc :annotations
                                                            {:turns {:corp {} :runner {}}
                                                             :clicks {}})}
                  (tr [:annotations.clear "Clear"])]]]))])})))

(defn log-typing []
  (let [typing (r/cursor game-state [:typing])
        username (get-in @app-state [:user :username])]
    (when (seq (remove nil? (remove #{username} @typing)))
      [:div [:p.typing
             (doall
               (for [i (range 10)]
                 ^{:key i}
                 [:span " " influence-dot " "]))]])))

(defn send-msg [s]
  (let [text (:msg @s)]
    (when (and (not (:replay @game-state))
               (not (empty? text)))
      (reset! should-scroll {:update false :send-msg true})
      (ws/ws-send! [:netrunner/say {:gameid-str (:gameid @game-state)
                                    :msg text}])
      (swap! s assoc :msg ""))))

(defn send-typing [s]
  "Send a typing event to server for this user if it is not already set in game state AND user is not a spectator"
  (let [text (:msg @s)
        username (get-in @app-state [:user :username])]
    (when (and (not (:replay @game-state))
               (not-spectator?))
      (if (empty? text)
        (ws/ws-send! [:netrunner/typing {:gameid-str (:gameid @game-state)
                                        :typing false}])
        (when (not-any? #{username} (:typing @game-state))
          (ws/ws-send! [:netrunner/typing {:gameid-str (:gameid @game-state)
                                          :typing true}]))))))

(defn indicate-action []
  (when (not-spectator?)
    [:button.indicate-action {:on-click #(do (.preventDefault %)
                                             (send-command "indicate-action"))
              :key "Indicate action"}
     (tr [:game.indicate-action "Indicate action"])]))

(defn log-input []
  (let [gameid (r/cursor game-state [:gameid])
        games (r/cursor app-state [:games])
        s (r/atom {})]
    (fn []
      (let [game (some #(when (= @gameid (str (:gameid %))) %) @games)]
        (when (or (not-spectator?)
                  (not (:mutespectators game)))
          [:div.log-input
           [:form {:on-submit #(do (.preventDefault %)
                                   (send-msg s))}
            [:input {:placeholder (tr [:chat.placeholder "Say something"])
                     :type "text"
                     :value (:msg @s)
                     :on-change #(do (swap! s assoc :msg (-> % .-target .-value))
                                     (send-typing s))}]]
           [indicate-action]])))))

(defn handle-dragstart [e card]
  (-> e .-target js/$ (.addClass "dragged"))
  (-> e .-dataTransfer (.setData "card" (.stringify js/JSON (clj->js card)))))

(defn handle-drop [e server]
  (-> e .-target js/$ (.removeClass "dragover"))
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))
        side (if (#{"HQ" "R&D" "Archives"} server) "Corp" "Runner")]
    (send-command "move" {:card card :server server})))

(defn abs [n] (max n (- n)))

;; touch support
(defonce touchmove (atom {}))

(defn release-touch [card]
  (-> card (.removeClass "disable-transition"))
  (-> card (.css "position" ""))
  (-> card (.css "top" "")))

(defn update-card-position [card touch]
  (-> card (.css "left" (str (- (int (aget touch "pageX")) 30) "px")))
  (-> card (.css "top"  (str (- (int (aget touch "pageY")) 42) "px"))))

(defn get-card [e server]
  (-> e .-target js/$ (.closest ".card-wrapper")))

(defn get-server-from-touch [touch]
  (let [cX (.. touch -clientX)
        cY (.. touch -clientY)
        server (-> (js/document.elementFromPoint cX cY)
                   js/$
                   (.closest "[data-server]")
                   (.attr "data-server"))]
    [server (> (+ (abs (- (:x @touchmove) cX))
                  (abs (- (:y @touchmove) cY)))
               30)]))

(defn handle-touchstart [e cursor]
  (let [touch (aget (.. e -targetTouches) 0)
        [server _] (get-server-from-touch touch)
        card (get-card e server)]
    (-> card (.addClass "disable-transition"))
    (reset! touchmove {:card (.stringify js/JSON (clj->js @cursor))
                       :x (.. touch -clientX)
                       :y (.. touch -clientY)
                       :start-server server})))

(defn handle-touchmove [e]
  (let [touch (aget (.. e -targetTouches) 0)
        card (get-card e (:start-server @touchmove))]
    (-> card (.css "position" "fixed"))
    (update-card-position card touch)))

(defn handle-touchend [e]
  (let [touch (aget (.. e -changedTouches) 0)
        card (get-card e (:start-server @touchmove))
        [server moved-enough] (get-server-from-touch touch)]
    (release-touch card)
    (when (and server moved-enough (not= server (:start-server @touchmove)))
      (let [cardinfo (-> @touchmove :card ((.-parse js/JSON)) (js->clj :keywordize-keys true))]
        (send-command "move" {:card cardinfo :server server})))))

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last str->int))

(defn remote->name [server]
  (let [num (remote->num server)]
    (str (tr [:game.server "Server"]) " " num)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (str->int
      (last (clojure.string/split (str zone) #":remote")))))

(defn get-remotes [servers]
  (->> servers
       (filter #(not (#{:hq :rd :archives} (first %))))
       (sort-by #(zone->sort-key (first %)))))

(defn facedown-card
  "Image element of a facedown card"
  ([side] (facedown-card side [] nil))
  ([side class-list alt-alt-text]
   (let [card-back (get-in @app-state [:options :card-back])
         s (lower-case side)
         alt (if (nil? alt-alt-text)
               (str "Facedown " s " card")
               alt-alt-text)
         tag (->> class-list
                  vec
                  (concat ["img" "card"])
                  (join ".")
                  keyword)]
     [tag {:src (str "/img/" card-back "-" s ".png")
           :alt alt}])))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [code title] :as card}]
  (when code
    [:div.card-frame
     [:div.blue-shade.card {:on-mouse-enter #(put! zoom-channel card)
                            :on-mouse-leave #(put! zoom-channel false)}
      (when-let [url (image-url card)]
        [:div
         [:span.cardname title]
         [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]))

(defn face-down?
  "Returns true if the installed card should be drawn face down."
  [{:keys [side type facedown rezzed host] :as card}]
  (if (= side "Corp")
    (and (not= type "Operation")
         (not rezzed)
         (not= (:side host) "Runner"))
    facedown))

(defn card-implementation [zoom-card]
  (when-let [card @zoom-card]
    (let [implemented (:implementation card)]
      (case implemented
        (:full "full") nil
        [:div.panel.blue-shade.implementation {:style {:right (get-in @app-state [:options :log-width])}}
         (case implemented
           nil [:span.unimplemented (tr [:game.unimplemented "Unimplemented"])]
           [:span.impl-msg implemented])]))))

(defn card-zoom-display
  [zoom-card img-side]
  (when-let [card @zoom-card]
    [:<>
     [:div.card-preview.blue-shade
      {:on-click #(reset! img-side (not @img-side))}
      (let [url (image-url card)
            show-img (= "image" (get-in @app-state [:options :card-zoom] "image"))]
        (if (and @img-side url show-img)
          [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}]
          [card-as-text card false]))]
     (when (get-in @app-state [:options :pin-zoom] false)
       [:button.win-right {:on-click #(reset! zoom-card false) :type "button"} "✘"])]))

(defn card-zoom [zoom-card img-side]
  (if @zoom-card
    (do (-> ".card-zoom" js/$ (.addClass "fade"))
        [card-zoom-display zoom-card img-side])
    (do (-> ".card-zoom" js/$ (.removeClass "fade")) nil)))

(defn card-zoom-view [zoom-card]
  (let [zoomed-card (r/atom nil)
        img-side (r/atom true)]
    (fn [zoom-card]
      (let [pin (get-in @app-state [:options :pin-zoom] false)]
        (when (or @zoom-card
                  (and (not @zoom-card) (not pin)))
          (reset! zoomed-card @zoom-card)
          (reset! img-side true))
        [:<>
         [:div.card-zoom
          [card-zoom zoomed-card img-side]]
         [card-implementation zoomed-card]]))))

(defn server-menu
  "The pop-up on a card in hand when clicked"
  [card c-state]
  (let [servers (get-in @game-state [:corp :install-list])]
    (when servers
      [:div.panel.blue-shade.servers-menu {:style (when (:servers @c-state) {:display "inline"})}
       (map-indexed
         (fn [i label]
           [:div {:key i
                  :on-click #(do (send-command "play" {:card card :server label})
                                 (swap! c-state dissoc :servers))}
            label])
         servers)])))

(defn runner-abs [card c-state runner-abilities subroutines title]
  (when (:runner-abilities @c-state)
    [:div.panel.blue-shade.runner-abilities {:style {:display "inline"}}
     (when (or (seq runner-abilities)
               (seq subroutines))
       [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
     (map-indexed
       (fn [i ab]
         [:div {:key i
                :on-click #(send-command "runner-ability" {:card card
                                                           :ability i})}
          (render-icons (add-cost-to-label ab))])
       runner-abilities)
     (when (seq subroutines)
       [:div {:on-click #(send-command "system-msg"
                                       {:msg (str "indicates to fire all unbroken subroutines on " title)})}
        (tr [:game.let-subs-fire "Let all subroutines fire"])])
     (when (seq subroutines)
       [:span.float-center (tr [:game.subs "Subroutines"]) ":"])
     (map-indexed
       (fn [i sub]
         [:span {:style {:display "block"}
                 :key i}
          [:span (cond (:broken sub)
                       {:class :disabled
                        :style {:font-style :italic}}
                       (false? (:resolve sub))
                       {:class :dont-resolve
                        :style {:text-decoration :line-through}})
           (render-icons (str " [Subroutine]" " " (:label sub)))]
          [:span.float-right
           (cond (:broken sub) banned-span
                 (:fired sub) "✅")]])
       subroutines)]))

(defn corp-abs [card c-state corp-abilities]
  (when (:corp-abilities @c-state)
    [:div.panel.blue-shade.corp-abilities {:style {:display "inline"}}
     (when (seq corp-abilities)
       [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
     (map-indexed
       (fn [i ab]
         [:div {:on-click #(send-command "corp-ability" {:card card
                                                         :ability i})}
          (render-icons (add-cost-to-label ab))])
       corp-abilities)]))

;; TODO (2020-10-08): We're using json as the transport layer for server-client
;; communication, so every non-key keyword is converted to a string, which blows.
;; Until this is changed, it's better to redefine this stuff in here and just not
;; worry about it.
(letfn
  [(is-type?  [card value] (= value (:type card)))
   (identity? [card] (or (is-type? card "Fake-Identity")
                         (is-type? card "Identity")))
   (get-nested-host [card] (if (:host card)
                             (recur (:host card))
                             card))
   (get-zone [card] (:zone (get-nested-host card)))
   (in-play-area? [card] (= (get-zone card) ["play-area"]))
   (in-current? [card] (= (get-zone card) ["current"]))
   (in-scored? [card] (= (get-zone card) ["scored"]))
   (corp? [card] (= (:side card) "Corp"))
   (installed? [card] (or (:installed card)
                          (= "servers" (first (get-zone card)))))
   (rezzed? [card] (:rezzed card))
   (runner? [card] (= (:side card) "Runner"))
   (condition-counter? [card] (and (:condition card)
                                   (or (is-type? card "Event")
                                       (is-type? card "Operation"))))
   (facedown? [card] (or (when (not (condition-counter? card))
                           (= (get-zone card) ["rig" "facedown"]))
                         (:facedown card)))]
  (defn active?
    "Checks if the card is active and should receive game events/triggers."
    [card]
    (or (identity? card)
        (in-play-area? card)
        (in-current? card)
        (in-scored? card)
        (and (corp? card)
             (installed? card)
             (rezzed? card))
        (and (runner? card)
             (installed? card)
             (not (facedown? card))))))

(defn card-abilities [card c-state abilities subroutines]
  (let [actions (action-list card)
        dynabi-count (count (filter :dynamic abilities))]
    (when (and (:abilities @c-state)
               (or (pos? (+ (count actions)
                            (count abilities)
                            (count subroutines)))
                   (some #{"derez" "rez" "advance" "trash"} actions)
                   (= type "ICE")))
      [:div.panel.blue-shade.abilities {:style {:display "inline"}}
       (when (seq actions)
         [:span.float-center (tr [:game.actions "Actions"]) ":"])
       (when (seq actions)
         (map-indexed
           (fn [i action]
             [:div {:key i
                    :on-click #(do (send-command action {:card card}))}
              (capitalize action)])
           actions))
       (when (and (active? card)
                  (seq abilities))
         [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
       (when (and (active? card)
                  (seq abilities))
         (map-indexed
           (fn [i ab]
             (if (:dynamic ab)
               [:div {:key i
                      :on-click #(send-command "dynamic-ability" (assoc (select-keys ab [:dynamic :source :index])
                                                                        :card card))}
                (render-icons (add-cost-to-label ab))]
               [:div {:key i
                      :on-click #(send-command "ability" {:card card
                                                          :ability i})}
                (render-icons (add-cost-to-label ab))]))
           abilities))
       (when (seq (remove :fired subroutines))
         [:div {:on-click #(send-command "unbroken-subroutines" {:card card})}
          (tr [:game.fire-unbroken "Fire unbroken subroutines"])])
       (when (seq subroutines)
         [:span.float-center (tr [:game.subs "Subroutines"]) ":"])
       (when (seq subroutines)
         (map-indexed
           (fn [i sub]
             [:div {:key i
                    :on-click #(send-command "subroutine" {:card card
                                                           :subroutine i})}
              [:span (cond (:broken sub)
                           {:class :disabled
                            :style {:font-style :italic}}
                           (false? (:resolve sub))
                           {:class :dont-resolve
                            :style {:text-decoration :line-through}})
               (render-icons (str " [Subroutine] " (:label sub)))]
              [:span.float-right
               (cond (:broken sub) banned-span
                     (:fired sub) "✅")]])
           subroutines))])))

(defn card-view
  [card flipped]
  (let [c-state (r/atom {})]
    (fn [{:keys [zone code type abilities counter advance-counter advancementcost current-advancement-requirement
                 subtype subtypes advanceable rezzed strength current-strength title remotes selected hosted
                 side rec-counter facedown server-target subtype-target icon new runner-abilities subroutines
                 corp-abilities]
          :as card}
         flipped]
      [:div.card-frame
       [:div.blue-shade.card {:class (str (when selected "selected")
                                          (when new " new")
                                          (when (same-card? card (:button @app-state)) " hovered"))
                              :draggable (when (not-spectator?) true)
                              :on-touch-start #(handle-touchstart % card)
                              :on-touch-end   #(handle-touchend %)
                              :on-touch-move  #(handle-touchmove %)
                              :on-drag-start #(handle-dragstart % card)
                              :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                              :on-mouse-enter #(when (or (not (or (not code) flipped facedown))
                                                         (spectator-view-hidden?)
                                                         (= (:side @game-state) (keyword (lower-case side))))
                                                 (put! zoom-channel card))
                              :on-mouse-leave #(put! zoom-channel false)
                              :on-click #(handle-card-click card c-state)}
        (if (or (not code) flipped facedown)
          (let [facedown-but-known (or (not (or (not code) flipped facedown))
                                       (spectator-view-hidden?)
                                       (= (:side @game-state) (keyword (lower-case side))))
                alt-str (if facedown-but-known (str "Facedown " title) nil)]
            [facedown-card side ["bg"] alt-str])
          (when-let [url (image-url card)]
            [:div
             [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]]))
        [:span.cardname title]
        [:div.counters
         (when counter
           (map-indexed (fn [i [type num-counters]]
                          (when (pos? num-counters)
                            (let [selector (str "div.darkbg." (lower-case (name type)) "-counter.counter")]
                              [(keyword selector) {:key type} num-counters])))
                        counter))
         (when (pos? rec-counter) [:div.darkbg.recurring-counter.counter {:key "rec"} rec-counter])
         (when (pos? advance-counter) [:div.darkbg.advance-counter.counter {:key "adv"} advance-counter])]
        (when (and (or current-strength strength)
                   (or (ice? card)
                       (has-subtype? card "Icebreaker"))
                   (active? card))
          [:div.darkbg.strength (or current-strength strength)])
        (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
        (when server-target [:div.darkbg.server-target server-target])
        (when (active? card)
          (let [server-card (get @all-cards title)]
            [:div.darkbg.additional-subtypes
             (join " - " (remove (into #{} (:subtypes server-card)) subtypes))]))

      (when (and (= zone ["hand"])
                 (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
        [server-menu card c-state])

      (when (pos? (+ (count runner-abilities) (count subroutines)))
        [runner-abs card c-state runner-abilities subroutines title])

      (when (pos? (count corp-abilities))
        [corp-abs card c-state corp-abilities])

      [card-abilities card c-state abilities subroutines]]
     (when (pos? (count hosted))
       [:div.hosted
          (let [distinct-hosted (vals (group-by :title hosted))]
            (show-distinct-cards distinct-hosted))])])))

(defn show-distinct-cards
  [distinct-cards]
  (doall (apply concat (for [cards distinct-cards] ; apply concat for one-level flattening
                         (let [hosting (remove #(zero? (count (:hosted %))) cards) ; There are hosted cards on these
                               others (filter #(zero? (count (:hosted %))) cards)
                               facedowns (filter face-down? others)
                               others (remove face-down? others)]
                           [; Hosting
                            (for [c hosting]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c (face-down? c)]])
                            ; Facedown
                            (for [c facedowns]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c true]])
                            ; Rest
                            (if (not-empty others)
                              (if (= 1 (count others))
                                (let [c (first others)
                                      flipped (face-down? c)]
                                  ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                    [card-view c flipped]])
                                [stacked-card-view others]))])))))

(defn stacked-card-view
  [cards]
  [:div.stacked
   (doall
     (for [c cards]
       (let [flipped (face-down? c)]
         ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                           [card-view c flipped]])))])

(defn drop-area [server hmap]
  (merge hmap {:on-drop #(handle-drop % server)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-server server}))

(defn close-popup [event ref msg shuffle? deck?]
  (-> ref js/$ .fadeOut)
  (cond
    shuffle? (send-command "shuffle" {:close "true"})
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn label [cursor opts]
  (let [fn (or (get-in opts [:opts :fn]) count)
        classes (str (when (pos? (count cursor)) "darkbg ")
                     (get-in opts [:opts :classes]))]
    [:div.header {:class classes}
     (str (get-in opts [:opts :name])
          (when (not (get-in opts [:opts :hide-cursor])) (str " (" (fn cursor) ")")))]))

(defn controls
  "Create the control buttons for the side displays."
  ([key] (controls key 1 -1))
  ([key increment decrement]
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
    [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]]))

(defn- this-user?
  [user]
  (if (:replay @game-state)
    (= (get-in @game-state [@replay-side :user :_id]) (:_id user))
    (= (:_id user) (-> @app-state :user :_id))))

(defn build-hand-card-view
  [user hand prompt remotes wrapper-class]
  (let [size (count @hand)]
    [:div
     (doall (map-indexed
              (fn [i card]
                [:div {:key (:cid card)
                       :class (str
                                (if (and (not= "select" (-> @prompt first :prompt-type))
                                         (this-user? @user)
                                         (not (:selected card)) (playable? card))
                                  "playable" "")
                                " "
                                wrapper-class)
                       :style {:left (when (< 1 size) (* (/ 320 (dec size)) i))}}
                 (if (or (this-user? @user)
                         (get-in @game-state [(utils/other-side (get-side @game-state)) :openhand]) ;; TODO: this rebuilds the hand UI on any state change
                         (spectator-view-hidden?))
                   [card-view (assoc card :remotes @remotes)]
                   [facedown-card (:side card)])])
              @hand))]))

(defn hand-view [user name translated-name hand hand-size prompt remotes popup popup-direction]
  (let [s (r/atom {})]
    (fn [user name translated-name hand hand-size prompt remotes popup popup-direction]
      (let [size (count @hand)]
        [:div.hand-container
         [:div.hand-controls
          [:div.panel.blue-shade.hand
           (drop-area name {:class (when (> size 6) "squeeze")})
           [build-hand-card-view user hand prompt remotes "card-wrapper"]
           [label @hand {:opts {:name translated-name
                                :fn (fn [cursor] (str (count cursor) "/" (:total @hand-size)))}}]]
          (when popup
            [:div.panel.blue-shade.hand-expand
             {:on-click #(-> (:hand-popup @s) js/$ .fadeToggle)}
             "+"])]
         (when popup
           [:div.panel.blue-shade.popup {:ref #(swap! s assoc :hand-popup %) :class popup-direction}
            [:div
             [:a {:on-click #(close-popup % (:hand-popup @s) nil false false)} (tr [:game.close "Close"])]
             [:label (tr [:game.card-count] size)]
             (let [{:keys [total]} @hand-size]
               [:div.hand-size (str total " " (tr [:game.max-hand "Max hand size"]))
                (controls :hand-size)])
             [build-hand-card-view user hand prompt remotes "card-popup-wrapper"]]])]))))

(defn show-deck [event ref]
  (-> ((keyword (str ref "-content")) @board-dom) js/$ .fadeIn)
  (-> ((keyword (str ref "-menu")) @board-dom) js/$ .fadeOut)
  (send-command "view-deck"))

(defn identity-view [render-side identity hand-count]
  (let [is-runner (= :runner render-side)
        title (if is-runner (tr [:game.grip "Grip"]) (tr [:game.hq "HQ"]))]
    [:div.blue-shade.identity
     [card-view @identity]
     [:div.header {:class "darkbg server-label"}
      (str title " (" @hand-count ")")]]))

(defn deck-view [render-side player-side identity deck deck-count]
   (let [is-runner (= :runner render-side)
         title (if is-runner (tr [:game.stack "Stack"]) (tr [:game.r&d "R&D"]))
         ref (if is-runner "stack" "rd")
         menu-ref (keyword (str ref "-menu"))
         content-ref (keyword (str ref "-content"))]
     (fn [render-side player-side identity deck]
       [:div.blue-shade.deck
        (drop-area title {:on-click #(-> (menu-ref @board-dom) js/$ .toggle)})
        (when (pos? @deck-count)
          [facedown-card (:side @identity) ["bg"] nil])
        [:div.header {:class "darkbg server-label"}
         (str title " (" @deck-count ")")]
        (when (= render-side player-side)
          [:div.panel.blue-shade.menu {:ref #(swap! board-dom assoc menu-ref %)}
           [:div {:on-click #(do (send-command "shuffle")
                                 (-> (menu-ref @board-dom) js/$ .fadeOut))} (tr [:game.shuffle "Shuffle"])]
           [:div {:on-click #(show-deck % ref)} (tr [:game.show "Show"])]])
        (when (= render-side player-side)
          [:div.panel.blue-shade.popup {:ref #(swap! board-dom assoc content-ref %)}
           [:div
            [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" false true)}
             (tr [:game.close "Close"])]
            [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" true true)}
             (tr [:game.close-shuffle "Close & Shuffle"])]]
           (doall
             (for [card @deck]
               ^{:key (:cid card)}
               [card-view card]))])])))

(defn discard-view-runner [player-side discard]
  (let [s (r/atom {})]
    (fn [player-side discard]
      [:div.blue-shade.discard
       (drop-area "Heap" {:on-click #(-> (:popup @s) js/$ .fadeToggle)})
       (when-not (empty? @discard)
         [card-view (last @discard)])
       [:div.header {:class "darkbg server-label"}
        (str (tr [:game.heap "Heap"]) " (" (count @discard) ")")]
       [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                     :class (if (= player-side :runner) "me" "opponent")}
        [:div
         [:a {:on-click #(close-popup % (:popup @s) nil false false)} (tr [:game.close "Close"])]]
        (doall
          (for [card @discard]
            ^{:key (:cid card)}
            [card-view card]))]])))

(defn discard-view-corp [player-side discard]
  (let [s (r/atom {})]
    (fn [player-side discard]
      (let [faceup? #(or (:seen %) (:rezzed %))
            draw-card #(if (faceup? %)
                         [card-view %]
                         (if (or (= player-side :corp)
                                 (spectator-view-hidden?))
                           [:div.unseen [card-view %]]
                           [facedown-card "corp"]))]
        [:div.blue-shade.discard
         (drop-area "Archives" {:on-click #(-> (:popup @s) js/$ .fadeToggle)})
         (when-not (empty? @discard)
           [:<> {:key "discard"} (draw-card (last @discard))])
         [:div.header {:class "darkbg server-label"}
          (let [total (count @discard)
                face-up (count (filter faceup? @discard))]
            (str (tr [:game.archives "Archives"])
                 ;; use non-breaking space to keep counts on same line
                 " (" (tr [:game.up-down-count] total face-up) ")"))]
         [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                       :class (if (= (:side @game-state) :runner) "opponent" "me")}
          [:div
           [:a {:on-click #(close-popup % (:popup @s) nil false false)} (tr [:game.close "Close"])]
           [:label (let [total (count @discard)
                         face-up (count (filter faceup? @discard))]
                     (tr [:game.face-down-count] total face-up))]]
          (doall
            (for [[idx c] (map-indexed vector @discard)]
              ^{:key idx}
              [:div (draw-card c)]))]]))))

(defn rfg-view [cards name popup]
  (let [dom (atom {})]
    (fn [cards name popup]
      (when-not (empty? @cards)
        (let [size (count @cards)]
          [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")
                                      :on-click (when popup #(-> (:rfg-popup @dom) js/$ .fadeToggle))}
           (doall
             (map-indexed (fn [i card]
                            [:div.card-wrapper {:key i
                                                :style {:left (when (> size 1) (* (/ 128 size) i))}}
                             [:div [card-view card]]])
                          @cards))
           [label @cards {:opts {:name name}}]

           (when popup
             [:div.panel.blue-shade.popup {:ref #(swap! dom assoc :rfg-popup %)
                                           :class "opponent"}
              [:div
               [:a {:on-click #(close-popup % (:rfg-popup @dom) nil false false)} (tr [:game.close "Close"])]
               [:label (tr [:game.card-count] size)]]
              (doall
                (for [c @cards]
                  ^{:key (:cid c)}
                  [card-view c]))])])))))

(defn play-area-view [user name cards]
  (fn [user name cards]
    (let [size (count @cards)]
      (when (pos? size)
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (doall
           (map-indexed (fn [i card]
                          [:div.card-wrapper {:key i
                                              :style {:left (when (> size 1) (* (/ 128 size) i))}}
                           (if (or (:seen card)
                                   (this-user? @user))
                             [card-view card]
                             [facedown-card (:side card)])])
                        @cards))
         [label @cards {:opts {:name name}}]]))))

(defn scored-view [scored agenda-point me?]
  (let [size (count @scored)]
    [:div.panel.blue-shade.scored.squeeze
     (doall
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:key i
                                          :style {:left (when (> size 1) (* (/ 128 (dec size)) i))}}
                       [:div [card-view card]]])
                    @scored))
     [label @scored {:opts {:name (tr [:game.scored-area "Scored Area"])}}]
     [:div.stats
      [:div (tr [:game.agenda-count] @agenda-point)
       (when me? (controls :agenda-point))]]]))

(defn name-area
  [user]
  [:div.namearea [avatar user {:opts {:size 32}}]
   [:div.namebox
    [:div.username (:username user)]
    (if-let [pronouns (get-in user [:options :pronouns])]
      (let [pro-str (if (= "blank" pronouns) "" (tr-pronouns pronouns))]
        [:div.pronouns (lower-case pro-str)]))]])

(defmulti stats-view #(get-in @% [:identity :side]))

(defn display-memory
  [memory]
  (let [me? (= (:side @game-state) :runner)]
    (fn [memory]
      (let [{:keys [available used only-for]} memory
            unused (- available used)]
        [:div (tr [:game.mu-count] unused available)
         (when (neg? unused) [:div.warning "!"])
         (when me? (controls :memory))]))))

(defn display-special-memory
  [memory]
  (when-let [only-for (->> (:only-for memory)
                           (filter #(pos? (:available (second %))))
                           (into {})
                           not-empty)]
    [:div
     (str "("
          (join "), (" (for [[mu-type {:keys [available used]}] only-for
                             :let [unused (max 0 (- available used))]]
                         (str unused " of " available
                              " " (capitalize (name mu-type))
                              " MU unused")))
          ")")]))

(defmethod stats-view "Runner" [runner]
  (let [me? (= (:side @game-state) :runner)]
    (fn [runner]
      (let [{:keys [user click credit run-credit memory link tag
                    brain-damage active]} @runner]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         (name-area user)
         [:div (tr [:game.click-count] click)
          (when me? (controls :click))]
         [:div (tr [:game.credit-count] credit run-credit)
          (when me? (controls :credit))]
         [display-memory memory]
         [display-special-memory memory]
         [:div (str link " " (tr [:game.link-strength "Link Strength"]))
          (when me? (controls :link))]
         (let [{:keys [base total is-tagged]} tag
               additional (- total base)
               show-tagged (or is-tagged (pos? total))]
           [:div (tr [:game.tag-count] base additional total)
            (when show-tagged [:div.warning "!"])
            (when me? (controls :tag))])
         [:div (str brain-damage " " (tr [:game.brain-damage "Brain Damage"]))
          (when me? (controls :brain-damage))]]))))

(defmethod stats-view "Corp" [corp]
  (let [me? (= (:side @game-state) :corp)]
    (fn [corp]
      (let [{:keys [user click credit bad-publicity active]} @corp]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         (name-area user)
         [:div (tr [:game.click-count] click)
          (when me? (controls :click))]
         [:div (tr [:game.credit-count] credit -1)
          (when me? (controls :credit))]
         (let [{:keys [base additional]} bad-publicity]
           [:div (tr [:game.bad-pub-count] base additional)
            (when me? (controls :bad-publicity))])]))))

(defn run-arrow [run]
  [:div.run-arrow [:div {:class (cond
                                  (= "approach-ice" (:phase run))
                                  "approach"
                                  (= "encounter-ice" (:phase run))
                                  "encounter"
                                  :else
                                  "")}]])

(defn server-view [{:keys [server central-view run]} opts]
  (let [content (:content server)
        ices (:ices server)
        run-pos (:position run)
        current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                      (nth ices (dec run-pos)))
        max-hosted (apply max (map #(count (:hosted %)) ices))]
    [:div.server
     [:div.ices {:style {:width (when (pos? max-hosted)
                                  (+ 84 3 (* 42 (dec max-hosted))))}}
      (when-let [run-card (:source-card run)]
        [:div.run-card [card-img run-card]])
      (doall
        (for [ice (reverse ices)]
          [:div.ice {:key (:cid ice)
                     :class (when (not-empty (:hosted ice)) "host")}
           (let [flipped (not (:rezzed ice))]
             [card-view ice flipped])
           (when (and current-ice (= (:cid current-ice) (:cid ice)))
             [run-arrow run])]))
      (when (and run (not current-ice))
        [run-arrow run])]
     [:div.content
      (when central-view
        central-view)
      (when (not-empty content)
        (doall
          (for [card content]
            (let [is-first (= card (first content))
                  flipped (not (:rezzed card))]
              [:div.server-card {:key (:cid card)
                                 :class (str (when central-view "central ")
                                             (when (or central-view
                                                       (and (< 1 (count content)) (not is-first)))
                                               "shift"))}
               [card-view card flipped]]))))
      [label content (update-in opts [:opts] assoc :classes "server-label" :hide-cursor true)]]]))

(defn stacked-label [cursor similar-servers opts]
  (let [similar-server-names (->> similar-servers
                                  (map first)
                                  (map remote->name))
        full-server-names (cons (get-in opts [:opts :name]) similar-server-names)
        numbers (map #(second (split % " ")) full-server-names)]
    [label full-server-names (update-in opts [:opts] assoc
                                        :classes "server-label"
                                        :name (str "Servers " (join ", " numbers))
                                        :hide-cursor true)]))

(defn stacked-view [{:keys [key server similar-servers central-view run]} opts]
  (let [content (apply conj
                       (:content server)
                       ; this unfolds all servers and picks the first item in it
                       ; since this creates a sequence, we need to apply it to conj
                       (map #(-> % second :content first) similar-servers))
        ices (:ices server)
        run-pos (:position run)
        current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                      (nth ices (dec run-pos)))]
    [:div.server
     [:div.ices
      (when-let [run-card (:source-card run)]
        [:div.run-card [card-img run-card]])
      (when (and run (not current-ice))
        [run-arrow run])]
     [:div.content
      (doall (for [card content]
               (let [is-first (= card (first content))
                     flipped (not (:rezzed card))]
                 [:div.server-card {:key (:cid card)
                                    :class (str (when (and (< 1 (count content)) (not is-first))
                                                  "shift"))}
                  [card-view card flipped]])))
      [stacked-label content similar-servers opts]]]))

(defn compare-servers-for-stacking [s1]
  (fn [s2]
    (let [ss1 (second s1)
          ss2 (second s2)]
      (and (= (-> ss1 :content first :normalizedtitle)
              (-> ss2 :content first :normalizedtitle))
           (not= s1 s2)
           (empty? (:ices ss1))
           (empty? (:ices ss2))
           (= 1 (count (:content ss1)))
           (= 1 (count (:content ss2)))
           (-> ss1 :content first asset?)
           (-> ss2 :content first asset?)
           (-> ss1 :content first :rezzed)
           (-> ss2 :content first :rezzed)
           (-> ss1 :content first :hosted empty?)
           (-> ss2 :content first :hosted empty?)))))

(defn board-view-corp [player-side identity deck deck-count hand hand-count discard servers run]
  (let [rs (:server @run)
        server-type (first rs)]
    [:div.corp-board {:class (if (= player-side :runner) "opponent" "me")}
     (doall
       (for [server (reverse (get-remotes @servers))
             :let [num (remote->num (first server))
                   similar-servers (filter #((compare-servers-for-stacking server) %) (get-remotes @servers))
                   all-servers (conj similar-servers server)]
             :when (or (empty? similar-servers)                                     ; it is a normal server-view
                       (not (get-in @app-state [:options :stacked-servers] false))  ; we're not in stacked mode
                       ; otherwise only show one view for the stacked remote
                       (< num (remote->num (first (first similar-servers)))))]
         (if (or (empty? similar-servers)
                 (not (get-in @app-state [:options :stacked-servers] false)))
           [server-view {:key num
                         :server (second server)
                         :run (when (= server-type (str "remote" num)) @run)}
            {:opts {:name (remote->name (first server))}}]
           [stacked-view {:key num
                          :server (second server)
                          :similar-servers similar-servers
                          :run (when
                                 (some #(= server-type (str "remote" %)) (map #(remote->num (first %)) all-servers))
                                 (= server-type (str "remote" num)) @run)}
            {:opts {:name (remote->name (first server))}}])))
     [server-view {:key "hq"
                   :server (:hq @servers)
                   :central-view [identity-view :corp identity hand-count]
                   :run (when (= server-type "hq") @run)}]
     [server-view {:key "rd"
                   :server (:rd @servers)
                   :central-view [deck-view :corp player-side identity deck deck-count]
                   :run (when (= server-type "rd") @run)}]
     [server-view {:key "archives"
                   :server (:archives @servers)
                   :central-view [discard-view-corp player-side discard]
                   :run (when (= server-type "archives") @run)}]]))

(defn board-view-runner [player-side identity deck deck-count hand hand-count discard rig run]
  (let [is-me (= player-side :runner)
        centrals [:div.runner-centrals
                  [discard-view-runner player-side discard]
                  [deck-view :runner player-side identity deck deck-count]
                  [identity-view :runner identity hand-count]]
        runner-f (if (and (not is-me)
                          (= "irl" (get-in @app-state [:options :runner-board-order])))
                   reverse
                   seq)]
    [:div.runner-board {:class (if is-me "me" "opponent")}
     (when-not is-me centrals)
     (doall
       (for [zone (runner-f [:program :hardware :resource :facedown])]
         ^{:key zone}
         [:div
          (let [cards (get @rig zone)
                distinct-cards (vals (group-by :title cards))]
            (show-distinct-cards distinct-cards))]))
     (when is-me centrals)]))

(defn play-sfx
  "Plays a list of sounds one after another."
  [sfx soundbank]
  (when-not (empty? sfx)
    (when-let [sfx-key (keyword (first sfx))]
      (.volume (sfx-key soundbank) (/ (str->int (get-in @app-state [:options :sounds-volume])) 100))
      (.play (sfx-key soundbank)))
    (play-sfx (rest sfx) soundbank)))

(defn update-audio [{:keys [gameid sfx sfx-current-id]} soundbank]
  ;; When it's the first game played with this state or when the sound history comes from different game, we skip the cacophony
  (let [sfx-last-played (:sfx-last-played @sfx-state)]
    (when (and (get-in @app-state [:options :sounds])
               (not (nil? sfx-last-played))
               (= gameid (:gameid sfx-last-played)))
      ;; Skip the SFX from queue with id smaller than the one last played, queue the rest
      (let [sfx-to-play (reduce (fn [sfx-list {:keys [id name]}]
                                  (if (> id (:id sfx-last-played))
                                    (conj sfx-list name)
                                    sfx-list)) [] sfx)]
        (play-sfx sfx-to-play soundbank)))
  ;; Remember the most recent sfx id as last played so we don't repeat it later
  (when sfx-current-id
    (swap! sfx-state assoc :sfx-last-played {:gameid gameid :id sfx-current-id}))))

(defn build-win-box
  "Builds the end of game pop up game end"
  [game-state]
  (let [win-shown (r/atom false)]
    (fn [game-state]
      (when (and (:winner @game-state)
                 (not @win-shown))
        (let [winner (:winner @game-state)
              winning-user (:winning-user @game-state)
              turn (:turn @game-state)
              reason (:reason @game-state)
              time (get-in @game-state [:stats :time :elapsed])]
          [:div.win.centered.blue-shade
           [:div
            winning-user
            " (" (capitalize (tr-side winner)) ") "
            (cond
              (= "Decked" (capitalize reason))
              (tr [:game.win-decked] turn)

              (= "Flatline" (capitalize reason))
              (tr [:game.win-flatlined] turn)

              (= "Concede" (capitalize reason))
              (tr [:game.win-conceded] turn)

              :else
              (tr [:game.win-points] turn))]
           [:div (tr [:game.time-taken] time)]
           [:br]
           [build-game-stats (get-in @game-state [:stats :corp]) (get-in @game-state [:stats :runner])]
           [:button.win-right {:on-click #(reset! win-shown true) :type "button"} "✘"]])))))

(defn build-start-box
  "Builds the start-of-game pop up box"
  [my-ident my-user my-hand my-prompt my-keep op-ident op-user op-keep me-quote op-quote my-side]
  (let [visible-quote (r/atom true)
        mulliganed (r/atom false)
        start-shown (r/cursor app-state [:start-shown])
        card-back (get-in @app-state [:options :card-back])]
    (fn [my-ident my-user my-hand my-prompt my-keep op-ident op-user op-keep me-quote op-quote my-side]
      (when (and (not @start-shown)
                 (:username @op-user)
                 (pos? (count @my-hand)))
        (let [squeeze (< 5 (count @my-hand))]
          [:div.win.centered.blue-shade.start-game
           [:div
            [:div
             [:div.box
              [:div.start-game.ident.column
               {:class (case @my-keep "mulligan" "mulligan-me" "keep" "keep-me" "")}
               (when-let [url (image-url @my-ident)]
                 [:img {:src     url :alt (:title @my-ident) :onLoad #(-> % .-target js/$ .show)
                        :class   (when @visible-quote "selected")
                        :onClick #(reset! visible-quote true)}])]
              [:div.column.contestants
               [:div (:username @my-user)]
               [:div.vs "VS"]
               [:div (:username @op-user)]
               [:div.intro-blurb
                (if @visible-quote
                  (str "\"" @me-quote "\"")
                  (str "\"" @op-quote "\""))]]
              [:div.start-game.ident.column
               {:class (case @op-keep "mulligan" "mulligan-op" "keep" "keep-op" "")}
               (when-let [url (image-url @op-ident)]
                 [:img {:src url
                        :alt (:title @op-ident)
                        :onLoad #(-> % .-target js/$ .show)
                        :class (when-not @visible-quote "selected")
                        :onClick #(reset! visible-quote false)}])]]
             (when (not= :spectator @my-side)
               [:div.start-hand
                [:div {:class (when squeeze "squeeze")}
                 (doall (map-indexed
                          (fn [i {:keys [title] :as card}]
                            [:div.start-card-frame {:style (when squeeze
                                                             {:left (* (/ 610 (dec (count @my-hand))) i)
                                                              :position "absolute"})
                                                    :id (str "startcard" i)
                                                    :key (str (:cid card) "-" i "-" @mulliganed)}
                             [:div.flipper
                              [:div.card-back
                               [:img.start-card {:src (str "/img/" card-back "-" (lower-case (:side @my-ident)) ".png")}]]
                              [:div.card-front
                               (when-let [url (image-url card)]
                                 [:div {:on-mouse-enter #(put! zoom-channel card)
                                        :on-mouse-leave #(put! zoom-channel false)}
                                  [:img.start-card {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]
                             (when-let [elem (.querySelector js/document (str "#startcard" i))]
                               (js/setTimeout #(.add (.-classList elem) "flip") (+ 1000 (* i 300))))])
                          @my-hand))]])
             [:div.mulligan
              (if (or (= :spectator @my-side)
                      (and @my-keep @op-keep))
                [cond-button (if (= :spectator @my-side)
                               (tr [:game.close "Close"]) (tr [:game.start "Start Game"]))
                 true #(swap! app-state assoc :start-shown true)]
                (list ^{:key "keepbtn"} [cond-button (tr [:game.keep "Keep"])
                                         (= "mulligan" (-> @my-prompt first :prompt-type))
                                         #(send-command "choice" {:choice {:uuid (->> (-> @my-prompt first :choices)
                                                                                      (filter (fn [c] (= "Keep" (:value c))))
                                                                                      first
                                                                                      :uuid)}})]
                      ^{:key "mullbtn"} [cond-button (tr [:game.mulligan "Mulligan"])
                                         (= "mulligan" (-> @my-prompt first :prompt-type))
                                         #(do (send-command "choice" {:choice {:uuid (->> (-> @my-prompt first :choices)
                                                                                          (filter (fn [c] (= "Mulligan" (:value c))))
                                                                                          first
                                                                                          :uuid)}})
                                              (reset! mulliganed true))]))]]]
           [:br]
           [:button.win-right {:on-click #(swap! app-state assoc :start-shown true) :type "button"} "✘"]])))))

(defn audio-component [{:keys [sfx] :as cursor}]
    (let [s (r/atom {})
        audio-sfx (fn [name] (list (keyword name)
                                   (new js/Howl (clj->js {:src [(str "/sound/" name ".ogg")
                                                                (str "/sound/" name ".mp3")]}))))
        soundbank (apply hash-map (concat
                                    (audio-sfx "agenda-score")
                                    (audio-sfx "agenda-steal")
                                    (audio-sfx "click-advance")
                                    (audio-sfx "click-card")
                                    (audio-sfx "click-credit")
                                    (audio-sfx "click-run")
                                    (audio-sfx "click-remove-tag")
                                    (audio-sfx "game-end")
                                    (audio-sfx "install-corp")
                                    (audio-sfx "install-runner")
                                    (audio-sfx "play-instant")
                                    (audio-sfx "rez-ice")
                                    (audio-sfx "rez-other")
                                    (audio-sfx "run-successful")
                                    (audio-sfx "run-unsuccessful")
                                    (audio-sfx "virus-purge")))]
        (r/create-class
            {:display-name "audio-component"
             :component-did-update
             (fn []
                 (update-audio (select-keys @game-state [:sfx :sfx-current-id :gameid]) soundbank))
             :reagent-render
             (fn [{:keys [sfx] :as cursor}]
              (let [_ @sfx]))}))) ;; make this component rebuild when sfx changes.

(defn get-run-ices []
  (let [server (-> (:run @game-state)
                   :server
                   first
                   keyword)]
    (get-in @game-state (concat [:corp :servers] [server] [:ices]))))

(defn get-current-ice []
  (let [run-ice (get-run-ices)
        pos (get-in @game-state [:run :position])]
    (when (and pos
               (pos? pos)
               (<= pos (count run-ice)))
      (nth run-ice (dec pos)))))

(def phase->title
  {"initiation" (tr [:game.initiation "Initiation"])
   "approach-ice" (tr [:game.approach-ice "Approach ice"])
   "encounter-ice" (tr [:game.encouter-ice "Encounter ice"])
   "pass-ice" (tr [:game.pass-ice "Pass ice"])
   "approach-server" (tr [:game.approach-server "Approach server"])
   "corp-phase-43" (tr [:game.corp-phase-43 "Corp phase 4.3"])
   "access-server" (tr [:game.access-server "Access server"])})

(defn phase->next-phase-title
  [run]
  (case (:phase @run)
    "initiation" (tr [:game.approach-ice "Approach ice"])
    "approach-ice" (if (rezzed? (get-current-ice))
                     (tr [:game.encouter-ice "Encounter ice"])
                     (if (> (:position @run) 1)
                       (tr [:game.approach-ice "Approach ice"])
                       (tr [:game.approach-server "Approach server"])))
    "encounter-ice" (tr [:game.pass-ice "Pass ice"])
    "pass-ice" (if (zero? (:position @run))
                 (tr [:game.approach-server "Approach server"])
                 (tr [:game.approach-ice "Approach ice"]))
    "approach-server" (tr [:game.access-server "Access server"])
    "corp-phase-43" (tr [:game.access-server "Access server"])
    "access-server" (tr [:game.end-of-run "End of run"])
    ;; Error
    (tr [:game.no-current-run "No current run"])))

(defn corp-run-div
  [run]
  [:div.panel.blue-shade
   [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get phase->title (:phase @run) (tr [:game.unknown-phase "Unknown phase"]))]
   (cond
     (= "approach-ice" (:phase @run))
     (let [current-ice (get-current-ice)]
       [cond-button
        (str (tr [:game.rez "Rez"]) " " (:title current-ice))
        (not (rezzed? current-ice))
        #(send-command "rez" {:card current-ice :press-continue true})])

     (= "encounter-ice" (:phase @run))
     (let [current-ice (get-current-ice)]
       [cond-button
        (tr [:game.fire-unbroken "Fire unbroken subs"])
        (and (seq (remove :fired (:subroutines current-ice)))
             (not (every? :broken (:subroutines current-ice))))
        #(send-command "unbroken-subroutines" {:card current-ice})])

     (= "approach-server" (:phase @run))
     [checkbox-button
      (tr [:game.action-access "Action before access"])
      (tr [:game.action-access "Action before access"])
      (:corp-phase-43 @run)
      #(send-command "corp-phase-43")])

   [cond-button
    (let [next-phase (:next-phase @run)]
      (if (or next-phase (zero? (:position @run)))
        (tr [:game.no-further "No further actions"])
        (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))))
    (and (not= "initiation" (:phase @run))
         (not= "pass-ice" (:phase @run))
         (not= "access-server" (:phase @run))
         (not= "corp" (:no-action @run)))
    #(send-command "continue")]

   (when (and (not= "approach-server" (:phase @run))
              (not= "corp-phase-43" (:phase @run))
              (not= "access-server" (:phase @run)))
     [checkbox-button
      (tr [:game.stop-auto-pass "Stop auto-passing priority"])
      (tr [:game.auto-pass "Auto-pass priority"])
      (:corp-auto-no-action @run)
      #(send-command "toggle-auto-no-action")])])

(defn runner-run-div
  [run]
  (let [phase (:phase @run)
        next-phase (:next-phase @run)]
    [:div.panel.blue-shade
     [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get phase->title phase)]
     (cond
       (:next-phase @run)
       [cond-button
        (phase->title next-phase)
        (and next-phase
             (not (:no-action @run)))
        #(send-command "start-next-phase")]

       (and (not (:next-phase @run))
            (not (zero? (:position @run)))
            (not= "encounter-ice" (:phase @run)))
       [cond-button
        (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))
        (not= "runner" (:no-action @run))
        #(send-command "continue")]

       (zero? (:position @run))
       [cond-button (tr [:game.access-server "Access server"])
        (not= "runner" (:no-action @run))
        #(send-command "continue")])

     (when (= "encounter-ice" (:phase @run))
       (let [current-ice (get-current-ice)
             title (:title current-ice)]
         [cond-button
          (tr [:game.let-subs-fire "Let all subroutines fire"])
          (and (seq (:subroutines current-ice))
               (not (every? #(or (:broken %) (false? (:resolve %))) (:subroutines current-ice))))
          #(send-command "system-msg"
                         {:msg (str "indicates to fire all unbroken subroutines on " title)})]))

     (when (or (= "approach-server" (:phase @run))
               (= "approach-ice" (:phase @run)))
       [cond-button
        (if (:jack-out @run) (tr [:game.jack-out "Jack Out"]) (tr [:game.undo-click "Undo click"]))
        (not (:cannot-jack-out @run))
        (if (:jack-out @run)
          #(send-command "jack-out")
          #(send-msg (r/atom {:msg "/undo-click"})))])

     (when (= "encounter-ice" (:phase @run))
       [cond-button
        (tr [:game.pass-continue "Pass ice and continue"])
        (or (not= "runner" (:no-action @run))
            (:jack-out-after-pass @run))
        #(send-command "continue" {:jack-out false})])

     (when (= "encounter-ice" (:phase @run))
       [cond-button
        (tr [:game.pass-jack "Pass ice and jack out"])
        (and (not (:cannot-jack-out @run))
             (or (not= "runner" (:no-action @run))
                 (not (:jack-out-after-pass @run))))
        #(send-command "continue" {:jack-out true})])]))

(defn run-div
  [side run]
  (if (= side :corp)
    [corp-run-div run]
    [runner-run-div run]))

(defn trace-div
  [prompt]
  [:div
   (when-let [base (:base prompt)]
     ;; This is the initial trace prompt
     (if (nil? (:strength prompt))
       (if (= "corp" (:player prompt))
         ;; This is a trace prompt for the corp, show runner link + credits
         [:div.info (tr [:side.runner "Runner"]) ": " (:link prompt) [:span {:class "anr-icon link"}]
          " + " (:runner-credits prompt) [:span {:class "anr-icon credit"}]]
         ;; Trace in which the runner pays first, showing base trace strength and corp credits
         [:div.info (tr [:game.trace "Trace"]) ": " (if (:bonus prompt) (+ base (:bonus prompt)) base)
          " + " (:corp-credits prompt) [:span {:class "anr-icon credit"}]])
       ;; This is a trace prompt for the responder to the trace, show strength
       (if (= "corp" (:player prompt))
         [:div.info "vs Trace: " (:strength prompt)]
         [:div.info "vs Runner: " (:strength prompt) [:span {:class "anr-icon link"}]])))
   [:div.credit-select
    ;; Inform user of base trace / link and any bonuses
    (when-let [base (:base prompt)]
      (if (nil? (:strength prompt))
        (if (= "corp" (:player prompt))
          (let [strength (if (:bonus prompt) (+ base (:bonus prompt)) base)]
            [:span (str strength " + ")])
          [:span (:link prompt) " " [:span {:class "anr-icon link"}] (str " + " )])
        (if (= "corp" (:player prompt))
          [:span (:link prompt) " " [:span {:class "anr-icon link"}] (str " + " )]
          (let [strength (if (:bonus prompt) (+ base (:bonus prompt)) base)]
            [:span (str strength " + ")]))))
    [:select#credit
     (doall (for [i (range (inc (:choices prompt)))]
              [:option {:value i :key i} i]))] (str " " (tr [:game.credits "credits"]))]
   [:button {:on-click #(send-command "choice"
                                      {:choice (-> "#credit" js/$ .val str->int)})}
    (tr [:game.ok "OK"])]])

(defn button-pane [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 corp runner me opponent] :as cursor}]
  (let [s (r/atom {})
        autocomp (r/track (fn [] (get-in @game-state [side :prompt 0 :choices :autocomplete])))]
    (r/create-class
      {:display-name "button-pane"

       :component-did-update
       (fn []
         (when (pos? (count @autocomp))
           (-> "#card-title" js/$ (.autocomplete (clj->js {"source" @autocomp}))))
         (when (get-in @game-state [side :prompt 0 :show-discard])
           (-> ".me .discard .popup" js/$ .fadeIn))
         (if (= "select" (get-in @game-state [side :prompt 0 :prompt-type]))
           (set! (.-cursor (.-style (.-body js/document))) "url('/img/gold_crosshair.png') 12 12, crosshair")
           (set! (.-cursor (.-style (.-body js/document))) "default"))
         (when (= "card-title" (get-in @game-state [side :prompt 0 :prompt-type]))
           (-> "#card-title" js/$ .focus))
         (doseq [{:keys [msg type options]} (get-in @game-state [side :toast])]
           (toast msg type options)))

    :reagent-render
    (fn [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 corp runner me opponent] :as cursor}]
      [:div.button-pane {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                         :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
       (if-let [prompt (first (:prompt @me))]
         [:div.panel.blue-shade
          (when-let [card (:card prompt)]
            [:div {:style {:text-align "center"}
                   :on-mouse-over #(card-highlight-mouse-over % card button-channel)
                   :on-mouse-out #(card-highlight-mouse-out % card button-channel)}
             (tr [:game.card "Card"]) ": " (render-message (:title card))])
          (when (:card prompt)
            [:hr])
          [:h4 (render-message (:msg prompt))]
          (cond
            ;; number prompt
            (get-in prompt [:choices :number])
            (let [n (get-in prompt [:choices :number])]
              [:div
               [:div.credit-select
                [:select#credit {:default-value (get-in prompt [:choices :default] 0)}
                 (doall (for [i (range (inc n))]
                          [:option {:key i :value i} i]))]]
               [:button {:on-click #(send-command "choice"
                                                  {:choice (-> "#credit" js/$ .val str->int)})}
                (tr [:game.ok "OK"])]])
            ;; trace prompts require their own logic
            (= (:prompt-type prompt) "trace")
            [trace-div prompt]

            ;; choice of number of credits
            (= (:choices prompt) "credit")
            [:div
             [:div.credit-select
              [:select#credit
               (doall (for [i (range (inc (:credit @me)))]
                        [:option {:value i :key i} i]))] (str " " (tr [:game.credits "credits"]))]
             [:button {:on-click #(send-command "choice"
                                                {:choice (-> "#credit" js/$ .val str->int)})}
              (tr [:game.ok "OK"])]]

            ;; auto-complete text box
            (:card-title (:choices prompt))
            [:div
             [:div.credit-select
              [:input#card-title {:placeholder "Enter a card title"
                                  :onKeyUp #(when (= 13 (.-keyCode %))
                                              (-> "#card-submit" js/$ .click)
                                              (.stopPropagation %))}]]
             [:button#card-submit {:on-click #(send-command "choice" {:choice (-> "#card-title" js/$ .val)})}
              (tr [:game.ok "OK"])]]

            ;; choice of specified counters on card
            (:counter (:choices prompt))
            (let [counter-type (keyword (:counter (:choices prompt)))
                  num-counters (get-in prompt [:card :counter counter-type] 0)]
              [:div
               [:div.credit-select
                [:select#credit
                 (doall (for [i (range (inc num-counters))]
                          [:option {:key i :value i} i]))] (str " " (tr [:game.credits "credits"]))]
               [:button {:on-click #(send-command "choice"
                                                  {:choice (-> "#credit" js/$ .val str->int)})}
                (tr [:game.ok "OK"])]])

            ;; otherwise choice of all present choices
            :else
            (doall (for [{:keys [idx uuid value]} (:choices prompt)]
                     (when (not= value "Hide")
                       [:button {:key idx
                                 :on-click #(send-command "choice" {:choice {:uuid uuid}})
                                 :on-mouse-over
                                 #(card-highlight-mouse-over % value button-channel)
                                 :on-mouse-out
                                 #(card-highlight-mouse-out % value button-channel)
                                 :id (:title value)}
                        (render-message (or (not-empty (:title value)) value))]))))]
         (if @run
           [run-div side run]
           [:div.panel.blue-shade
            (if (= (keyword @active-player) side)
              (when (and (not (or @runner-phase-12 @corp-phase-12))
                         (zero? (:click @me))
                         (not @end-turn))
                [:button {:on-click #(send-command "end-turn")} (tr [:game.end-turn "End Turn"])])
              (when @end-turn
                [:button {:on-click #(send-command "start-turn")} (tr [:game.start-turn "Start Turn"])]))
            (when (and (= (keyword @active-player) side)
                       (or @runner-phase-12 @corp-phase-12))
              [:button {:on-click #(send-command "end-phase-12")}
               (if (= side :corp) (tr [:game.mandatory-draw "Mandatory Draw"]) (tr [:game.take-clicks "Take Clicks"]))])
            (when (= side :runner)
              [:div
               [cond-button (tr [:game.remove-tag "Remove Tag"])
                (and (not (or @runner-phase-12 @corp-phase-12))
                     (pos? (:click @me))
                     (>= (:credit @me) 2)
                     (pos? (get-in @me [:tag :base])))
                #(send-command "remove-tag")]
               [:div.run-button
                [cond-button (tr [:game.run "Run"]) (and (not (or @runner-phase-12 @corp-phase-12))
                                        (pos? (:click @me)))
                 #(do (send-command "generate-runnable-zones")
                      (swap! s update :servers not))]
                [:div.panel.blue-shade.servers-menu {:style (when (:servers @s) {:display "inline"})}
                 (let [servers (get-in @game-state [:runner :runnable-list])]
                   (map-indexed (fn [i label]
                                  [:div {:key i
                                         :on-click #(do (send-command "run" {:server label})
                                                        (swap! s update :servers not))}
                                   label])
                                servers))]]])
            (when (= side :corp)
              [cond-button (tr [:game.purge "Purge"])
               (and (not (or @runner-phase-12 @corp-phase-12))
                    (>= (:click @me) 3))
               #(send-command "purge")])
            (when (= side :corp)
              [cond-button (tr [:game.trash-resource "Trash Resource"])
               (and (not (or @runner-phase-12 @corp-phase-12))
                    (pos? (:click @me))
                    (>= (:credit @me) (- 2 (or (:trash-cost-bonus @me) 0)))
                    (is-tagged? game-state))
               #(send-command "trash-resource")])
            [cond-button (tr [:game.draw "Draw"])
             (and (not (or @runner-phase-12 @corp-phase-12))
                  (pos? (:click @me))
                  (not-empty (:deck @me)))
             #(send-command "draw")]
            [cond-button (tr [:game.gain-credit "Gain Credit"])
             (and (not (or @runner-phase-12 @corp-phase-12))
                  (pos? (:click @me)))
             #(send-command "credit")]]))])})))

(defn starting-timestamp [start-date]
  (let [d (js/Date. start-date)]
    [:div.panel.blue-shade
     [:span.float-center
      (str (tr [:game.game-start "Game start"]) ": " (.toLocaleTimeString d))]]))

(defn gameboard []
  (let [active (r/cursor app-state [:active-page])
        start-date (r/cursor game-state [:start-date])
        run (r/cursor game-state [:run])
        side (r/cursor game-state [:side])
        turn (r/cursor game-state [:turn])
        end-turn (r/cursor game-state [:end-turn])
        corp-phase-12 (r/cursor game-state [:corp-phase-12])
        runner-phase-12 (r/cursor game-state [:runner-phase-12])
        corp (r/cursor game-state [:corp])
        runner (r/cursor game-state [:runner])
        active-player (r/cursor game-state [:active-player])
        render-board? (r/track (fn [] (and corp runner side)))
        zoom-card (r/cursor app-state [:zoom])
        background (r/cursor app-state [:options :background])]

    (go (while true
          (let [zoom (<! zoom-channel)]
            (swap! app-state assoc :zoom zoom))))

    (go (while true
          (let [button (<! button-channel)]
            (swap! app-state assoc :button button))))

    (r/create-class
      {:display-name "gameboard"

       :reagent-render
       (fn []
         (when (= "/play" (first @active))
           (when @@render-board?
             (let [me-side (if (= :spectator @side) :corp @side)
                   op-side (utils/other-side me-side)
                   me (r/cursor game-state [me-side])
                   opponent (r/cursor game-state [op-side])
                   ;; hands
                   me-hand (r/cursor game-state [me-side :hand])
                   me-hand-count (r/cursor game-state [me-side :hand-count])
                   op-hand (r/cursor game-state [op-side :hand])
                   op-hand-count (r/cursor game-state [op-side :hand-count])
                   me-hand-size (r/cursor game-state [me-side :hand-size])
                   op-hand-size (r/cursor game-state [op-side :hand-size])
                   ;; decks
                   me-deck (r/cursor game-state [me-side :deck])
                   me-deck-count (r/cursor game-state [me-side :deck-count])
                   op-deck (r/cursor game-state [op-side :deck])
                   op-deck-count (r/cursor game-state [op-side :deck-count])
                   ;; discards
                   me-discard (r/cursor game-state [me-side :discard])
                   op-discard (r/cursor game-state [op-side :discard])
                   ;; user settings
                   me-user (r/cursor game-state [me-side :user])
                   op-user (r/cursor game-state [op-side :user])
                   ;; prompts
                   me-prompt (r/cursor game-state [me-side :prompt])
                   op-prompt (r/cursor game-state [op-side :prompt])
                   ;; identity cards
                   me-ident (r/cursor game-state [me-side :identity])
                   op-ident (r/cursor game-state [op-side :identity])
                   ;; score areas
                   me-scored (r/cursor game-state [me-side :scored])
                   op-scored (r/cursor game-state [op-side :scored])
                   me-agenda-point (r/cursor game-state [me-side :agenda-point])
                   op-agenda-point (r/cursor game-state [op-side :agenda-point])
                   ;; servers
                   corp-servers (r/cursor game-state [:corp :servers])
                   corp-remotes (r/track (fn [] (get-remotes (get-in @game-state [:corp :servers]))))
                   runner-rig (r/cursor game-state [:runner :rig])
                   sfx (r/cursor game-state [:sfx])]
               [:div.gameview
                [:div.gameboard

                 (let [me-keep (r/cursor game-state [me-side :keep])
                       op-keep (r/cursor game-state [op-side :keep])
                       me-quote (r/cursor game-state [me-side :quote])
                       op-quote (r/cursor game-state [op-side :quote])]
                   [build-start-box me-ident me-user me-hand me-prompt me-keep op-ident op-user op-keep me-quote op-quote side])

                 [build-win-box game-state]

                 [:div {:class (if (:replay @game-state)
                                 (case @replay-side
                                   :runner (get-in @game-state [:runner :user :options :background] "lobby-bg")
                                   :corp (get-in @game-state [:corp :user :options :background] "lobby-bg")
                                   :spectator @background)
                                 @background)}]

                 [:div.rightpane
                  [card-zoom-view zoom-card]
                  [:div.log
                   (when (:replay @game-state)
                     [log-selector])
                   [log-pane]
                   [log-typing]
                   [log-input]]]
                 (do (resize-card-zoom) nil)

                 [:div.centralpane
                  (if (= op-side :corp)
                    [board-view-corp   me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard corp-servers run]
                    [board-view-runner me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard runner-rig run])
                  (if (= me-side :corp)
                    [board-view-corp   me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard corp-servers run]
                    [board-view-runner me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard runner-rig run])]

                 [:div.leftpane [:div.opponent
                                 (let [srv (if (= :corp op-side) "HQ" "Grip")
                                       translated-srv (if (= :corp op-side) (tr [:game.hq "HQ"]) (tr [:game.grip "Grip"]))]
                                   [hand-view op-user srv translated-srv op-hand op-hand-size op-prompt corp-remotes
                                    (= @side :spectator) "opponent"])]

                  [:div.inner-leftpane
                   [audio-component {:sfx sfx}]

                   [:div.left-inner-leftpane
                    [:div
                     [stats-view opponent]
                     [scored-view op-scored op-agenda-point false]]
                    [:div
                     [scored-view me-scored me-agenda-point true]
                     [stats-view me]]]

                   [:div.right-inner-leftpane
                    (let [op-rfg (r/cursor game-state [op-side :rfg])
                          op-current (r/cursor game-state [op-side :current])
                          op-play-area (r/cursor game-state [op-side :play-area])
                          me-rfg (r/cursor game-state [me-side :rfg])
                          me-current (r/cursor game-state [me-side :current])
                          me-play-area (r/cursor game-state [me-side :play-area])]
                      [:div
                       [starting-timestamp @start-date]
                       [rfg-view op-rfg (tr [:game.rfg "Removed from the game"]) true]
                       [rfg-view me-rfg (tr [:game.rfg "Removed from the game"]) true]
                       [play-area-view op-user (tr [:game.play-area "Play Area"]) op-play-area]
                       [play-area-view me-user (tr [:game.play-area "Play Area"]) me-play-area]
                       [rfg-view op-current (tr [:game.current "Current"]) false]
                       [rfg-view me-current (tr [:game.current "Current"]) false]])
                    (when-not (= @side :spectator)
                      [button-pane {:side me-side :active-player active-player :run run :end-turn end-turn
                                    :runner-phase-12 runner-phase-12 :corp-phase-12 corp-phase-12
                                    :corp corp :runner runner :me me :opponent opponent}])]]

                  [:div.me
                   (let [srv (if (= :corp me-side) "HQ" "Grip")
                         translated-srv (if (= :corp me-side) (tr [:game.hq "HQ"]) (tr [:game.grip "Grip"]))]
                     [hand-view me-user srv translated-srv me-hand me-hand-size me-prompt
                      corp-remotes true "me"])]]]
                (when (:replay @game-state)
                  [:div.bottompane
                   [replay-panel]])]))))})))
