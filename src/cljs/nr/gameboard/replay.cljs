(ns nr.gameboard.replay
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :refer [<! timeout] :as async]
    [clojure.string :as s :refer [blank? capitalize]]
    [game.replay :as replay]
    [nr.ajax :refer [DELETE GET PUT]]
    [nr.appstate :refer [app-state]]
    [nr.gameboard.state :refer [game-state last-state replay-side]]
    [nr.local-storage :as ls]
    [nr.new-game :refer [create-game]]
    [nr.translations :refer [tr tr-span tr-element]]
    [nr.utils :refer [tr-non-game-toast render-message]]
    [nr.ws :as ws]
    [reagent.core :as r]))

(defonce replay-timeline (atom []))
(defonce replay-status (r/atom {:autoplay false :speed 1600}))
(defonce show-replay-link (r/atom false))

(declare load-notes get-remote-annotations)

(defn leave-game! []
  (reset! game-state nil)
  (swap! app-state assoc :gameid nil)
  (swap! app-state dissoc :current-game :start-shown)
  (set! (.-cursor (.-style (.-body js/document))) "default")
  (set! (.-onbeforeunload js/window) nil)
  (-> "#gameboard" js/$ .fadeOut)
  (-> "#gamelobby" js/$ .fadeIn))

(defn replay-deps []
  {:app-state app-state
   :game-state game-state
   :last-state last-state
   :replay-status replay-status
   :replay-timeline replay-timeline
   :replay-side replay-side
   :load-notes load-notes
   :get-remote-annotations get-remote-annotations})

(defn run-replay-step! [replay-fn & args]
  (apply replay-fn (replay-deps) args)
  (reset! ws/lock false))

(defn init-replay [app-state state]
  (ls/save! "gameid" "local-replay")
  (swap! app-state assoc :gameid "local-replay") ;set for main.cljs
  (run-replay-step! replay/populate-replay-timeline! state)
  (if (:replay-jump-to state)
    (run-replay-step! replay/replay-jump-to! (:replay-jump-to state))
    (run-replay-step! replay/replay-jump! 0)))

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
      "ArrowLeft" (cond (.-ctrlKey e) (run-replay-step! replay/replay-step-backward!)
                        (.-shiftKey e) (run-replay-step! replay/replay-backward!)
                        :else (run-replay-step! replay/replay-log-backward!))
      "ArrowRight" (cond (.-ctrlKey e) (run-replay-step! replay/replay-step-forward!)
                         (.-shiftKey e) (run-replay-step! replay/replay-forward!)
                         :else (run-replay-step! replay/replay-log-forward!))
      nil)))

(defn ignore-diff? []
  (let [log (-> @game-state :log last :text)]
    (or (= log "typing")
        (s/includes? log "joined the game"))))

(defn start-replay-game []
  (let [lobby-state (atom {:room "casual"})
        state (atom {:flash-message ""
                     :format "casual"
                     :room "casual"
                     :side "Any Side"
                     :gateway-type "Beginner"
                     :precon "worlds-2012-a"
                     :title "Replay game"})
        options (atom {:allow-spectator true
                       :api-access false
                       :password ""
                       :protected false
                       :save-replay false
                       :singleton false
                       :spectatorhands false
                       :open-decklists false
                       :replay-id (:gameid @game-state)
                       :replay-timestamp @replay-status
                       :timed false
                       :timer nil})]
    (leave-game!)
    (create-game state lobby-state options)))

(defn replay-panel []
  (go (while true
        (while (not (:autoplay @replay-status))
          (<! (timeout 100)))
        (while (and (ignore-diff?) (not (replay/replay-reached-end? @replay-status @replay-timeline)))
          (run-replay-step! replay/replay-forward!))
        (run-replay-step! replay/replay-forward!)
        (if (s/includes? (-> @game-state :log last :text) "ending their turn")
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
                   [:div.step-label {:on-click #(run-replay-step! replay/replay-jump! n)
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
         [:button.small {:on-click #(run-replay-step! replay/replay-step-backward!) :type "button"
                         :title "Rewind one click (Ctrl + ← )"} "⏮︎"]
         [:button.small {:on-click #(run-replay-step! replay/replay-log-backward!) :type "button"
                         :title "Rewind one log entry (←)"} "⏪︎"]
         [:button.small {:on-click #(toggle-play-pause) :type "button"
                         :title (if (:autoplay @replay-status) "Pause (Space)" "Play (Space)")} (if (:autoplay @replay-status) "⏸ " "▶ ")]
         [:button.small {:on-click #(run-replay-step! replay/replay-log-forward!) :type "button"
                         :title "Forward to next log entry (→)"} "⏩︎"]
         [:button.small {:on-click #(run-replay-step! replay/replay-step-forward!) :type "button"
                         :title "Forward one click (Ctrl + → )"} "⏭︎"]]
        (when (and (not= "local-replay" (:gameid @game-state))
                   (:replay-shared @game-state))
          [:div.sharing
           [:input {:style (if @show-replay-link {:display "inline"} {:display "none"})
                    :type "text" :read-only true
                    :value (generate-replay-link (.-origin (.-location js/window)))}]
           [:button {:on-click #(start-replay-game)} [tr-span [:replay_start-new-game "Start new game at this point"]]]
           [:button {:on-click #(swap! show-replay-link not)} [tr-span [:replay_share-timestamp "Share timestamp"]]]])])}))

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
          (tr-non-game-toast  [:log_remote-annotations-fail "Could not get remote annotations."]
                             "error" {:time-out 3 :close-button true})))))

(defn load-remote-annotations [pos]
  (when (< pos (count (:remote-annotations @replay-status)))
    (let [anno (nth (:remote-annotations @replay-status) pos)]
      (swap! replay-status assoc :annotations anno))))

(defn delete-remote-annotations [pos]
  (when (< pos (count (:remote-annotations @replay-status)))
    (let [anno (nth (:remote-annotations @replay-status) pos)]
      (go (let [{:keys [status json]} (<! (DELETE (str "/profile/history/annotations/delete/" (:gameid @game-state) "?date=" (:date anno))))]
            (if (= 200 status)
              (get-remote-annotations (:gameid @game-state))))))))

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
                                     :data-i18n-key :annotations_turn-placeholder
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
                                       :data-i18n-key :annotations_click-placeholder
                                       :on-change #(update-notes)}]]])
(defn notes-shared-pane []
  (let [annotation-options (r/atom {:file ""})]
    [:div.notes-shared
     (when (not= "local-replay" (:gameid @game-state))
       [:div.remote-annotations
        [:h4 [tr-span [:annotations_available-annotations "Available annotations"]] " "
         [:button.small {:type "button"
                         :on-click #(get-remote-annotations (:gameid @game-state))} "⟳"]]
        (if (empty? (:remote-annotations @replay-status))
          [tr-span [:annotations_no-published-annotations "No published annotations."]]
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
                   :on-click #(publish-annotations)}
          [tr-span [:annotations_publish "Publish"]]]]
        [:hr]])
     [tr-element :h4 [:annotations_import-local "Import local annotation file"]]
     [:input {:field :file
              :type :file
              :on-change #(swap! replay-status assoc :annotations-file (aget (.. % -target -files) 0))}]
     [:div.button-row
      [:button {:type "button" :on-click #(load-annotations-file)}
       [tr-span [:annotations_load-local "Load"]]]
      [:button {:type "button" :on-click #(save-annotations-file)}
       [tr-span [:annotations_save-local "Save"]]]
      [:button {:type "button" :on-click #(swap! replay-status assoc :annotations
                                                 {:turns {:corp {} :runner {}}
                                                  :clicks {}})}
       [tr-span [:annotations_clear "Clear"]]]]]))
