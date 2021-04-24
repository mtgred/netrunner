(ns nr.gameboard.log
  (:require [cljs.core.async :refer [chan put!]]
            [clojure.string :as string]
            [nr.appstate :refer [app-state]]
            [nr.avatar :refer [avatar]]
            [nr.gameboard.replay :refer [update-notes replay-status get-remote-annotations load-remote-annotations
                                         delete-remote-annotations publish-annotations load-annotations-file save-annotations-file]]
            [nr.gameboard.state :refer [game-state not-spectator?]]
            [nr.help :refer [command-info]]
            [nr.translations :refer [tr]]
            [nr.utils :refer [influence-dot render-message]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(def commands (distinct (map :name command-info)))
(def command-info-map (->> command-info (map (fn [{:keys [name has-args usage help]}] [name {:has-args has-args :usage usage :help help}])) (into {})))

(defonce zoom-channel (chan))

(defonce log-mode (r/atom :log))

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
                                          :stop log-stop-resize}))
         (resize-card-zoom)
         (when (:update @should-scroll)
           (let [n (r/dom-node this)]
             (set! (.-scrollTop n) (.-scrollHeight n)))))

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
                                  :title (string/capitalize (subs (str icon) 1))
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

(defn indicate-action [send]
  (when (not-spectator?)
    [:button.indicate-action {:on-click #(do (.preventDefault %)
                                             (send "indicate-action"))
                              :key "Indicate action"}
     (tr [:game.indicate-action "Indicate action"])]))

(defn fuzzy-match-score
  "Matches if all characters in input appear in target in order.
  Score is sum of matched indices, lower is a better match"
  [input target]
  (loop [curr-input (first input)
         rest-input (rest input)
         target-index (string/index-of target curr-input 0)
         score target-index]
    (when target-index
      (if (not (seq rest-input))
        score
        (let [next-index (string/index-of target (first rest-input) (inc target-index))]
          (recur
            (first rest-input)
            (rest rest-input)
            next-index
            (+ score (or next-index 0))))))))

(defn find-command-matches
  ([input commands]
   (when (= "/" (first input))
     (->> commands
       (map (fn [target] {:match target :score (fuzzy-match-score input target)}))
       (filter :score)
       (sort-by :score)
       (map :match)))))

(defn show-command-menu? [s]
  (seq (:command-matches s)))

(defn reset-command-menu
  "Resets the command menu state."
  [state]
  (do (swap! state assoc :command-matches ())
      (swap! state assoc :command-highlight nil)))

(defn command-menu-key-down-handler
  [state e]
  (when (show-command-menu? @state)
    (let [key-code (-> e .-keyCode)
          matches (:command-matches @state)
          match-count (count matches)]
      (cond
        ;; ArrowDown
        (#{40} key-code) (do (.preventDefault e)
                             (swap! state update :command-highlight #(if % (mod (inc %) match-count) 0)))
        ;; ArrowUp
        (#{38} key-code) (when (:command-highlight @state)
                           (do (.preventDefault e)
                               (swap! state update :command-highlight #(if % (mod (dec %) match-count) 0))))
        ;; Return, Space, ArrowRight, Tab
        (#{13 32 39 9} key-code) (when (or (= 1 match-count) (:command-highlight @state))
                                   (let [use-index (if (= 1 match-count) 0 (:command-highlight @state))
                                         command (nth matches use-index)]
                                     (do (.preventDefault e)
                                         (swap! state assoc :msg (str command " "))
                                         (reset-command-menu state)
                                         ;; auto send when no args needed
                                         (when (and (= key-code 13)
                                                 (not (get-in command-info-map [command :has-args])))
                                           (send-msg state)))))))))

(defn log-input-change-handler
  [s e]
  (do (reset-command-menu s)
      (swap! s assoc :command-matches (-> e .-target .-value (find-command-matches commands)))
      (swap! s assoc :msg (-> e .-target .-value))
      (send-typing s)))

(defn log-input [send]
  (let [gameid (r/cursor game-state [:gameid])
        games (r/cursor app-state [:games])
        !input-ref (r/atom nil)
        s (r/atom {})]
    (fn []
      (let [game (some #(when (= @gameid (str (:gameid %))) %) @games)]
        (when (or (not-spectator?)
                (not (:mutespectators game)))
          [:div.log-input
           [:div.form-container
            [:form {:on-submit #(do (.preventDefault %)
                                    (reset-command-menu s)
                                    (send-msg s))}
             [:input {:placeholder (tr [:chat.placeholder "Say something"])
                      :type "text"
                      :ref (partial reset! !input-ref)
                      :value (:msg @s)
                      :on-key-down (partial command-menu-key-down-handler s)
                      :on-change (partial log-input-change-handler s)}]]]
           [indicate-action send]
           (when (show-command-menu? @s)
             [:div.command-matches-container.panel.blue-shade
              {:on-mouse-leave #(swap! s dissoc :command-highlight)}
              [:ul.command-matches
               (doall (map-indexed
                        (fn [i match]
                          [:li.command-match
                           {:key match
                            :class (when (= i (:command-highlight @s)) "highlight")}
                           [:span {:on-mouse-over #(swap! s assoc :command-highlight i)
                                   :on-click #(do
                                                (swap! s assoc :msg (str match " "))
                                                (reset-command-menu s)
                                                (.focus @!input-ref))}

                            (get-in command-info-map [match :usage])]])
                        (:command-matches @s)))]])])))))

(defn log-panel [send]
  (fn []
    [:div.log
     (when (:replay @game-state)
       [log-selector])
     [log-pane]
     [log-typing]
     [log-input send]]))
