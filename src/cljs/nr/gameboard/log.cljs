(ns nr.gameboard.log
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
            [nr.gameboard.state :refer [game-state]]
            [nr.translations :refer [tr tr-pronouns tr-side]]
            [nr.utils :refer [banned-span influence-dot influence-dots map-longest
                              toastr-options render-icons render-message
                              checkbox-button cond-button get-image-path
                              non-game-toast image-or-face]]
            [nr.ws :as ws]
            [reagent.core :as r]))

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

(defn log-panel []
  (fn []
    [:div.log
     (when (:replay @game-state)
       [log-selector])
     [log-pane]
     [log-typing]
     [log-input]]))
