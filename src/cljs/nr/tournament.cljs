(ns nr.tournament
  (:require
   [nr.appstate :refer [app-state]]
   [nr.ws :as ws]
   [nr.avatar :refer [avatar]]
   [nr.stats :refer [faction-icon-memo]]
   [jinteki.utils :refer [slugify str->int]]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.duration :as duration]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [nr.utils :refer [cond-button non-game-toast]]
   ["react" :as react]
   [reagent.core :as r]
   [clojure.string :as str]))

;; STATE MANAGEMENT

(defonce active-round
  ^{:doc "State of any active tournament timer from the appstate (remote)"}
  (r/atom nil))

(defonce stored-tables
  ^{:doc "State of all competitive lobbies as of last load"}
  (r/atom []))

(defonce action-summary
  ^{:doc "State of all tables that do not follow normal settings - ie have a time extension, or are excluded"}
  (r/atom []))

(defonce tournament-state
  ^{:doc "State of the timer to be created"}
  (r/atom {:reporting {:self-reporting false
                       :self-reporting-url "https://tournaments.nullsignal.games/"}
           :round-start {:alert true
                         :one-minute-warning true
                         :start-in 5}
           :round {:time-in-round 40
                   :time-expiry-text "TIME IN ROUND"
                   :time-expiry-rules-text "Time has been called. The active player finishes their turn, then the opposing player takes a turn. If the game has not concluded by the end of that turn, then the game is decided on agenda points."
                   :explain-time-resolution true
                   :twenty-minute-warning false
                   :five-minute-warning true
                   :one-minute-warning false}}))

;; SOME HELPER FUNCTIONS FOR THE INPUTS

(defn- check-helper
  [state keyseq label]
  [:div [:label [:input {:type "checkbox" :value true :checked (get-in @state (vec keyseq) true)
                         :on-change #(let [checked (.. % -target -checked)]
                                       (swap! state assoc-in (vec keyseq) checked))}]
         label]])

(defn- minutes-helper
  [state keyseq label-pre label-post]
  [:div [:label
         label-pre
         [:input {:type "number" :min 0 :step 1 :style {:width "10ch"}
                  :value (get-in @state (vec keyseq) 5)
                  :on-change #(swap! state assoc-in (vec keyseq) (.. % -target -valueAsNumber))}]
         label-post]])

(defn- dividerlabel
  [text]
  [:div {:style {:display "flex" :align-items "center" :margin "12px 0"}}
   [:div {:style {:flex "1" :height "1px" :background "#ccc"}}]
   [:span {:style {:margin "0 8px"}} text]
   [:div {:style {:flex "1" :height "1px" :background "#ccc"}}]])

(defn- text-helper
  [state keyseq label]
  [:div [:label label
         [:input {:type "text" :style {:width "100%"}
                  :value (get-in @state (vec keyseq) "placeholder")
                  :on-change #(swap! state assoc-in (vec keyseq) (.. % -target -value))}]]])

;; FORMATTING/BODY

(defn timer-management
  []
  [:div
   [:h3 "Set up a round"]
   [:form
    ;; print a message that the round is starting
    [:fieldset
     [:legend "Reporting"]
     [check-helper tournament-state [:reporting :self-reporting] "Encourage self-reporting?"]
     [text-helper tournament-state [:reporting :self-reporting-url] "Link for reporting: "]]
    [:fieldset
     [:legend "Round start"]
     [check-helper tournament-state [:round-start :alert] "Inform players about round start?"]
     [check-helper tournament-state [:round-start :one-minute-warning] "Inform players 1 minute before round start?"]
     [minutes-helper tournament-state [:round-start :start-in] "Round starts in " " minutes"]]
    [:fieldset
     [:legend "Round properties"]
     [minutes-helper tournament-state [:round :time-in-round] "Round is " " minutes long"]
     [text-helper tournament-state [:round :time-expiry-text] "Time call: "]
     [check-helper tournament-state [:round :explain-time-resolution] "Explain resolution when calling time in round (text below)?"]
     [text-helper tournament-state [:round :time-expiry-rules-text] "Time rules explainer: "]
     [dividerlabel "Warnings"]
     [check-helper tournament-state [:round :twenty-minute-warning] "Give players a 20-minute warning (half-way for 40 minute rounds)"]
     [check-helper tournament-state [:round :five-minute-warning] "Give players a 5-minute warning"]
     [check-helper tournament-state [:round :one-minute-warning] "Give players a 1-minute warning"]]]
   [:p]
   [cond-button "Declare Round"
    (not @active-round)
    (fn []
      (non-game-toast "locking in a round structure..." "info" nil)
      (ws/ws-send! [:tournament/declare-round {:tournament-settings @tournament-state}]))]
   [cond-button "Conclude Round"
    @active-round
    (fn []
      (when (js/confirm "Are you sure you want to conclude the round? This CANNOT be undone")
        (non-game-toast "attempting to conclude round..." "info" nil)
        (ws/ws-send! [:tournament/conclude-round {}])))]
   [:p]])

(defn tournament-lobbies-container
  []
  [:div
   [:h3 "Tournament lobbies"]
   [:div "Here you can load all lobbies in the competitive channel, set time extensions on specific lobbies, and exclude lobbies from timers and announcements."]
   [:p]
   [:div [:button
          {:type "button"
           :on-click (fn []
                       (ws/ws-send! [:tournament/view-tables {}])
                       (non-game-toast "refreshing tables..." "info" nil))}
          "Refresh State"]
    [:button
     {:type "button"
      :on-click (fn []
                  (do (ws/ws-send! [:tournament/update-tables {:competitive-lobbies @stored-tables}])
                      (non-game-toast "locking in changes..." "info" nil)))}
     "Commit Changes"]]
   [:p]

   ;; table of all active competitive lobbies
   [:table {:style {:border-collapse "collapse"
                    :text-align "center"
                    :width "100%"}}
    [:thead
     [:tr
      (for [h ["Lobby name" "Corporation" "Runner" "Excluded?" "Time ext (min)"]]
        ^{:key h} [:th {:style {:border "1px solid #ccc" :padding "4px"}} h])]]

    [:tbody
     (doall
       (for [{:keys [gameid title corp runner excluded? time-extension] :as row} @stored-tables
             :let [idx (.indexOf @stored-tables row)]]
         ^{:key gameid}
         [:tr
          [:td title]
          [:td corp]
          [:td runner]
          [:td [:input {:type "checkbox"
                        :checked (if excluded? true false)
                        :on-change #(swap! stored-tables update-in [idx :excluded?] not)}]]
          [:td [:input {:type "number"
                        :min 0
                        :step 1
                        :value (or time-extension 0)
                        :on-change #(let [v (.. % -target -valueAsNumber)]
                                      (swap! stored-tables assoc-in [idx :time-extension] v))
                        :style {:width "10ch"}}]]]))]]])

(defn- split-players [competitive-lobbies]
  (mapv (fn [lobby]
          (let [corp   (first (filter #(= (:side % nil) "Corp")   (:players lobby)))
                runner (first (filter #(= (:side % nil) "Runner") (:players lobby)))]
            (-> lobby
                (assoc :corp (:uid corp "-") :runner (:uid runner "-"))
                (dissoc :players))))
        competitive-lobbies))

(defn- time-until
  "Helper method for game-time. Computes how many minutes since game start"
  [end]
  (let [now (inst/now)
        diff (duration/between now end)
        total-seconds (duration/get diff chrono/seconds)
        minutes (quot total-seconds 60)
        seconds (abs (rem total-seconds 60))]
    [minutes seconds]))

(defn countdown [target-time]
  (let [remaining (r/atom nil)
        interval (r/atom nil)]
    (r/create-class
      {:component-did-mount
       (fn []
         (reset! interval
                 ;; Update timer at most every 1 sec
                 (js/setInterval #(reset! remaining (time-until target-time)) 1000)))
       :component-will-unmount
       (fn []
         (js/clearInterval @interval)
         (reset! interval nil))
       :reagent-render
       (fn []
         [:span {:style (when (<= (first @remaining) 0) {:color "red"})}
          (str (first @remaining) " minutes and " (second @remaining) " seconds remaining until the round ends")])})))

(defn active-round-section []
  [:div
   [:h3 "Active Round"]
   (if-not @active-round
     [:div "There is no currently active round. Set one up below."]
     [:div {:style {:display "flex"
                    :flex-direction "column"
                    :gap "6px"}}
      [:ul
       [:li (str (:source-uid @active-round) " declared the round")]
       [:li [countdown (:round-end @active-round)]]
       (when (:round-20m-warning @active-round) [:li "There is a 20m warning"])
       (when (:round-5m-warning @active-round) [:li "There is a 5m warning"])
       (when (:round-1m-warning @active-round) [:li "There is a 1m warning"])
       (when-let [u (:report-match @active-round)] [:li (str "Players will be asked to report at: " u)])]])
   [:div [:button
          {:type "button"
           ;; TODO - sent a thing to refresh the tournament lobbies
           :on-click (fn []
                       (ws/ws-send! [:tournament/view-tables {}])
                       (non-game-toast "refreshing tables..." "info" nil))}
          "Refresh State"]]

   [:p]])

(defn tournament []
  (r/with-let [user (r/cursor app-state [:user])]
    ;; this is bad practice I think? I will fix it later
    [:div.container
     [:div.about-bg]
     (when (:tournament-organizer @user)
       [:div.container.panel.blue-shade.content-page
        [:h1 "Tournament Manager"]
        [:hr] [active-round-section]

        ;; TODO - actions taken

        [:hr] [timer-management]
        [:hr] [tournament-lobbies-container]])]))


;; ws handlers

(defmethod ws/event-msg-handler :tournament/view-tables [{{:keys [competitive-lobbies tournament-state] :as d} :?data}]
  (let [player-split (split-players competitive-lobbies)]
    (reset! stored-tables player-split)
    (reset! action-summary (filter #(or (pos? (:time-extension % 0))(:excluded? %)) player-split)))
  (js/console.log (str "Data: " d))
  (reset! active-round tournament-state)
  (non-game-toast "tables refreshed!" "info" nil))

(defmethod ws/event-msg-handler :tournament/declare-round [{{:keys [error]} :?data}]
  (non-game-toast error "error" nil))
