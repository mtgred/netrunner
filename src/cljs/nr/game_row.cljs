(ns nr.game-row
  (:require
   [jinteki.utils :refer [superuser?]]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.duration :as duration]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [authenticated] :as auth]
   [nr.player-view :refer [player-view]]
   [nr.sounds :refer [resume-sound]]
   [nr.translations :refer [tr tr-format]]
   [nr.utils :refer [slug->format]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(defn- reset-game-name
  [gameid]
  (authenticated
    (fn [_]
      (ws/ws-send! [:lobby/rename-game {:gameid gameid}]))))

(defn- delete-game
  [gameid]
  (authenticated
    (fn [_]
      (ws/ws-send! [:lobby/delete-game {:gameid gameid}]))))

(defn join-game
  ([lobby-state game action] (join-game lobby-state game action nil))
  ([lobby-state {:keys [gameid started]} action request-side]
   (authenticated
     (fn [_]
       (swap! lobby-state assoc :editing false)
       (ws/ws-send! [(case action
                       "join" :lobby/join
                       "watch" (if started :game/watch :lobby/watch)
                       "rejoin" :game/rejoin)
                     (cond-> {:gameid gameid}
                       request-side (conj {:request-side request-side}))])))))

(defn can-watch? [user game current-game editing]
  (or (superuser? @user)
      (and (:allow-spectator game)
           (not (or current-game editing)))))

(defn watch-button [lobby-state user game current-game editing]
  (when (can-watch? user game current-game editing)
    (if (not (:password game))
      [:button {:on-click #(do (join-game lobby-state game "watch")
                               (resume-sound))}
       (tr [:lobby.watch "Watch"])]
      [:button {:on-click #(if (:password game)
                             (authenticated
                               (fn [_]
                                 (swap! lobby-state assoc :password-game {:game game :action "watch"})))
                             (do (join-game lobby-state game "watch")
                                 (resume-sound)))}
       (tr [:lobby.watch "Watch"])])))

(defn can-join? [user {:keys [room started players]} current-game editing]
  (if (= "tournament" room)
    (some #(= (:username user) (get-in % [:user :username])) players)
    (and (= 1 (count players))
         (not current-game)
         (not editing)
         (not started)
         (not (some #(= (:username user) (get-in % [:user :username])) players)))))

(defn join-button [lobby-state user game current-game editing]
  (when (can-join? user game current-game editing)
    (if (and (some #(= "Any Side" (:side %)) (:players game))
             (not (:password game)))
      [:div.split-button
       [:button {:on-click #(do (join-game lobby-state game "join")
                                (resume-sound))}
        (tr [:lobby.join "Join"])]
       [:button.dropdown-toggle {:data-toggle "dropdown"}
        [:b.caret]]
       [:ul.dropdown-menu.blue-shade
        [:a.block-link {:on-click #(do (join-game lobby-state game "join" "Corp")
                                       (resume-sound))}
         (tr [:lobby.as-corp "As Corp"])]
        [:a.block-link {:on-click #(do (join-game lobby-state game "join" "Runner")
                                       (resume-sound))}
         (tr [:lobby.as-runner "As Runner"])]]]
      [:button {:on-click #(if (:password game)
                             (authenticated
                               (fn [_]
                                 (swap! lobby-state assoc :password-game {:game game :action "join"})))
                             (do (join-game lobby-state game "join")
                                 (resume-sound)))}
       (tr [:lobby.join "Join"])])))

(defn can-rejoin? [user {:keys [started players original-players]} current-game editing]
  (and (= 1 (count players))
       (not current-game)
       (not editing)
       started
       (some #(= (:username @user) (get-in % [:user :username])) original-players)))

(defn rejoin-button [lobby-state user game current-game editing]
  (when (can-rejoin? user game current-game editing)
    [:button {:on-click #(if (:password game)
                           (authenticated
                             (fn [_]
                               (swap! lobby-state assoc :password-game {:game game :action "rejoin"})))
                           (do (join-game lobby-state game "rejoin")
                               (resume-sound)))}
     (tr [:lobby.rejoin "Rejoin"])]))

(defn mod-menu-popup [s user {gameid :gameid}]
  (when (and (:show-mod-menu @s)
             (superuser? @user))
    [:div.ctrl-menu
     [:div.panel.blue-shade.mod-menu
      [:div {:on-click #(do (reset-game-name gameid)
                            (swap! s assoc :show-mod-menu false))}
       (tr [:lobby.reset "Reset Game Name"])]
      [:div {:on-click #(do (delete-game gameid)
                            (swap! s assoc :show-mod-menu false))}
       (tr [:lobby.delete "Delete Game"])]
      [:div {:on-click #(swap! s assoc :show-mod-menu false)}
       (tr [:lobby.cancel "Cancel"])]]]))

(defn game-title [s user game]
  [:h4 {:on-click #(swap! s update :show-mod-menu not)
        :class (when (superuser? @user) "clickable")}
   (str (when (:save-replay game) "🟢")
        (when (:password game) (str "[" (tr [:lobby.private "PRIVATE"]) "] "))
        (:title game)
        (let [c (count (:spectators game))]
          (when (pos? c) (str " (" (tr [:lobby.spectator-count] c) ")"))))])

(defn game-format [{fmt :format singleton? :singleton first-five? :first-five}]
  [:div {:class "game-format"}
   [:span.format-label (tr [:lobby.format "Format"]) ":  "]
   [:span.format-type (tr-format (slug->format fmt "Unknown"))]
   [:span.format-singleton (str (when singleton? " (singleton)"))]
   [:span.format-first-five (str (when first-five? " (first-five mode)"))]])

(defn- time-since
  "Helper method for game-time. Computes how many minutes since game start"
  [start]
  (let [now (inst/now)
        diff (duration/between start now)
        total-seconds (duration/get diff chrono/seconds)
        minutes (abs (quot total-seconds 60))]
    minutes))

(defn game-time [game]
(when (:started game)
  [:div.game-time (str (time-since (:date game)) "m")]))

(defn players-row [{players :players :as game}]
  (into
    [:div]
    (map
      (fn [player]
        ^{:key (:_id player)}
        [player-view player game])
      players)))

(defn game-row [lobby-state game current-game editing]
  (r/with-let [state (r/atom {:show-mod-menu false})
               user (r/cursor app-state [:user])]
    [:div.gameline {:class (when (= (:gameid game) (:gameid current-game)) "active")}
     [watch-button lobby-state user game current-game editing]
     [join-button lobby-state user game current-game editing]
     [rejoin-button lobby-state user game current-game editing]
     [game-title state user game]
     [mod-menu-popup state user game]
     [game-format game]
     [game-time game]
     [players-row game]]))
