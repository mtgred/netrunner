(ns nr.game-row
  (:require [reagent.core :as r]
            [taoensso.sente :as sente]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [authenticated] :as auth]
            [nr.player-view :refer [player-view]]
            [nr.sounds :refer [resume-sound]]
            [nr.utils :refer [slug->format cond-button]]
            [nr.ws :as ws]
            [jinteki.utils :refer [superuser?]]))

(defn join-game [gameid s action password]
  (authenticated
    (fn [user]
      (swap! s assoc :editing false)
      (ws/ws-send! [(case action
                      "join" :lobby/join
                      "watch" :lobby/watch
                      "rejoin" :netrunner/rejoin)
                    {:gameid gameid
                     :password password
                     :options (:options @app-state)}]
                   8000
                   #(if (sente/cb-success? %)
                      (case %
                        403 (swap! s assoc :error-msg "Invalid password")
                        404 (swap! s assoc :error-msg "Not allowed")
                        200 (swap! s assoc :prompt false))
                      (swap! s assoc :error-msg "Connection aborted"))))))

(defn- reset-game-name
  [gameid]
  (authenticated
    (fn [user]
      (ws/ws-send! [:lobby/rename-game {:gameid gameid}]))))

(defn game-row
  [{:keys [title format room password started players gameid current-game
           password-game original-players editing] :as game}]
  (r/with-let [s (r/atom {:show-mod-menu false})
               user (:user @app-state)
               join (fn [action]
                      (let [password (:password password-game password)
                            input-password (:password @s)]
                        (cond
                          (empty? password)
                          (join-game (if password-game (:gameid password-game) gameid) s action nil)
                          input-password
                          (join-game (if password-game (:gameid password-game) gameid) s action input-password)
                          :else
                          (do (swap! app-state assoc :password-gameid gameid)
                              (swap! s assoc :prompt action)))))]
    [:div.gameline {:class (when (= current-game gameid) "active")}
     (when (or (superuser? user)
               (and (:allow-spectator game)
                    (not (or password-game current-game editing))))
       [:button {:on-click #(do (join "watch")
                                (resume-sound))} "Watch" editing])
     (when (or (and (= "tournament" room)
                    (some #(= (:username user) (get-in % [:user :username])) players))
               (and (not= "tournament" room)
                    (>= 1 (count players))
                    (not current-game)
                    (not editing)
                    (not started)
                    (not (some #(= (get-in % [:user :_id]) (get-in @app-state [:user :_id])) players))))
       [:button {:on-click #(do (join "join")
                                (resume-sound))}
        "Join"])
     (when (and (not current-game)
                (not editing)
                started
                (= 1 (count players))
                (not password-game)
                (some #(= (get-in % [:user :_id]) (get-in @app-state [:user :_id])) original-players))
       [:button {:on-click #(do (join "rejoin")
                                (resume-sound))}
        "Rejoin"])
     (let [c (count (:spectators game))]
       [:h4
        {:on-click #(swap! s update :show-mod-menu not)
         :class (when (or (:isadmin user)
                          (:ismoderator user))
                  "clickable")}
        (str (when-not (empty? (:password game))
               "[PRIVATE] ")
             (:title game)
             (when (pos? c)
               (str  " (" c " spectator" (when (> c 1) "s") ")")))])

     (when (and (:show-mod-menu @s)
                (or (:isadmin user) (:ismoderator user)))
       [:div.panel.blue-shade.mod-menu
        [:div {:on-click #(do (reset-game-name gameid)
                              (swap! s assoc :show-mod-menu false))} "Reset Game Name"]
        [:div {:on-click #(swap! s assoc :show-mod-menu false)} "Cancel"]])

     [:div {:class "game-format"}
      [:span.format-label "Format:  "]
      [:span.format-type (slug->format format "Unknown")]]

     [:div (doall
             (map-indexed
               (fn [idx player]
                 ^{:key idx}
                 [player-view player game])
               players))]

     (when-let [prompt (:prompt @s)]
       [:div.password-prompt
        [:h3 (str "Password for " (if password-game (:title password-game) title))]
        [:p
         [:input.game-title {:on-change #(swap! s assoc :password (.. % -target -value))
                             :type "password"
                             :value (:password @s)
                             :placeholder "Password"
                             :maxLength "30"
                             :on-key-press (fn [e]
                                            (when (= 13 (.. e -charCode))
                                              (join prompt)))}]]

        [:p
         [:button {:type "button"
                   :on-click #(join prompt)}
          prompt]
         [:span.fake-link {:on-click #(do
                                        (swap! app-state dissoc :password-gameid)
                                        (swap! s assoc
                                               :prompt false
                                               :error-msg nil
                                               :password nil))}
          "Cancel"]]
        (when-let [error-msg (:error-msg @s)]
          [:p.flash-message error-msg])])]))
