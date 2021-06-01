(ns nr.game-row
  (:require [reagent.core :as r]
            [taoensso.sente :as sente]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [authenticated] :as auth]
            [nr.player-view :refer [player-view]]
            [nr.sounds :refer [resume-sound]]
            [nr.utils :refer [slug->format cond-button]]
            [nr.translations :refer [tr tr-format tr-watch-join]]
            [nr.ws :as ws]
            [jinteki.utils :refer [superuser?]]))

(defn join-game [gameid s action password request-side]
  (authenticated
    (fn [user]
      (swap! s assoc :editing false)
      (ws/ws-send! [(case action
                      "join" :lobby/join
                      "watch" :lobby/watch
                      "rejoin" :netrunner/rejoin)
                    {:gameid gameid
                     :password password
                     :request-side request-side}]
                   8000
                   #(if (sente/cb-success? %)
                      (case %
                        403 (swap! s assoc :error-msg (tr [:lobby.invalid-password "Invalid password"]))
                        404 (swap! s assoc :error-msg (tr [:lobby.not-allowed "Not allowed"]))
                        200 (swap! s assoc :prompt false))
                      (swap! s assoc :error-msg (tr [:lobby.aborted "Connection aborted"])))))))

(defn- reset-game-name
  [gameid]
  (authenticated
    (fn [user]
      (ws/ws-send! [:lobby/rename-game {:gameid gameid}]))))

(defn- delete-game
  [gameid]
  (authenticated
    (fn [user]
      (ws/ws-send! [:lobby/delete-game {:gameid gameid}]))))

(defn game-row
  [{:keys [title format room password started players gameid current-game
           password-game original-players editing] :as game}]
  (r/with-let [s (r/atom {:show-mod-menu false})
               user (:user @app-state)
               join (fn [action]
                      (let [password (:password password-game password)
                            input-password (:password @s)
                            request-side (:request-side @s)]
                        (cond
                          (not password)
                          (join-game (if password-game (:gameid password-game) gameid) s action nil request-side)
                          input-password
                          (join-game (if password-game (:gameid password-game) gameid) s action input-password request-side)
                          :else
                          (do (swap! app-state assoc :password-gameid gameid)
                              (swap! s assoc :prompt action)))))]
    [:div.gameline {:class (when (= current-game gameid) "active")}
     (when (or (superuser? user)
               (and (:allow-spectator game)
                    (not (or password-game current-game editing))))
       [:button {:on-click #(do (join "watch")
                                (resume-sound))} (tr [:lobby.watch "Watch"]) editing])
     (when (or (and (= "tournament" room)
                    (some #(= (:username user) (get-in % [:user :username])) players))
               (and (not= "tournament" room)
                    (>= 1 (count players))
                    (not current-game)
                    (not editing)
                    (not started)
                    (not (some #(= (get-in % [:user :_id]) (get-in @app-state [:user :_id])) players))))
       (if (some #(= "Any Side" (:side %)) players)
         [:div.split-button
          [:button {:on-click #(do (swap! s assoc :request-side "Any Side")
                                   (join "join")
                                   (resume-sound))}
           (tr [:lobby.join "Join"])]
          [:button.dropdown-toggle {:data-toggle "dropdown"}
           [:b.caret]]
          [:ul.dropdown-menu.blue-shade
           [:a.block-link {:on-click #(do (swap! s assoc :request-side "Corp")
                                          (join "join")
                                          (resume-sound))}
            (tr [:lobby.as-corp "As Corp"])]
           [:a.block-link {:on-click #(do (swap! s assoc :request-side "Runner")
                                          (join "join")
                                          (resume-sound))}
            (tr [:lobby.as-runner "As Runner"])]]]
         [:button {:on-click #(do (swap! s assoc :request-side "Any Side")
                                  (join "join")
                                  (resume-sound))}
          (tr [:lobby.join "Join"])]))
     (when (and (not current-game)
                (not editing)
                started
                (= 1 (count players))
                (not password-game)
                (some #(= (get-in % [:user :_id]) (get-in @app-state [:user :_id])) original-players))
       [:button {:on-click #(do (join "rejoin")
                                (resume-sound))}
        (tr [:lobby.rejoin "Rejoin"])])
     (let [c (:spectator-count game)]
       [:h4
        {:on-click #(swap! s update :show-mod-menu not)
         :class (when (or (:isadmin user)
                          (:ismoderator user))
                  "clickable")}
        (str (when (:save-replay game) "🟢")
             (when (:password game) (str "[" (tr [:lobby.private "PRIVATE"]) "] "))
             (:title game)
             (when (pos? c) (str " (" (tr [:lobby.spectator-count] c) ")")))])

     (when (and (:show-mod-menu @s)
                (or (:isadmin user) (:ismoderator user)))
       [:div.ctrl-menu
        [:div.panel.blue-shade.mod-menu
         [:div {:on-click #(do (reset-game-name gameid)
                               (swap! s assoc :show-mod-menu false))} (tr [:lobby.reset "Reset Game Name"])]
         [:div {:on-click #(do (delete-game gameid)
                               (swap! s assoc :show-mod-menu false))} (tr [:lobby.delete "Delete Game"])]
         [:div {:on-click #(swap! s assoc :show-mod-menu false)} (tr [:lobby.cancel "Cancel"])]]])

     [:div {:class "game-format"}
      [:span.format-label (tr [:lobby.format "Format"]) ":  "]
      [:span.format-type (tr-format (slug->format format "Unknown"))]]

     [:div (doall
             (map-indexed
               (fn [idx player]
                 ^{:key idx}
                 [player-view player game])
               players))]

     (when-let [prompt (:prompt @s)]
       [:div.password-prompt
        [:h3 (str (tr [:lobby.password-for "Password for"]) " " (if password-game (:title password-game) title))]
        [:p
         [:input.game-title {:on-change #(swap! s assoc :password (.. % -target -value))
                             :type "password"
                             :value (:password @s)
                             :placeholder (tr [:lobby.password "Password"])
                             :maxLength "30"
                             :on-key-press (fn [e]
                                            (when (= 13 (.. e -charCode))
                                              (join prompt)))}]]

        [:p
         [:button {:type "button"
                   :on-click #(join prompt)}
          (tr-watch-join prompt)]
         [:span.fake-link {:on-click #(do
                                        (swap! app-state dissoc :password-gameid)
                                        (swap! s assoc
                                               :prompt false
                                               :error-msg nil
                                               :password nil))}
          (tr [:lobby.cancel "Cancel"])]]
        (when-let [error-msg (:error-msg @s)]
          [:p.flash-message error-msg])])]))
