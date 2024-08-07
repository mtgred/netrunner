(ns nr.password-game
  (:require
   [nr.auth :refer [authenticated]]
   [nr.translations :refer [tr tr-watch-join]]
   [nr.ws :as ws]
   [reagent.core :as r]
   [taoensso.sente :as sente]))

(defn join-game
  [lobby-state state game action request-side]
  (authenticated
    (fn [_]
      (ws/ws-send! [(case action
                      "join" :lobby/join
                      "watch" :lobby/watch
                      "rejoin" :game/rejoin)
                    (cond-> {:gameid (:gameid game)
                             :password (:password @state)}
                      request-side (conj {:request-side request-side}))]
                   8000
                   #(if (sente/cb-success? %)
                      (case %
                        403 (swap! state assoc :error-msg (tr [:lobby.invalid-password "Invalid password"]))
                        404 (swap! state assoc :error-msg (tr [:lobby.not-allowed "Not allowed"]))
                        200 (swap! lobby-state assoc :editing false :password-game nil))
                      (swap! state assoc :error-msg (tr [:lobby.aborted "Connection aborted"])))))))

(defn password-game [lobby-state]
  (r/with-let [game (r/cursor lobby-state [:password-game :game])
               action (r/cursor lobby-state [:password-game :action])
               request-side (r/cursor lobby-state [:password-game :request-side])
               state (r/atom {:password nil
                              :error-msg nil})]
    (fn [lobby-state]
      [:div.password-prompt
       [:h3 (str (tr [:lobby.password-for "Password for"])
                 " " (:title @game))]
       [:p
        [:input.game-title {:on-change #(swap! state assoc :password (.. % -target -value))
                            :value (:password @state)
                            :placeholder (tr [:lobby.password "Password"])
                            :maxLength "30"
                            :on-key-press (fn [e]
                                            (when (= 13 (.. e -charCode))
                                              (join-game lobby-state state @game @action @request-side)))}]]
       [:p
        [:button {:type "button"
                  :on-click #(join-game lobby-state state @game @action @request-side)}
         (tr-watch-join @action)]
        [:span.fake-link {:on-click #(do
                                       (swap! lobby-state dissoc :password-game)
                                       (reset! state {:error-msg nil :password nil}))}
         (tr [:lobby.cancel "Cancel"])]]
       (when-let [error-msg (:error-msg @state)]
         [:p.flash-message error-msg])])))
