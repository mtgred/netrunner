(ns nr.users
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [POST GET PUT DELETE]]
            [nr.utils :refer [non-game-toast]]
            [nr.ws :refer [ws-send!]]
            [nr.appstate :refer [app-state]]
            [clojure.string :as s]
            [reagent.core :as r]))

(def users-state (r/atom {}))

(go (when (:isadmin (:user @app-state)) (swap! users-state assoc :mods (:json (<! (GET "/admin/mods"))))))
(go (when (:isadmin (:user @app-state)) (swap! users-state assoc :specials (:json (<! (GET "/admin/specials"))))))

(defn- update-mod-response [response]
  (case (:status response)
    200 (do
          (go (swap! users-state assoc :mods (:json (<! (GET "/admin/mods")))))
          (non-game-toast "Updated moderators" "success" nil))
    404 (non-game-toast "Unknown username" "error" nil)
    (non-game-toast "Failed to update moderators" "error" nil)))

(defn- remove-moderator [id]
  (go (let [response (<! (DELETE (str "/admin/mods/" id)))]
        (update-mod-response response))))

(defn- add-moderator [msg]
  (go (let [response (<! (PUT "/admin/mods" {:username msg} :json))]
        (update-mod-response response))))

(defn- update-special-response [response]
  (case (:status response)
    200 (do
          (go (swap! users-state assoc :specials (:json (<! (GET "/admin/specials")))))
          (non-game-toast "Updated alt art users" "success" nil))
    404 (non-game-toast "Unknown username" "error" nil)
    (non-game-toast "Failed to update alt art users" "error" nil)))

(defn- remove-special [id]
  (go (let [response (<! (DELETE (str "/admin/specials/" id)))]
        (update-special-response response))))

(defn- add-special [msg]
  (go (let [response (<! (PUT "/admin/specials" {:username msg} :json))]
        (update-special-response response))))

(defn- users-list [users remove-fn]
  [:div.users-box.panel.blue-shade
   [:ul.list
    (doall
      (for [d @users]
        [:li.users-item
         {:key (:_id d)}
         [:span
          [:button.delete
           {:on-click #(remove-fn (:_id d))}
           "Remove"]]
         [:span.title (:username d "")]]))]])

(defn- user-add [state state-key add-fn]
  [:form.msg-box {:on-submit #(let [msg (state-key @state "")]
                                (.preventDefault %)
                                (when-not (s/blank? msg)
                                  (add-fn msg)
                                  (swap! state assoc state-key "")))}
   [:input {:type "text"
            :placeholder "Type username"
            :value (state-key @state "")
            :on-change #(swap! state assoc state-key (-> % .-target .-value))}]
   (let [msg (state-key @state "")
         disabled (s/blank? msg)]
     [:button {:disabled disabled
               :class (if disabled "disabled" "")}
      "Add"])])

(defn users-container []
  (r/with-let [mods (r/cursor users-state [:mods])
               specials (r/cursor users-state [:specials])]
    (let [s (r/atom {})]
      (fn []
        [:div.container.panel.blue-shade.content-page
         [:h3 "Moderators"]
         (users-list mods remove-moderator)
         [:h4 "Add moderator"]
         (user-add s :mod-name add-moderator)
         [:br]
         [:h3 "Alt Art Access"]
         (users-list specials remove-special)
         [:h4 "Add Alt Art User"]
         (user-add s :special-name add-special)]))))

(defn users []
  (r/with-let [user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])]
    (when (and (= "/users" (first @active))
               (:isadmin @user))
      [:div.page-container
       [users-container]])))
