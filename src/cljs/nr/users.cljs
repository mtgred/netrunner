(ns nr.users
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [POST GET PUT DELETE]]
            [nr.utils :refer [non-game-toast]]
            [nr.ws :refer [ws-send!]]
            [nr.appstate :refer [app-state]]
            [clojure.string :as s]
            [reagent.core :as r]))

(def users-state (r/atom {}))

(def user-type-infos
  {:mods     {:api-base "/admin/mods"     :display-name "moderators"}
   :tos      {:api-base "/admin/tos"      :display-name "tournament organizers"}
   :specials {:api-base "/admin/specials" :display-name "alt art users"}})

(go (when (:isadmin (:user @app-state)) (swap! users-state assoc :mods (:json (<! (GET "/admin/mods"))))))
(go (when (:isadmin (:user @app-state)) (swap! users-state assoc :tos (:json (<! (GET "/admin/tos"))))))
(go (when (:isadmin (:user @app-state)) (swap! users-state assoc :specials (:json (<! (GET "/admin/specials"))))))

(defn- update-user-response-handler
  [user-type]
  (let [{:keys [api-base display-name]} (user-type-infos user-type)]
    (fn [response]
      (case (:status response)
        200 (do
              (go (swap! users-state assoc user-type (:json (<! (GET api-base)))))
              (non-game-toast (str "Updated " display-name) "success" nil))
        404 (non-game-toast "Unknown username" "error" nil)
        (non-game-toast (str "Failed to update " display-name) "error" nil)))))

(defn- remove-user
  "Creates a handler which will remove the `user-type` from the user"
  [user-type]
  (fn [id]
    (go (let [response (<! (DELETE (str (get-in user-type-infos [user-type :api-base]) "/" id)))]
          ((update-user-response-handler user-type) response)))))

(defn- add-user
  "Creates a handler which will add the `user-type` to the user"
  [user-type]
  (fn [msg]
    (go (let [response (<! (PUT (get-in user-type-infos [user-type :api-base]) {:username msg} :json))]
          ((update-user-response-handler user-type) response)))))

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
               specials (r/cursor users-state [:specials])
               tos (r/cursor users-state [:tos])]
    (let [s (r/atom {})]
      (fn []
        [:div.container.panel.blue-shade.content-page
         [:h3 "Moderators"]
         (users-list mods (remove-user :mods))
         [:h4 "Add moderator"]
         (user-add s :mod-name (add-user :mods))
         [:br]
         [:h3 "Alt Art Access"]
         (users-list specials (remove-user :specials))
         [:h4 "Add Alt Art User"]
         (user-add s :special-name (add-user :specials))
         [:br]
         [:h3 "Tournament Organizers"]
         (users-list tos (remove-user :tos))
         [:h4 "Add Tournament Organizer"]
         (user-add s :mod-name (add-user :tos))]))))

(defn users []
  (r/with-let [user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])]
    (when (and (= "/users" (first @active))
               (:isadmin @user))
      [:div.page-container
       [users-container]])))
