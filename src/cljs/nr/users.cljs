(ns nr.users
  (:require
   [clojure.string :as str]
   [nr.appstate :refer [app-state]]
   [nr.utils :refer [non-game-toast]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(def users-state (r/atom {}))

(defmethod ws/event-msg-handler :admin/fetch-users
  [{{success :success} :?data}]
  (when success
    (reset!
      users-state
      (reduce
        (fn [acc {:keys [ismoderator specials tournament-organizer banned] :as user}]
          (cond-> acc
            ismoderator (update :mods conj user)
            specials (update :specials conj user)
            tournament-organizer (update :tos conj user)
            banned (update :banned conj user)))
        {}
        success))))

(defmethod ws/event-msg-handler :admin/user-edit
  [{{:keys [error success]} :?data}]
  (cond
    error (non-game-toast error "error" nil)
    success
    (let [{:keys [action user-type user]} success]
      (case action
        :admin/add-user
        (do (swap! users-state update user-type (fnil conj #{}) user)
            (non-game-toast (str "Updated " (:username user)) "success" nil))
        :admin/remove-user
        (do (swap! users-state update user-type (fnil disj #{}) user)
            (non-game-toast (str "Removed " (:username user)) "success" nil))
        ; else
        (non-game-toast "Wrong action type" "error" nil)))))

(defn- remove-user
  "Creates a handler which will remove the `user-type` from the user"
  [user-type]
  (fn [username]
    (ws/ws-send! [:admin/edit-user {:action :admin/remove-user
                                    :user-type user-type
                                    :username username}])))

(defn- add-user
  "Creates a handler which will add the `user-type` to the user"
  [user-type]
  (fn [username]
    (ws/ws-send! [:admin/edit-user {:action :admin/add-user
                                    :user-type user-type
                                    :username username}])))

(defn- users-list [users remove-fn]
  [:div.users-box.panel.blue-shade
   [:ul.list
    (doall
      (for [d (sort-by :username @users)]
        [:li.users-item
         {:key (:_id d)}
         [:span
          [:button.delete
           {:on-click #(remove-fn (:username d))}
           "Remove"]]
         [:span.title (:username d "")]]))]])

(defn- user-add [state state-key add-fn]
  [:form.msg-box {:on-submit #(let [username (state-key @state "")]
                                (.preventDefault %)
                                (when-not (str/blank? username)
                                  (add-fn username)
                                  (swap! state assoc state-key "")))}
   [:input {:type "text"
            :placeholder "Type username"
            :value (state-key @state "")
            :on-change #(swap! state assoc state-key (-> % .-target .-value))}]
   (let [username (state-key @state "")
         disabled (str/blank? username)]
     [:button {:disabled disabled
               :class (if disabled "disabled" "")}
      "Add"])])

(defn users-container []
  (r/with-let [mods (r/cursor users-state [:mods])
               specials (r/cursor users-state [:specials])
               tos (r/cursor users-state [:tos])
               banned (r/cursor users-state [:banned])]
    (let [s (r/atom {})
          rows [{:h3 "Moderators"
                 :cursor mods
                 :key :mods
                 :h4 "Add moderator"}
                {:h3 "Alt Art Access"
                 :cursor specials
                 :key :specials
                 :h4 "Give user alt art access"}
                {:h3 "Tournament Organizers"
                 :cursor tos
                 :key :tos
                 :h4 "Add Tournament Organizer"}
                {:h3 "Banned Users"
                 :cursor banned
                 :key :banned
                 :h4 "Ban user"}]]
      (fn []
        (into [:div.container.panel.blue-shade.content-page]
              (->> (for [row rows]
                     [[:h3 (:h3 row)]
                      (users-list (:cursor row) (remove-user (:key row)))
                      [:h4 (:h4 row)]
                      (user-add s (:key row) (add-user (:key row)))])
                   (interpose [[:br]])
                   (mapcat identity)
                   (map-indexed (fn [idx itm] ^{:key idx} itm))))))))

(defn users []
  (r/with-let [user (r/cursor app-state [:user])]
    (ws/ws-send! [:admin/fetch-users])
    [:div.page-container
     [:div.account-bg]
     (when (:isadmin @user)
       [users-container])]))
