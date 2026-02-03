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
        (fn [acc {:keys [ismoderator special tournament-organizer banned] :as user}]
          (cond-> acc
            ismoderator (update :mods conj user)
            special (update :specials conj user)
            tournament-organizer (update :tos conj user)
            banned (update :banned conj user)))
        {}
        success))))

(defmethod ws/event-msg-handler :admin/fetch-ip-bans
  [{{success :success} :?data}]
  (when success
    (swap! users-state assoc :ip-banned success)))

(defmethod ws/event-msg-handler :admin/look-up-ip
  [{{:keys [error success]} :?data}]
  (if success
    (let [{:keys [username last-ip-address]} success]
      (prn "success: " success)
      (swap! users-state assoc :ip-lookup-name username :ip-lookup-result last-ip-address))
    (non-game-toast error nil)))

(defmethod ws/event-msg-handler :admin/ip-ban-user
  [{{:keys [error success]} :?data}]
  (if success
    (let [{:keys [username ip-address]} success]
      (swap! users-state update :ip-banned conj {:username username :ip-address ip-address}))
    (non-game-toast error nil)))

(defmethod ws/event-msg-handler :admin/ip-unban-user
  [{{:keys [error success]} :?data}]
  (if success
    (let [{:keys [username ip-address]} success]
      (swap! users-state update :ip-banned #(filterv (fn [m] (not= username (:username m))) %)))
    (non-game-toast error nil)))

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
        (do (swap! users-state update user-type
                   (fn [lst elt] (remove #(= (:_id elt) (:_id %)) lst))
                   user)
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
      (for [d (sort-by #(str/lower-case (:username %)) @users)]
        [:li.users-item
         {:key (:_id d)}
         [:span
          [:button.delete
           {:on-click #(remove-fn (:username d))}
           "Remove"]]
         [:span.title (:username d "")]]))]])

(defn- ip-bans-list [users]
  ;; here it's a list of {:username ... :address ...}
  [:div.users-box.panel.blue-shade
   [:ul.list
    (doall
      (for [d (sort-by #(str/lower-case (:username %)) @users)]
        [:li.users-item
         {:key (:username d)}
         [:span
          [:button.delete
           {:on-click #(ws/ws-send! [:admin/ip-unban-user {:username (:username d)}])}
           "Remove"]]
         [:span.title (:username d "") " - " (:ip-address d)]]))]])

(defn- ip-ban-user-add [state state-key]
  [:form.msg-box {:on-submit #(let [username (state-key @state "")]
                                (.preventDefault %)
                                (when-not (str/blank? username)
                                  (ws/ws-send! [:admin/ip-ban-user {:username username}])
                                  (swap! state assoc state-key "")))}
   [:input {:type "text"
            :placeholder "Type username"
            :value (state-key @state "")
            :on-change #(swap! state assoc state-key (-> % .-target .-value))}]
   (let [username (state-key @state "")
         disabled (str/blank? username)]
     [:button {:disabled disabled
               :class (if disabled "disabled" "")}
      "IP Ban User"])])

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

(defn- ip-lookup-box [state state-key]
  (fn []
    [:form.msg-box {:on-submit #(let [username (state-key @state "")]
                                  (.preventDefault %)
                                  (when-not (str/blank? username)
                                    (ws/ws-send! [:admin/look-up-ip {:username username}])))}
     [:input {:type "text"
              :placeholder "Type username"
              :value (state-key @state "")
              :on-change #(swap! state assoc state-key (-> % .-target .-value))}]
     [:input {:type "text"
              :read-only true
              :value (:ip-lookup-result @state "")
              :placeholder "Look up an IP"}]
    (let [username (state-key @state "")
          disabled (str/blank? username)]
      [:button {:disabled disabled
                :class (if disabled "disabled" "")}
       "Search"])]))




(defn users-container []
  (r/with-let [mods (r/cursor users-state [:mods])
               specials (r/cursor users-state [:specials])
               tos (r/cursor users-state [:tos])
               banned (r/cursor users-state [:banned])
               ip-bans (r/cursor users-state [:ip-banned])]
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
        [:div.container.panel.blue-shade.content-page
         (into [:div]
               (->> (for [{:keys [h3 cursor key h4] :as row} rows]
                      ;; (if (= key :ip-bans)
                      ;;   [[:h3 (:h3 row)]
                      [[:h3 h3]
                       (users-list cursor (remove-user key))
                       [:h4 h4]
                       (user-add s key (add-user key))])
                    (interpose [[:br]])
                    (mapcat identity)
                    (map-indexed (fn [idx itm] ^{:key idx} itm))))
         [:br]
         [:h3 "IP Lookup"]
         [ip-lookup-box users-state :ip-lookup-name]
         [:br]
         [:h3 "Ip Bans"]
         [ip-bans-list ip-bans]
         [ip-ban-user-add users-state :ip-ban-name]]))))

(defn users []
  (r/with-let [user (r/cursor app-state [:user])]
    (ws/ws-send! [:admin/fetch-users])
    (ws/ws-send! [:admin/fetch-ip-bans])
    [:div.page-container
     [:div.account-bg]
     (when (or (:isadmin @user) (:ismoderator @user))
       [users-container])]))
