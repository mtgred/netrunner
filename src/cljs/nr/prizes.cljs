(ns nr.prizes
  (:require
   [clojure.string :as str]
   [nr.appstate :refer [app-state]]
   [nr.utils :refer [non-game-toast]]
   [nr.ws :as ws]
   [nr.users :refer [users-state]]
   [reagent.core :as r]
   [jinteki.card-backs :as card-backs]))

(def selected-user (r/atom {}))
(def prizes-state (r/atom {}))

(defmethod ws/event-msg-handler :prizes/load-user
  [{{:keys [error success]} :?data}]
  (cond
    error (non-game-toast error "error" nil)
    success (let [{:keys [username prizes]} success]
              (non-game-toast (str "loaded user: " username) "info" nil)
              (reset! selected-user {:username username :prizes prizes}))))

(defmethod ws/event-msg-handler :prizes/update-user
  [{{:keys [error success]} :?data}]
  (cond
    error (non-game-toast error "error" nil)
    success (do (non-game-toast success "info" nil)
                (reset! selected-user {}))))

(defn assign-card-backs []
  [:section
   [:h4 "Card Backs"]
   [:p "These are card backs that the player can select in the settings menu. Players can opt to see the card backs other players use, only the ones they select, or only ffg/nsg backs for their opponents."]
   [:br]
   (doall
     (for [[k {:keys [name description]}] (card-backs/just-prizes)]
       ^{:key k}
       [:div
        [:label [:input {:type "checkbox"
                         :checked (get-in @selected-user [:prizes :card-backs k] false)
                         :on-change #(swap! selected-user assoc-in [:prizes :card-backs k] (.. % -target -checked))}]
         (str name " - " description)]]))])

(defn- load-user-fn [username]
  (ws/ws-send! [:prizes/load-user {:username username}]))

(defn- save-user-fn []
  (when (:username @selected-user)
    (let [{:keys [card-backs]} (:prizes @selected-user)
          prizes {:card-backs (into {} (filter second card-backs))}
          username (:username @selected-user)]
    (ws/ws-send! [:prizes/update-user {:username username :prizes prizes}]))))

(defn- user-save []
  (let [disabled (not (:username @selected-user))]
    [:section
     [:p.float-right
      [:button {:disabled disabled
                :class (if disabled "disabled" "")
                :on-click save-user-fn}
       "Save User"]]]))

(defn- user-load []
  [:form.msg-box {:on-submit #(let [username (:username @prizes-state "")]
                                (.preventDefault %)
                                (when-not (str/blank? username)
                                  (load-user-fn username)))}
   [:input {:type "text"
            :placeholder "Type username"
            :value (:username @prizes-state "")
            :on-change #(swap! prizes-state assoc :username (-> % .-target .-value))}]
   (let [username (:username @prizes-state "")
         disabled (str/blank? username)]
     [:button {:disabled disabled
               :class (if disabled "disabled" "")}
      "Load User"])])

(defn active-user []
  [:div "User Selected: " (:username @selected-user "(nil)")])

(defn prizes-container []
  [:div.container.panel.blue-shade.content-page
   [:section
    [:h3 "Tournament Prizes"]
    [:p "Assign tournament prizes to users. I'm trusting you to be honest. These are done on a per-user basis."]
    [:br]
    [:p "To assign: Load a user. Tick the prizes they should have. Save the user."]
    [user-load]]
   [:section [active-user]]
   [assign-card-backs]
   [user-save]
   ])

(defn prizes []
  (r/with-let [user (r/cursor app-state [:user])]
    (ws/ws-send! [:admin/fetch-users])
    [:div.page-container
     [:div.account-bg]
     (when (or (:isadmin @user) (:ismoderator @user))
       [prizes-container])]))
