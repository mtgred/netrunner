(ns nr.users
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [POST GET PUT DELETE]]
            [nr.cardbrowser :refer [non-game-toast] :as cb]
            [nr.ws :refer [ws-send!]]
            [nr.appstate :refer [app-state]]
            [clojure.string :as s]
            [reagent.core :as r]))

(def users-state (r/atom {}))

(go (swap! users-state assoc :mods (:json (<! (GET "/admin/mods")))))

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

(defn users-container []
  (r/with-let [mods (r/cursor users-state [:mods])]
    (let [s (r/atom {})]
      (fn []
        [:div.container.panel.blue-shade.content-page
         [:h3 "Moderators"]
         [:div.users-box.panel.blue-shade
          [:ul.list
           (doall
             (for [d @mods]
               [:li.users-item
                {:key (:_id d)}
                [:span 
                 [:button.delete
                  {:on-click #(remove-moderator (:_id d))}
                  "Remove"]]
                [:span.title (:username d "")]]))]]
         [:h4 "Add moderator"]
         [:form.msg-box {:on-submit #(let [msg (:mod-name @s "")]
                                       (.preventDefault %)
                                       (when-not (s/blank? msg)
                                         (add-moderator msg)
                                         (swap! s assoc :mod-name "")))}
          [:input {:type "text"
                   :placeholder "Type username"
                   :value (:mod-name @s "")
                   :on-change #(swap! s assoc :mod-name (-> % .-target .-value))}]
          (let [msg (:mod-name @s "")
                disabled (s/blank? msg)]
            [:button {:disabled disabled
                      :class (if disabled "disabled" "")}
             "Add"])]

         ;; [:br]
         ]))))

(defn users []
  (r/with-let [user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])]
    (when (and (= "/users" (first @active))
               (:isadmin @user))
      [users-container])))
