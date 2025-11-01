(ns nr.admin
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [clojure.string :as s]
   [nr.ajax :refer [DELETE GET POST PUT]]
   [nr.appstate :refer [app-state]]
   [nr.utils :refer [format-date-time ISO-ish-formatter non-game-toast
                     render-icons]]
   [nr.ws :as ws]
   [reagent.core :as r]
   [taoensso.sente :as sente]))

(def admin-state
  (r/atom {:pause-game-creation (:block-game-creation @app-state)}))

(go (swap! admin-state assoc :news (:json (<! (GET "/data/news")))))
(when (:isadmin (:user @app-state))
  (go (swap! admin-state assoc :version (:json (<! (GET "/admin/version"))))))

(defn- post-data [url callback data]
  (go (let [response (<! (POST url data :json))]
        (callback response))))

(defn- update-news-response [response]
  (if (= 200 (:status response))
    (do
      (go (swap! admin-state assoc :news (:json (<! (GET "/data/news")))))
      (non-game-toast "Updated news items" "success" nil))
    (non-game-toast "Failed to update news items" "error" nil)))

(defn- delete-news-item [id]
  (go (let [response (<! (DELETE (str "/admin/news/" id)))]
        (update-news-response response))))

(defn- post-news-item [msg]
  (post-data "/admin/news" update-news-response {:item msg}))

(defn- update-version-response [response]
  (if (= 200 (:status response))
    (do
      (go (swap! admin-state assoc :version (:json (<! (GET "/admin/version")))))
      (non-game-toast "Updated version" "success" nil))
    (non-game-toast "Failed to update version" "error" nil)))

(defn- update-version-item [msg]
  (go (let [response (<! (PUT "/admin/version" {:version msg} :json))]
        (update-version-response response))))

(defn- update-banned-response [response]
  (if (= 200 (:status response))
    (do
      (go (swap! admin-state assoc :banned (:json (<! (GET "/admin/banned")))))
      (non-game-toast "Updated banned message" "success" nil))
    (non-game-toast "Failed to update banned message" "error" nil)))

(defn- update-banned-item [msg]
  (go (let [response (<! (PUT "/admin/banned" {:banned msg} :json))]
        (update-banned-response response))))

(defn- update-announce-response [response]
  (if (sente/cb-success? response)
    (case response
      200 (non-game-toast "Sent announcement" "success" nil)
      403 (non-game-toast "Not an admin" "error" nil)
      ; else
      (non-game-toast "Failed to send announcement" "error" nil))
    (non-game-toast "Failed to send announcement" "error" nil)))

(defn- post-announce-item [msg]
  (ws/ws-send! [:admin/announce {:message msg}]
               8000
               update-announce-response))

(defn- update-pause-game-creation
  [paused]
  (ws/ws-send! [:admin/block-game-creation paused]
               8000
               (fn [response]
                 (swap! admin-state assoc :pause-game-creation response))))

(defn admin-container []
  (r/with-let [news (r/cursor admin-state [:news])
               version (r/cursor admin-state [:version])
               pause-game-creation (r/cursor admin-state [:pause-game-creation])
               s (r/atom {})]
    [:div.container.panel.blue-shade.content-page
     [:h3 "Site News"]
     [:div.news-box.panel.blue-shade
      [:ul.list
       (doall
         (for [d @news]
           [:li.news-item
            {:key (:date d)}
            [:span
             [:button.delete
              {:on-click #(delete-news-item (:_id d))}
              "Delete"]]
            [:span.date
             (format-date-time ISO-ish-formatter (:date d))]
            [:span.title (render-icons (:item d ""))]]))]]
     [:h4 "Add news item"]
     [:form.msg-box {:on-submit #(let [msg (:news-msg @s "")]
                                   (.preventDefault %)
                                   (when-not (s/blank? msg)
                                     (post-news-item msg)
                                     (swap! s assoc :news-msg "")))}
      [:input {:type "text"
               :placeholder "Post something...."
               :value (:news-msg @s "")
               :on-change #(swap! s assoc :news-msg (-> % .-target .-value))}]
      (let [msg (:news-msg @s "")
            disabled (s/blank? msg)]
        [:button {:disabled disabled
                  :class (if disabled "disabled" "")}
         "Post"])]

     [:br]
     [:h3 "App Version"]
     [:div.panel
      [:input {:type "text" :name "version" :value (:version @version "") :read-only true}]]
     [:h4 "Update app version string"]
     [:form.msg-box {:on-submit #(let [msg (:version-msg @s)]
                                   (.preventDefault %)
                                   (when-not (s/blank? msg)
                                     (update-version-item msg)
                                     (swap! s assoc :version-msg "")))}
      [:input {:type "text"
               :placeholder "Type something...."
               :value (:version-msg @s "")
               :on-change #(swap! s assoc :version-msg (-> % .-target .-value))}]
      (let [msg (:version-msg @s "")
            disabled (s/blank? msg)]
        [:button {:disabled disabled
                  :class (if disabled "disabled" "")}
         "Update"])]

     [:br]
     [:h3 "Update banned user login failure message"]
     [:form.msg-box {:on-submit #(let [msg (:banned @s)]
                                   (.preventDefault %)
                                   (when-not (s/blank? msg)
                                     (update-banned-item msg)
                                     (swap! s assoc :banned "")))}
      [:input {:type "text"
               :placeholder "Type something...."
               :value (:banned @s "")
               :on-change #(swap! s assoc :banned (-> % .-target .-value))}]
      (let [msg (:banned @s "")
            disabled (s/blank? msg)]
        [:button {:disabled disabled
                  :class (if disabled "disabled" "")}
         "Update"])]

     [:br]
     [:h3 "Site Announcement"]
     [:form.msg-box {:on-submit #(let [msg (:announce-msg @s)]
                                   (.preventDefault %)
                                   (when-not (s/blank? msg)
                                     (post-announce-item msg)
                                     (swap! s assoc :announce-msg "")))}
      [:input {:type "text"
               :placeholder "Type something...."
               :value (:announce-msg @s "")
               :on-change #(swap! s assoc :announce-msg (-> % .-target .-value))}]
      (let [msg (:announce-msg @s "")
            disabled (s/blank? msg)]
        [:button {:disabled disabled
                  :class (if disabled "disabled" "")}
         "Send"])]

     [:br]
     [:h3 "Game Creation Control"]
     [:div.panel.blue-shade
      [:label
       [:input {:type "checkbox"
                :checked @pause-game-creation
                :on-change #(update-pause-game-creation (.. % -target -checked))}]
       " Pause new game creation (allows draining games before maintenance)"]]]))


(defn admin []
  (r/with-let [user (r/cursor app-state [:user])]
    [:div.page-container
     [:div.help-bg]
     (when (:isadmin @user)
       [admin-container])]))
