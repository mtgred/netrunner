(ns nr.admin
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [POST GET PUT DELETE]]
            [nr.cardbrowser :refer [non-game-toast] :as cb]
            [nr.utils :refer [render-icons]]
            [nr.ws :refer [ws-send!]]
            [nr.appstate :refer [app-state]]
            [clojure.string :as s]
            [reagent.core :as r]))

(def admin-state (r/atom {}))

(go (swap! admin-state assoc :news (:json (<! (GET "/data/news")))))

(defn- post-data [url callback data]
  (go (let [response (<! (POST url data :json))]
        (callback response))))

(defn- delete-data [url callback]
  (go (let [response (<! (DELETE url))]
        (callback response))))

(defn- update-news-response [response]
  (if (= 200 (:status response))
    (do
      (go (swap! admin-state assoc :news (:json (<! (GET "/data/news")))))
      (non-game-toast "Updated news items" "success" nil))
    (non-game-toast "Failed to update news items" "error" nil)))

(defn- delete-news-item [id]
  (delete-data (str "/admin/news/" id) update-news-response))

(defn- post-news-item [msg]
  (post-data "/admin/news" update-news-response {:item msg}))

(defn admin-container []
  (r/with-let [news (r/cursor admin-state [:news])]
    (let [s (r/atom {})]
      (fn []
        [:div.container
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
                [:span.date (-> (:date d) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]
                [:span.title (render-icons (:item d ""))]]))]]
         [:h4 "Add news item"]
         [:form.msg-box {:on-submit #(let [msg (:news-msg @s "")]
                                       (.preventDefault %)
                                       (when-not (s/blank? msg)
                                         (post-news-item msg)
                                         (swap! s assoc :news-msg "")
                                         ))}
          [:input {:type "text"
                   :placeholder "Post something...."
                   :value (:news-msg @s)
                   :on-change #(swap! s assoc :news-msg (-> % .-target .-value))}]
          (let [msg (:news-msg @s "")
                disabled (s/blank? msg)]
            [:button {:disabled disabled
                      :class (if disabled "disabled" "")}
             "Post"])]
         ]))))

(defn admin []
  (r/with-let [user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])]
    (when (and (= "/admin" (first @active))
               (:isadmin @user))
      [admin-container])))
