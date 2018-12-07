(ns nr.integration
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ws :as ws]
            [cljs.core.async :refer [chan put! <!] :as async]
            [nr.auth :refer [authenticated] :as auth]
            [nr.cardbrowser :refer [non-game-toast] :as cb]
            [reagent.core :as r]))

(def integration-state (r/atom {:api-keys nil}))
(def key-channel (chan))

(go (while true
      (let [msg (<! key-channel)]
        (swap! integration-state assoc :api-keys (:keys msg)))))

(defn- send-key-message
  ([k] (send-key-message k {}))
  ([k args]
   (authenticated
     (fn [user]
       (ws/ws-send! [k args])))))

(defn- request-all-keys []
  (send-key-message :integration/list-keys))

(defn- add-key []
  (send-key-message :integration/create-key))

(defn- remove-key [k]
  (send-key-message :integration/delete-key {:_id (:_id k)}))

(defn api-key-view [k]
  [:div.api-key
   [:div
    [:input {:value (:token k) :readOnly true}]
    [:button.clipboard-btn
     {:data-clipboard-text (:token k)
      :onClick #(cb/non-game-toast "Copied key" "success" nil)}
     "Copy"]
    [:button.api-key-remove
     {:on-click #(do
                   (remove-key k)
                   (cb/non-game-toast "Deleted key" "success" nil))}
     "Delete"]]
   [:div
    [:span [:strong "Issued: "]
     [:span.date (-> (:issued k) js/Date. js/moment (.format "Do MMM YYYY"))] ]
    [:span.expires-text
     (if (-> (:expires k) js/Date. js/moment .isBefore)
       {:class "expired"})
     [:strong "Expires: "]
     [:span.date (-> (:expires k) js/Date. js/moment (.format "Do MMM YYYY"))] ]
    ]])

(defn integration []
  (let [api-keys (r/cursor integration-state [:api-keys])]
    (r/create-class
      {:display-name "integration"

       :reagent-render
       (fn []
         [:div.integration.panel.content-page.blue-shade
          [:h3 "3rd Party Integration"]
          [:h4 "API Keys"]
          [:div.api-keys
           (for [k @api-keys]
             ^{:key (:_id k)}
             [:div (api-key-view k)])
           [:div.api-button-div
            [:button.api-key-add {:on-click #(add-key)} "New"]]]])})))

(ws/register-ws-handler! :integration/key-list (partial put! key-channel))
