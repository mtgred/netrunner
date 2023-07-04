(ns nr.features
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [nr.ajax :refer [GET]]
   [nr.appstate :refer [app-state]]
   [reagent.core :as r]))

(def feature-state (r/atom {}))

;;
;; (defn- update-version-response [response]
;;   (if (= 200 (:status response))
;;     (do
;;       (go (swap! admin-state assoc :version (:json (<! (GET "/admin/version")))))
;;       (non-game-toast "Updated version" "success" nil))
;;     (non-game-toast "Failed to update version" "error" nil)))
;;
;; (defn- update-version-item [msg]
;;   (go (let [response (<! (PUT "/admin/version" {:version msg} :json))]
;;         (update-version-response response))))
;;

(defn- feature-container [_state]
  (r/with-let [_s (r/atom {})]
    [:div.container.panel.blue-shade.content-page
     [:h3 "Site Features"]
     [:p "Not implemented yet"]
     @feature-state
     ]))
;; [:div
;;  [:label [:input {:type "checkbox"
;;                   :value true
;;                   :checked (:disable-ws-buffer @state)
;;                   :on-change #(swap! state assoc-in [:disable-ws-buffer] (.. % -target -checked))}]
;;   "Disable websocket buffering"
;;   ]]])


(defn features []
  (let [user (r/cursor app-state [:user])]

    (go (when (:isadmin (:user @app-state))
          (reset! feature-state (:features (:json (<! (GET "/admin/features")))))))

    (fn []
      [:div.page-container
       [:div.help-bg]
       (when (:isadmin @user)
         [feature-container feature-state])])))
