(ns nr.features
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [POST GET PUT DELETE]]
            [nr.utils :refer [render-icons non-game-toast]]
            [nr.ws :refer [ws-send!]]
            [nr.appstate :refer [app-state]]
            [clojure.string :as s]
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

(defn- feature-container [state]
  (r/with-let [s (r/atom {})]
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
  (let [user (r/cursor app-state [:user])
        active (r/cursor app-state [:active-page])]

    (go (when (:isadmin (:user @app-state))
          (reset! feature-state (:features (:json (<! (GET "/admin/features")))))))

    (fn []
      (when (and (= "/features" (first @active))
                 (:isadmin @user))
        [:div.page-container
         [feature-container feature-state]]))))
