(ns netrunner.chat
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]))

(def app-state
  (atom
    {:active-channel :general
     :channels {:general ["foobar" "spam eggs"]
                :belgium ["Vive la frite !" "On aime la biere ici."]}}))

(defn msg-input-view [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
       (dom/input nil)
       (dom/button #js {:onClick #(.log js/console "Sending")} "Send")))))

(defn channel-view [channel owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil (str "#" (name channel))))))

(defn channel-list-view [app owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (apply dom/ul #js {:className "blue-shade panel"}
             (om/build-all channel-view (keys (:channels app)))))))

(defn message-view [message owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil (str message)))))

(defn message-list-view [messages owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (apply dom/ul #js {:className "blue-shade panel"} (om/build-all message-view messages)))))

(defn chat-app [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
       (om/build channel-list-view app)
       (om/build message-list-view (get-in app [:channels (:active-channel app)]))
       (om/build msg-input-view app)))))

(om/root chat-app app-state {:target (. js/document (getElementById "chat"))})

;; (swap! app-state assoc-in [:channels :France] [])
;; (swap! app-state assoc :active-channel :belgium)
