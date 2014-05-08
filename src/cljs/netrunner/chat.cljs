(ns netrunner.chat
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.socket :refer [out-channel chat-channel]]))

(def app-state
  (atom
   {:active-channel [:general]
    :channels {:general ["foobar" "spam eggs"]
               :belgium ["Vive la frite !" "On aime la biere ici."]
               :france ["Vive le pinard" "Et le petit jaune!"]}}))

(go (while true
      (let [msg (<! chat-channel)
            ch (keyword (aget msg "channel"))
            text (aget msg "msg")
            messages (get-in @app-state [:channels ch])]
        (swap! app-state assoc-in [:channels ch] (conj messages text)))))

(defn send-msg [cursor owner]
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (when-not (zero? (alength text))
      (aset input "value" "")
      (.focus input)
      (put! out-channel #js {:type "chat"
                             :channel (name (first (:active-channel @cursor)))
                             :msg text}))))

(defn msg-input-view [cursor owner]
  (om/component
   (sab/html
    [:div.msg-box
     [:input {:type "text" :ref "msg-input" :placeholder "Say something..."
              :onKeyPress #(when (== (.-keyCode %) 13) (send-msg cursor owner))}]
     [:button {:on-click #(send-msg cursor owner)} "Send"]])))

(defn channel-view [{:keys [channel active-channel]} owner]
  (om/component
   (sab/html
    [:div {:class (if (= (first active-channel) channel) "active" "")
           :on-click #(om/update! active-channel [channel])}
     (str "#" (name channel))])))

(defn channel-list-view [{:keys [channels active-channel]} owner]
  (om/component
   (sab/html
    [:div.blue-shade.panel.channel-list {}
     [:button.add {} "+"]
     [:h4 {} "Channels"]
     (for [ch (keys channels)]
       (om/build channel-view {:channel ch :active-channel active-channel}))])))

(defn message-view [message owner]
  (om/component
   (sab/html [:div {} (str message)])))

(defn message-list-view [messages owner]
  (om/component
   (sab/html
    [:div.blue-shade.panel.message-list
     (om/build-all message-view messages)])))

(defn chat-app [cursor owner]
  (om/component
   (sab/html
    [:div.chat-app
     (om/build channel-list-view cursor)
     [:div.chat-box
      (om/build message-list-view (get-in cursor [:channels (first (:active-channel cursor))]))
      (om/build msg-input-view cursor)]])))

(om/root chat-app app-state {:target (. js/document (getElementById "chat"))})
