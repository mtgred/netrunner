(ns nr.lobby-chat
  (:require
   [nr.appstate :refer [app-state]]
   [nr.avatar :refer [avatar]]
   [nr.translations :refer [tr]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(defn send-msg [state]
  (let [text (:msg @state)]
    (when-not (empty? text)
      (ws/ws-send! [:lobby/say {:gameid (:gameid @app-state)
                                :msg text}])
      (let [msg-list (:message-list @state)]
        (set! (.-scrollTop msg-list) (+ (.-scrollHeight msg-list) 500)))
      (swap! state assoc :msg ""))))

(defn lobby-chat [_messages]
  (let [state (r/atom {})]
    (r/create-class
      {:display-name "lobby-chat"
       :component-did-update
       (fn []
         (let [msg-list (:message-list @state)
               height (.-scrollHeight msg-list)]
           (when (< (- height (.-scrollTop msg-list) (.height (js/$ ".lobby .chat-box"))) 500)
             (set! (.-scrollTop msg-list) (.-scrollHeight msg-list)))))
       :reagent-render
       (fn [messages]
         [:div.chat-box
          [:h3 (tr [:lobby.chat "Chat"])]
          [:div.message-list {:ref #(swap! state assoc :message-list %)}
           (doall
             (map-indexed
               (fn [i msg]
                 (if (= (:user msg) "__system__")
                   ^{:key i}
                   [:div.system (:text msg)]
                   ^{:key i}
                   [:div.message
                    [avatar (:user msg) {:opts {:size 38}}]
                    [:div.content
                     [:div.username (get-in msg [:user :username])]
                     [:div (:text msg)]]]))
               @messages))]
          [:div
           [:form.msg-box {:on-submit #(do (.preventDefault %)
                                           (send-msg state))}
            [:input {:placeholder (tr [:chat.placeholder "Say something"])
                     :type "text"
                     :value (:msg @state)
                     :on-change #(swap! state assoc :msg (-> % .-target .-value))}]
            [:button (tr [:chat.send "Send"])]]]])})))
