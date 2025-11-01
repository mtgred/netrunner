(ns nr.lobby-chat
  (:require
   [nr.avatar :refer [avatar]]
   [nr.translations :refer [tr tr-element]]
   [nr.ws :as ws]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(defn send-message [state current-game]
  (let [text (:msg @state)]
    (when (and (string? text) (not-empty text))
      (ws/ws-send! [:lobby/say {:gameid (:gameid @current-game)
                                :text text}])
      (swap! state assoc :should-scroll true)
      (swap! state assoc :msg ""))))

(defn scrolled-to-end?
  [el tolerance]
  (> tolerance (- (.-scrollHeight el) (.-scrollTop el) (.-clientHeight el))))

(defn lobby-chat [_current-game _messages]
  (r/with-let [state (r/atom {:message-list nil
                              :msg ""
                              :should-scroll false})
               message-list (r/cursor state [:message-list])
               current-input (r/cursor state [:msg])
               should-scroll (r/cursor state [:should-scroll])]
    (r/create-class
      {:display-name "lobby-chat"
       :component-did-mount
       (fn []
         (when-let [el @message-list]
           (set! (.-scrollTop el) (.-scrollHeight el))))
       :component-did-update
       (fn []
         (when-let [el @message-list]
           (when (or @should-scroll
                     (scrolled-to-end? el 15))
             (swap! state assoc :should-scroll false)
             (set! (.-scrollTop el) (.-scrollHeight el)))))
       :reagent-render
       (fn [current-game messages]
         [:div.chat-box
          [tr-element :h3 [:lobby_chat "Chat"]]
          (into [:div.message-list {:ref #(swap! state assoc :message-list %)}]
                (map
                  (fn [{:keys [user text timestamp]}]
                    (if (= user "__system__")
                      [:div.system {:key timestamp} text]
                      [:div.message {:key timestamp}
                       [avatar user {:opts {:size 38}}]
                       [:div.content
                        [:div.username (:username user)]
                        [:div text]]]))
                  @messages))
          [:div
           [:form.msg-box {:on-submit #(do (.preventDefault %)
                                           (send-message state current-game))}
            [:input {:placeholder (tr [:chat_placeholder "Say something..."])
                     :data-i18n-key :chat_placeholder
                     :type "text"
                     :value @current-input
                     :on-change #(swap! state assoc :msg (-> % .-target .-value))}]
            [tr-element :button  [:chat_send "Send"]]]]])})))
