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
               :belgium ["Vive la frite !" "On aime la biere ici."]}}))

(go (while true
      (let [msg (<! chat-channel)
            ch (keyword (aget msg "channel"))
            text (aget msg "msg")
            messages (get-in @app-state [:channels ch])]
        (swap! app-state assoc-in [:channels ch] (conj messages text)))))

(defn send-msg [app owner]
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (when-not (zero? (alength text))
      (aset input "value" "")
      (.focus input)
      (put! out-channel #js {:type "chat"
                             :channel (name (first (:active-channel @app)))
                             :msg text}))))

(defn msg-input-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.msg-box
        [:input {:type "text" :ref "msg-input" :placeholder "Say something..."
                 :onKeyPress #(when (== (.-keyCode %) 13) (send-msg app owner))}]
        [:button {:on-click #(send-msg app owner)} "Send"]]))))

(defn channel-view [cursor owner]
  (reify
    om/IRender
    (render [this]
      (let [ch (:channel cursor)
            active-ch (:active-channel cursor)]
       (sab/html
        [:div {:class (if (= (first active-ch) ch) "active" "")
               :on-click #(om/update! active-ch [ch])}
         (str "#" (name ch))])))))

(defn channel-list-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.blue-shade.panel.channel-list
        (for [ch (keys (:channels app))]
          (om/build channel-view {:channel ch
                                  :active-channel (:active-channel app)}))]))))

(defn message-view [message owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div {} (str message)]))))

(defn message-list-view [messages owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.blue-shade.panel.message-list
        (om/build-all message-view messages)]))))

(defn chat-app [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.chat-app
        (om/build channel-list-view app)
        [:div.chat-box
         (om/build message-list-view (get-in app [:channels (first (:active-channel app))]))
         (om/build msg-input-view app)]]))))

(om/root chat-app app-state {:target (. js/document (getElementById "chat"))})
