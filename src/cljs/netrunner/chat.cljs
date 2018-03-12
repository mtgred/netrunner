(ns netrunner.chat
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :as s]
            [netrunner.appstate :refer [app-state]]
            [netrunner.auth :refer [avatar authenticated] :as auth]
            [netrunner.gameboard :refer [card-preview-mouse-over card-preview-mouse-out get-message-parts create-span card-zoom] :as gameboard]
            [netrunner.ajax :refer [GET PUT]]
            [netrunner.ws :as ws]))

(declare fetch-messages)

(enable-console-print!)

(def chat-channel (chan))

(ws/register-ws-handler!
  :chat/message
  (partial put! chat-channel))

(defn current-block-list [] (get-in @app-state [:options :blocked-users] nil))

(defn filter-blocked-messages
  [messages]
  (if-let [blocked-users current-block-list]
    (filter #(= -1 (.indexOf blocked-users (:username %))) messages)
    messages))

(defn update-message-channel
  [channel messages]
  (swap! app-state assoc-in [:channels channel] (filter-blocked-messages messages)))

(go (while true
      (let [msg (<! chat-channel)
            ch (keyword (:channel msg))
            messages (get-in @app-state [:channels ch])]
        (update-message-channel ch (reverse (conj (reverse messages) msg))))))

(defn- post-response [owner blocked-user response]
  (if (= 200 (:status response))
    (netrunner.gameboard/toast (str "Blocked user " blocked-user ". Refresh browser to update.") "success" nil)
    (netrunner.gameboard/toast "Failed to block user" "error" nil)))

(defn block-user
  [owner blocked-user]
  (authenticated
    (fn [user]
      (let [my-user-name (:username user)
            current-blocked-list (current-block-list)]
        (when (and (not (s/blank? blocked-user))
                   (not= my-user-name blocked-user)
                   (and current-blocked-list (= -1 (.indexOf current-blocked-list blocked-user))))
          (let [new-block-list (conj current-blocked-list blocked-user)]
            (swap! app-state assoc-in [:options :blocked-users] new-block-list)
            (netrunner.account/post-options "/profile" (partial post-response owner blocked-user))))))))

(defn send-msg [event channel owner]
  (.preventDefault event)
  (authenticated
   (fn [user]
     (let [input (om/get-node owner "msg-input")
           text (.-value input)
           $div (js/$ ".chat-app .message-list")]
       (when-not (empty? text)
         (ws/ws-send! [:chat/say {:channel   (name channel)
                                  :msg       text
                                  :username  (:username user)
                                  :emailhash (:emailhash user)}])
         (.scrollTop $div (+ (.prop $div "scrollHeight") 500))
         (aset input "value" "")
         (.focus input))))))

(defn msg-input-view [{:keys [channel]} owner]
  (om/component
   (sab/html
    [:form.msg-box {:on-submit #(send-msg % channel owner)}
     [:input {:type "text" :ref "msg-input" :placeholder "Say something..." :accessKey "l"}]
     [:button "Send"]])))

(defn channel-view [{:keys [channel active-channel]} owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.block-link {:class (if (= active-channel channel) "active" "")
                         :on-click #(put! (:channel-ch state) channel)}
        (str "#" (name channel))]))))

(defn message-view [message owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (sab/html
        [:div.message
         (om/build avatar message {:opts {:size 38}})
         [:div.content
          [:div
           [:span.username (:username message)]
           (when-let [user (:user @app-state)]
             (when (not= (:username message) (:username user))
               [:button.block-user {:on-click #(block-user owner (:username message))
                                    :title "Hide messages and games from this user"} "❌"]))
           [:span.date (-> (:date message) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]]
          [:div
           {:on-mouse-over #(card-preview-mouse-over % (:zoom-ch state))
            :on-mouse-out  #(card-preview-mouse-out % (:zoom-ch state))}
           (for [item (get-message-parts (:msg message))]
               (create-span item))]]]))))

(defn fetch-messages [owner]
  (let [channel (om/get-state owner :channel)
        messages (get-in @app-state [:channels channel])]
    (when (empty? messages)
      (go (let [x (<! (GET (str "/messages/" (name channel))))
                data (:json x)]
            (update-message-channel channel data))))))

(defn chat [cursor owner]
  (reify
    om/IInitState
    (init-state [this] {:channel :general
                        :channel-ch (chan)
                        :zoom false
                        :zoom-ch (chan)
                        :scrolling false})

    om/IWillMount
    (will-mount [this]
      (fetch-messages owner)
      (go (while true
            (let [ch (<! (om/get-state owner :channel-ch))]
              (om/set-state! owner :scrolling false)
              (om/set-state! owner :channel ch))))
      (go (while true
            (let [card (<! (om/get-state owner :zoom-ch))]
              (om/set-state! owner :zoom card)))))

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (fetch-messages owner)
      (let [curr-channel (om/get-state owner :channel)
            prev-channel (:channel prev-state)
            curr-msg-count (count (get-in cursor [:channels curr-channel]))
            prev-msg-count (count (get-in prev-props [:channels curr-channel]))
            curr-page (:active-page cursor)
            prev-page (:active-page prev-props)
            is-scrolled (om/get-state owner :scrolling)
            div (om/get-node owner "message-list")
            scroll-top (.-scrollTop div)
            scroll-height (.-scrollHeight div)]
        (when (or (and (zero? scroll-top)
                       (not is-scrolled))
                  (not= curr-page prev-page)
                  (not= curr-channel prev-channel)
                  (and (not= curr-msg-count prev-msg-count)
                       (not is-scrolled)))
          (aset div "scrollTop" scroll-height))))

    om/IRenderState
    (render-state [this state]
      (sab/html
         [:div.chat-app
          [:div.blue-shade.panel.channel-list
           [:h4 "Channels"]
           (for [ch [:general :america :europe :asia-pacific :united-kingdom :français :español :italia :polska
                     :português :sverige :stimhack-league :русский]]
             (om/build channel-view {:channel ch :active-channel (:channel state)}
                       {:init-state {:channel-ch (:channel-ch state)}}))]
          [:div.chat-container
           [:div.chat-card-zoom
            (when-let [card (om/get-state owner :zoom)]
              (om/build card-zoom card))]
           [:div.chat-box
            [:div.blue-shade.panel.message-list {:ref "message-list"
                                                 :on-scroll #(let [currElt (.-currentTarget %)
                                                                   scroll-top (.-scrollTop currElt)
                                                                   scroll-height (.-scrollHeight currElt)
                                                                   client-height (.-clientHeight currElt)
                                                                   scrolling (< (+ scroll-top client-height) scroll-height)]
                                                               (om/set-state! owner :scrolling scrolling))}
             (if (not (:cards-loaded cursor))
               [:h4 "Loading cards..."]
               (om/build-all message-view (get-in cursor [:channels (:channel state)])
                             {:init-state {:zoom-ch (:zoom-ch state)}}))
             ]
            (when (:user @app-state)
              [:div
               (om/build msg-input-view state)])]]]))))

(om/root chat app-state {:target (. js/document (getElementById "chat"))})
