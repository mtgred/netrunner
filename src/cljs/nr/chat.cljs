(ns nr.chat
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<! chan put!] :as async]
   [clojure.string :refer [lower-case] :as s]
   [jinteki.utils :refer [superuser?]]
   [nr.account :as account]
   [nr.ajax :refer [GET]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [authenticated] :as auth]
   [nr.avatar :refer [avatar]]
   [nr.cardbrowser :refer [image-url]]
   [nr.gameboard.card-preview :refer [card-preview-mouse-out
                                      card-preview-mouse-over]]
   [nr.news :refer [news]]
   [nr.translations :refer [tr tr-pronouns]]
   [nr.utils :refer [non-game-toast render-message set-scroll-top
                     store-scroll-top format-date-time day-word-with-time-formatter]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(defonce chat-state (atom {}))

(def chat-channel (chan))
(def delete-msg-channel (chan))
(def delete-all-channel (chan))

(go (swap! chat-state assoc :config (:json (<! (GET "/chat/config")))))

(defmethod ws/event-msg-handler :chat/message [{data :?data}] (put! chat-channel data))
(defmethod ws/event-msg-handler :chat/delete-msg [{data :?data}] (put! delete-msg-channel data))
(defmethod ws/event-msg-handler :chat/delete-all [{data :?data}] (put! delete-all-channel data))

(defmethod ws/event-msg-handler :chat/blocked
  [{{:keys [reason]} :?data}]
  (let [reason-str (case reason
                     :rate-exceeded (tr [:chat_rate-exceeded "Rate exceeded"])
                     :length-exceeded (tr [:chat_length-exceeded "Length exceeded"]))]
    (non-game-toast (tr [:chat_message-blocked] {:reason-str reason-str})
                    "warning" nil)))

(defn current-block-list []
  (get-in @app-state [:options :blocked-users] []))

(defn filter-blocked-messages
  [messages]
  (let [user (:user @app-state)]
    (filter #(or (superuser? user)
                 (= -1 (.indexOf (current-block-list) (:username %))))
            messages)))

(defn update-message-channel
  [channel messages]
  (swap! app-state assoc-in [:channels channel] (filter-blocked-messages messages)))

(defn filter-message-channel
  [channel k v]
  (let [messages (get-in @app-state [:channels channel])
        filtered (remove #(= v (k %)) messages)]
    (update-message-channel channel filtered)))

(go (while true
      (let [msg (<! chat-channel)
            ch (keyword (:channel msg))
            messages (get-in @app-state [:channels ch])]
        (update-message-channel ch (reverse (conj (reverse messages) msg))))))

(go (while true
      (let [msg (<! delete-msg-channel)
            ch (keyword (:channel msg))
            id (:_id msg)]
        (filter-message-channel ch :_id id))))

(go (while true
      (let [msg (<! delete-all-channel)
            username (:username msg)
            channels (keys (:channels @app-state))]
        (doseq [ch channels]
          (filter-message-channel ch :username username)))))

(defn- post-response [blocked-user response]
  (if (= 200 (:status response))
    (non-game-toast (str "Blocked user " blocked-user ". Refresh browser to update.") "success" nil)
    (non-game-toast "Failed to block user" "error" nil)))

(defn- delete-message
  [message]
  (authenticated
    (fn [_]
      (ws/ws-send! [:chat/delete-msg {:msg message}]))))

(defn- delete-all-messages
  [username]
  (authenticated
    (fn [_]
      (ws/ws-send! [:chat/delete-all {:sender username}]))))

(defn block-user
  [blocked-user]
  (authenticated
    (fn [user]
      (let [my-user-name (:username user)
            current-blocked-list (current-block-list)]
        (when (and (not (s/blank? blocked-user))
                   (not= my-user-name blocked-user)
                   (= -1 (.indexOf current-blocked-list blocked-user)))
          (let [new-block-list (conj current-blocked-list blocked-user)]
            (swap! app-state assoc-in [:options :blocked-users] new-block-list)
            (account/post-options (partial post-response blocked-user))))))))

(defn send-msg [s channel]
  (authenticated
    (fn [user]
      (let [input (:msg-input @chat-state)
            text (:msg @s)]
        (when-not (empty? text)
          (ws/ws-send! [:chat/say {:channel   (name channel)
                                   :msg       text
                                   :username  (:username user)
                                   :emailhash (:emailhash user)}])
          (let [msg-list (:message-list @chat-state)]
            (set! (.-scrollTop msg-list) (+ (.-scrollHeight msg-list) 500)))
          (swap! s assoc :msg "")
          (.focus input))))))

(defn- illegal-message
  [s]
  (let [msg (:msg @s "")
        msg-len (.-length msg)
        max-len (get-in @chat-state [:config :max-length])]
   (or (s/blank? msg)
       (and max-len
            (>= msg-len max-len)))))

(defn msg-input-view [channel s]
  [:form.msg-box {:on-submit #(do (.preventDefault %)
                                  (when-not (illegal-message s)
                                    (send-msg s channel)))}
   [:input {:type "text" :ref #(swap! chat-state assoc :msg-input %)
            :placeholder (tr [:chat_placeholder "Say something..."]) :accessKey "l" :value (:msg @s)
            :on-change #(swap! s assoc :msg (-> % .-target .-value))}]
   (let [disabled (illegal-message s)]
     [:button {:disabled disabled
               :class (if disabled "disabled" "")}
      (tr [:chat_send "Send"])])])

(defn channel-view [{:keys [channel active-channel]} s]
  [:div.block-link {:class (if (= active-channel channel) "active" "")
                    :on-click #(do (swap! s assoc :scrolling false)
                                   (swap! s assoc :channel
                                          (keyword (s/replace (-> % .-target .-innerHTML) #"#" ""))))}
   (str "#" (name channel))])

(defn- hide-block-menu [msg-state]
  (-> (:msg-buttons @msg-state) js/$ .hide))

(defn message-view [message _]
  (let [msg-state (atom {})
        user (:user @app-state)
        my-msg (= (:username message) (:username user))]
    (fn [message s]
      [:div.message
      [avatar message {:opts {:size 38}}]
      [:div.content
       [:div.name-menu
        [:span.username
         {:on-click #(-> (:msg-buttons @msg-state) js/$ .toggle)
          :class (if my-msg "" "clickable")}
         (:username message)]

        (when-let [pronouns (:pronouns message)]
          (let [pro-str (if (= "blank" pronouns) "" (str "(" (tr-pronouns pronouns) ")"))]
            [:span.pronouns (lower-case pro-str)]))

        (when user
          (when (not my-msg)
            [:div.panel.blue-shade.block-menu
             {:ref #(swap! msg-state assoc :msg-buttons %)}
             (when (or (:isadmin user) (:ismoderator user))
               [:div {:on-click #(do
                                   (delete-message message)
                                   (hide-block-menu msg-state))} (tr [:chat_delete "Delete Message"])])
             (when (or (:isadmin user) (:ismoderator user))
               [:div {:on-click #(do
                                   (delete-all-messages (:username message))
                                   (hide-block-menu msg-state))} (tr [:chat_delete-all "Delete All Messages From User"])])
             [:div {:on-click #(do
                                 (block-user (:username message))
                                 (hide-block-menu msg-state))} (tr [:chat_block "Block User"])]
             [:div {:on-click #(hide-block-menu msg-state)} (tr [:chat_cancel "Cancel"])]]))
         [:span.date (format-date-time day-word-with-time-formatter (:date message))]]
       [:div
        {:on-mouse-over #(card-preview-mouse-over % (:zoom-ch @s))
         :on-mouse-out  #(card-preview-mouse-out % (:zoom-ch @s))}
        (render-message (:msg message))]]])))

(defn card-zoom-image
  [card]
  [:div.card-preview.blue-shade
   (when-let [url (image-url card)]
     [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}])])

(defn card-zoom [zoom-card]
  (if-let [card @zoom-card]
    (do (-> ".card-zoom" js/$ (.addClass "fade"))
        [card-zoom-image card])
    (do (-> ".card-zoom" js/$ (.removeClass "fade")) nil)))

(defn fetch-all-messages []
  (doseq [channel (keys (:channels @app-state))]
    (go (let [x (<! (GET (str "/messages/" (name channel))))
              data (:json x)]
          (update-message-channel channel data)))))

(fetch-all-messages)

(defn message-panel [s old scroll-top]
  (r/with-let [cards-loaded (r/cursor app-state [:cards-loaded])]
    (r/create-class
      {:display-name "message-panel"
       :component-did-mount #(set-scroll-top % @scroll-top)
       :component-will-unmount #(store-scroll-top % scroll-top)
       :component-did-update
       (fn []
         (when-let [msg-list (:message-list @chat-state)]
           (let [curr-channel (:channel @s)
                 prev-channel (:prev-channel @old)
                 curr-msg-count (count (get-in @app-state [:channels curr-channel]))
                 prev-msg-count (:prev-msg-count @old)
                 curr-page (:active-page @app-state)
                 prev-page (:prev-page @old)
                 is-scrolled (:scrolling @s)]
             (when (or (and (zero? (.-scrollTop msg-list))
                            (not is-scrolled))
                       (not= curr-page prev-page)
                       (not= curr-channel prev-channel)
                       (and (not= curr-msg-count prev-msg-count)
                            (not is-scrolled)))
               (set! (.-scrollTop msg-list) (.-scrollHeight msg-list))
               ; use an atom instead of prev-props as r/current-component is only valid in component functions
               (swap! old assoc :prev-page curr-page)
               (swap! old assoc :prev-channel curr-channel)
               (swap! old assoc :prev-msg-count curr-msg-count)))))

       :reagent-render
       (fn [s _old _scroll-top]
         [:div.blue-shade.panel.message-list {:ref #(swap! chat-state assoc :message-list %)
                                              :on-scroll #(let [currElt (.-currentTarget %)
                                                                scroll-top (.-scrollTop currElt)
                                                                scroll-height (.-scrollHeight currElt)
                                                                client-height (.-clientHeight currElt)
                                                                scrolling (< (+ scroll-top client-height) scroll-height)]
                                                            (swap! s assoc :scrolling scrolling))}
          (if (not @cards-loaded)
            [:h4 "Loading cards..."]
            (let [message-list (get-in @app-state [:channels (:channel @s)])]
              (doall (map-indexed
                       (fn [i message]
                         [:div {:key i}
                          [message-view message s]])
                       message-list))))])})))

(defn chat []
  (let [user (r/cursor app-state [:user])]
    (fn [s curr-msg old scroll-top]
      [:div#chat.chat-app
       [:div.blue-shade.panel.channel-list
        [:h4 (tr [:chat_channels "Channels"])]
        (doall
          (for
            [ch [:general :america :europe :asia-pacific :united-kingdom :français :español :italia :polska
                 :português :sverige :русский]]
            ^{:key ch}
            [channel-view {:channel ch :active-channel (:channel @s)} s]))]
       [:div.chat-container
        [:div.chat-card-zoom
         (when-let [card (:zoom @s)]
           [card-zoom (r/atom card)])]
        [:div.chat-box
         [message-panel s old scroll-top]
         (when @user
           [:div
            [msg-input-view (:channel @s) curr-msg]])]]])))

(defn chat-page []
  (r/with-let [s (r/atom {:channel :general
                          :zoom false
                          :zoom-ch (chan)
                          :scrolling false})
               curr-msg (r/atom {})
               scroll-top (atom 0)
               old (atom {:prev-msg-count 0})] ; old is not a r/atom so we don't render when this is updated

    (go (while true
          (let [card (<! (:zoom-ch @s))]
            (swap! s assoc :zoom card))))

    (fn []
      [:div.container
       [:div.home-bg]
       [:h1 (tr [:chat_title "Play Netrunner in your browser"])]
       [news]
       [chat s curr-msg old scroll-top]
       [:div#version [:span (str "Version " (or (get @app-state :app-version) "Unknown"))]]])))
