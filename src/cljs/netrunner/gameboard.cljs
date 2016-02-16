(ns netrunner.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize lower-case]]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [avatar] :as auth]
            [netrunner.cardbrowser :refer [image-url add-symbols] :as cb]
            [differ.core :as differ]
            [om.dom :as dom]))

(defonce game-state (atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defn toastr-options
  "Function that generates the correct toastr options for specified settings"
  [options]
  (js-obj "closeButton" (:close-button options false)
          "debug" false
          "newestOnTop" false
          "progressBar" false
          "positionClass" "toast-card"
          ;; preventDuplicates - identical toasts don't stack when the property is set to true.
          ;; Duplicates are matched to the previous toast based on their message content.
          "preventDuplicates" (:prevent-duplicates options true)
          "onclick" nil
          "showDuration" 300
          "hideDuration" 1000
          ;; timeOut - how long the toast will display without user interaction
          "timeOut" (:time-out options 3000)
          ;; extendedTimeOut - how long the toast will display after a user hovers over it
          "extendedTimeOut" (:time-out options 1000)
          "showEasing" "swing"
          "hideEasing" "linear"
          "showMethod" "fadeIn"
          "hideMethod" "fadeOut"))

(defn init-game [game side]
  (.setItem js/localStorage "gameid" (:gameid @app-state))
  (swap! game-state merge game)
  (swap! game-state assoc :side side)
  (swap! last-state #(identity @game-state)))

(declare toast)

(defn notify
  "Send a notification to the chat, and a toast to the current player of the specified severity"
  [text severity]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text}))
  (toast text severity nil))

(def zoom-channel (chan))
(def socket (.connect js/io (str js/iourl "/lobby")))
(def socket-channel (chan))
(.on socket "netrunner" #(put! socket-channel (js->clj % :keywordize-keys true)))
(.on socket "disconnect" #(notify "Connection to the server lost. Attempting to reconnect."
                                  "error"))
(.on socket "reconnect" #(when (.-onbeforeunload js/window)
                           (notify "Reconnected to the server." "success")
                           (.emit socket "netrunner" #js {:action "reconnect" :gameid (:gameid @app-state)})))

(def anr-icons {"[Credits]" "credit"
                "[$]" "credit"
                "[c]" "credit"
                "[Credit]" "credit"
                "[Click]" "click"
                "[Subroutine]" "subroutine"
                "[Recurring Credits]" "recurring-credit"
                "1[Memory Unit]" "mu1"
                "1[mu]" "mu1"
                "2[Memory Unit]" "mu2"
                "2[mu]" "mu2"
                "3[Memory Unit]" "mu3"
                "3[mu]" "mu3"
                "[Link]" "link"
                "[l]" "link"
                "[Memory Unit]" "mu"
                "[mu]" "mu"
                "[Trash]" "trash"
                "[t]" "trash"})

(go (while true
      (let [msg (<! socket-channel)]
        (reset! lock false)
        (case (:type msg)
          ("do" "notification" "quit") (do (swap! game-state (if (:diff msg) #(differ/patch @last-state (:diff msg))
                                                                             #(assoc (:state msg) :side (:side @game-state))))
                                           (swap! last-state #(identity @game-state)))
          nil))))

(defn send [msg]
  (.emit socket "netrunner" (clj->js msg)))

(defn not-spectator? [game-state app-state]
  (#{(get-in @game-state [:corp :user]) (get-in @game-state [:runner :user])} (:user @app-state)))

(defn send-command
  ([command] (send-command command nil))
  ([command args]
     (when-not @lock
       (try (js/ga "send" "event" "game" command) (catch js/Error e))
       (reset! lock true)
       (send {:action "do" :gameid (:gameid @game-state) :side (:side @game-state)
              :user (:user @app-state)
              :command command :args args}))))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)
        $div (js/$ ".gameboard .messages")]
    (when-not (empty? text)
      (send-command "say" {:user (:user @app-state) :text text})
      (.scrollTop $div (+ (.prop $div "scrollHeight") 500))
      (aset input "value" "")
      (.focus input))))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr type)]
    (f msg))
  (send-command "toast"))

(defn action-list [{:keys [type zone rezzed advanceable advance-counter advancementcost current-cost] :as card}]
  (-> []
      (#(if (and (= type "Agenda") (>= advance-counter current-cost))
          (cons "score" %) %))
      (#(if (or (and (= type "Agenda")
                     (= (first zone) "servers"))
                (= advanceable "always")
                (and rezzed
                     (= advanceable "while-rezzed"))
                (and (not rezzed)
                     (= advanceable "while-unrezzed")))
          (cons "advance" %) %))
      (#(if (#{"Asset" "ICE" "Upgrade"} type)
          (if-not rezzed (cons "rez" %) (cons "derez" %))
          %))))

(defn handle-abilities [{:keys [abilities facedown side] :as card} owner]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))]
    (when-not (and (= side "Runner") facedown)
      (cond (or (> c 1)
                (= (first actions) "derez")) (-> (om/get-node owner "abilities") js/$ .toggle)
            (= c 1) (if (= (count abilities) 1)
                          (send-command "ability" {:card card :ability 0})
                          (send-command (first actions) {:card card}))))))

(defn handle-card-click [{:keys [type zone counter advance-counter advancementcost advanceable
                                 root] :as card} owner]
  (let [side (:side @game-state)]
    (when (not-spectator? game-state app-state)
      (if (= (get-in @game-state [side :prompt 0 :prompt-type]) "select")
        (send-command "select" {:card card})
        (if (and (= (:type card) "Identity") (= side (keyword (.toLowerCase (:side card)))))
          (handle-abilities card owner)
          (if (= side :runner)
            (case (first zone)
              "hand" (if (:host card)
                       (when (:installed card)
                         (handle-abilities card owner))
                       (send-command "play" {:card card}))
              ("rig" "current" "onhost" "play-area") (handle-abilities card owner)
              nil)
            (case (first zone)
              "hand" (case type
                       ("Upgrade" "ICE") (if root
                                           (send-command "play" {:card card :server root})
                                           (-> (om/get-node owner "servers") js/$ .toggle))
                       ("Agenda" "Asset") (if (< (count (get-in @game-state [:corp :servers])) 4)
                                            (send-command "play" {:card card :server "New remote"})
                                            (-> (om/get-node owner "servers") js/$ .toggle))
                       (send-command "play" {:card card}))
              ("servers" "scored" "current" "onhost") (handle-abilities card owner)
              nil)))))))

(defn in-play? [card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @game-state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn playable? [{:keys [title side zone cost type uniqueness abilities] :as card}]
  (let [my-side (:side @game-state)
        me (my-side @game-state)]
    (and (= (keyword (.toLowerCase side)) my-side)
         (and (= zone ["hand"])
              (or (not uniqueness) (not (in-play? card)))
              (or (#{"Agenda" "Asset" "Upgrade" "ICE"} type) (>= (:credit me) cost))
              (pos? (:click me))))))

(defn is-card-item [item]
  (and (> (.indexOf item "~") -1)
       (= 0 (.indexOf item "["))))

(defn extract-card-info [item]
  (if (is-card-item item)
    [(.substring item 1 (.indexOf item "~"))
     (.substring item (inc (.indexOf item "~")) (dec (count item)))]))

(defn create-span-impl [item]
  (if (= "[hr]" item)
    [:hr ]
  (if (= "[!]" item)
    [:div.smallwarning "!"]
  (if-let [class (anr-icons item)]
    [:span {:class (str "anr-icon " class)}]
  (if-let [[title code] (extract-card-info item)]
    [:span {:class "fake-link" :id code} title]
    [:span item])))))

(defn get-alt-art [[title cards]]
  (let [s (sort-by #(not= (:setname %) "Alternates") cards)]
    {:title title :code (:code (first s))}))

(defn prepare-cards []
 (->> (:cards @app-state)
      (group-by :title)
      (map get-alt-art)
      (sort-by #(count (:title %1)))
      (reverse)))

(def prepared-cards (memoize prepare-cards))

(def create-span (memoize create-span-impl))

(defn add-image-codes-impl [text]
  (reduce #(.replace %1 (js/RegExp. (str "(^|[^\\[\\S])" (:title %2) "(?![~\\w])") "g") (str "$1" "[" (:title %2) "~" (:code %2) "]")) text (prepared-cards)))

(def add-image-codes (memoize add-image-codes-impl))

(defn get-message-parts-impl [text]
  (let [with-image-codes (add-image-codes (if (nil? text) "" text))]
      (.split with-image-codes (js/RegExp. "(\\[[^\\]]*])" "g"))))

(def get-message-parts (memoize get-message-parts-impl))

(defn get-card-code [e]
  (let [code (str (.. e -target -id))]
    (when (pos? (count code))
      code)))

(defn card-preview-mouse-over [e]
  (when-let [code (get-card-code e)] (put! zoom-channel {:code code})))

(defn card-preview-mouse-out [e]
  (when-let [code (get-card-code e)] (put! zoom-channel false)))

(defn log-pane [messages owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (let [div (om/get-node owner "msg-list")
            scrolltop (.-scrollTop div)
            height (.-scrollHeight div)]
        (when (or (zero? scrolltop)
                  (< (- height scrolltop (.height (js/$ ".gameboard .log"))) 500))
          (aset div "scrollTop" height))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.log { :on-mouse-over card-preview-mouse-over
                   :on-mouse-out  card-preview-mouse-out  }
        [:div.messages.panel.blue-shade {:ref "msg-list"}
         (for [msg messages]
           (if (= (:user msg) "__system__")
             [:div.system (for [item (get-message-parts (:text msg))] (create-span item))]
             [:div.message
              (om/build avatar (:user msg) {:opts {:size 38}})
              [:div.content
               [:div.username (get-in msg [:user :username])]
               [:div (for [item (get-message-parts (:text msg))] (create-span item))]]]))]
        [:form {:on-submit #(send-msg % owner)}
         [:input {:ref "msg-input" :placeholder "Say something" :accessKey "l"}]]]))))

(defn handle-dragstart [e cursor]
  (-> e .-target js/$ (.addClass "dragged"))
  (-> e .-dataTransfer (.setData "card" (.stringify js/JSON (clj->js @cursor)))))

(defn handle-drop [e server]
  (-> e .-target js/$ (.removeClass "dragover"))
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))
        side (if (#{"HQ" "R&D" "Archives"} server) "Corp" "Runner")]
    (send-command "move" {:card card :server server})))

(defn abs [n] (max n (- n)))

;; touch support
(defonce touchmove (atom {}))

(defn release-touch [card]
  (-> card (.removeClass "disable-transition"))
  (-> card (.css "position" ""))
  (-> card (.css "top" "")))

(defn update-card-position [card touch]
  (-> card (.css "left" (str (- (int (aget touch "pageX")) 30) "px")))
  (-> card (.css "top"  (str (- (int (aget touch "pageY")) 42) "px"))))

(defn get-card [e server]
  (-> e .-target js/$ (.closest ".card-wrapper")))

(defn get-server-from-touch [touch]
  (let [cX (.. touch -clientX)
        cY (.. touch -clientY)
        server (-> (js/document.elementFromPoint cX cY)
                   js/$
                   (.closest "[data-server]")
                   (.attr "data-server"))]
    [server (> (+ (abs (- (:x @touchmove) cX))
                  (abs (- (:y @touchmove) cY)))
               30)]))

(defn handle-touchstart [e cursor]
  (let [touch (aget (.. e -targetTouches) 0)
        [server _] (get-server-from-touch touch)
        card (get-card e server)]
    (-> card (.addClass "disable-transition"))
    (reset! touchmove {:card (.stringify js/JSON (clj->js @cursor))
                       :x (.. touch -clientX)
                       :y (.. touch -clientY)
                       :start-server server})))

(defn handle-touchmove [e]
  (let [touch (aget (.. e -targetTouches) 0)
        card (get-card e (:start-server @touchmove))]
    (-> card (.css "position" "fixed"))
    (update-card-position card touch)))

(defn handle-touchend [e]
  (let [touch (aget (.. e -changedTouches) 0)
        card (get-card e (:start-server @touchmove))
        [server moved-enough] (get-server-from-touch touch)]
    (release-touch card)
    (when (and server moved-enough (not= server (:start-server @touchmove)))
      (let [cardinfo (-> @touchmove :card ((.-parse js/JSON)) (js->clj :keywordize-keys true))]
        (send-command "move" {:card cardinfo :server server})))))

(defn ability-costs [ab]
  (when-let [cost (:cost ab)]
    (str (clojure.string/join
           ", " (for [c (partition 2 cost)]
                  (str (case (first c)
                         "credit" (str (second c) " [" (capitalize (name (first c))) "]")
                         (clojure.string/join "" (repeat (second c) (str "[" (capitalize (name (first c))) "]")))
                         )))) ": ")))

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last js/parseInt))

(defn remote->name [server]
  (let [num (remote->num server)]
    (str "Server " num)))

(defn get-remotes [servers]
 (->> servers 
     (filter #(not (#{:hq :rd :archives} (first %))))
     (sort-by #(remote->num (first %)))))

(defn remote-list [remotes]
  (->> remotes (map #(remote->name (first %))) (sort-by #(remote->num (first %)))))

(defn card-counter-type [card]
  (let [counter-type (:counter-type card)]
    ;; Determine the appropriate type of counter for styling, falling back to
    ;; power counters when no other type can be inferred.
    (cond
      ;; If an installed card contains an annotation, use it.
      (and (:installed card)
           (not (nil? counter-type)))
        counter-type
      (= "Agenda" (:type card)) "Agenda"
      ;; Assume uninstalled cards with counters are hosted on Personal
      ;; Workshop.
      (not (:installed card)) "Power"
      (not (:subtype card)) "Power"
      (> (.indexOf (:subtype card) "Virus") -1) "Virus"
      :else "Power")))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [code title] :as cursor}]
  (om/component
   (when code
     (sab/html
      [:div.card-frame
       [:div.blud-shade.card {:on-mouse-enter #(put! zoom-channel cursor)
                              :on-mouse-leave #(put! zoom-channel false)}
        (when-let [url (image-url cursor)]
          [:div
           [:span.cardname title]
           [:img.card.bg {:src url :onError #(-> % .-target js/$ .hide)}]])]]))))

(defn card-view [{:keys [zone code type abilities counter advance-counter advancementcost current-cost subtype
                         advanceable rezzed strength current-strength title remotes selected hosted
                         side rec-counter facedown named-target icon]
                  :as cursor}
                 owner {:keys [flipped] :as opts}]
  (om/component
   (when code
     (sab/html
      [:div.card-frame
       [:div.blue-shade.card {:class (when selected "selected")
                              :draggable (when (not-spectator? game-state app-state) true)
                              :on-touch-start #(handle-touchstart % cursor)
                              :on-touch-end   #(handle-touchend %)
                              :on-touch-move  #(handle-touchmove %)
                              :on-drag-start #(handle-dragstart % cursor)
                              :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                              :on-mouse-enter #(when (or (not (or flipped facedown))
                                                         (= (:side @game-state) (keyword (.toLowerCase side))))
                                                 (put! zoom-channel cursor))
                              :on-mouse-leave #(put! zoom-channel false)
                              :on-click #(handle-card-click @cursor owner)}
        (when-let [url (image-url cursor)]
          (if (or flipped facedown)
            [:img.card.bg {:src (str "/img/" (.toLowerCase side) ".png")}]
            [:div
             [:span.cardname title]
             [:img.card.bg {:src url :onError #(-> % .-target js/$ .hide)}]]))
        [:div.counters
         (when (pos? counter) (let [counter-type (or (card-counter-type @cursor) "")
                                    norm-type (lower-case counter-type)
                                    selector (str "div.darkbg." norm-type "-counter.counter")]
                                [(keyword selector) counter]))
         (when (pos? rec-counter) [:div.darkbg.recurring-counter.counter rec-counter])
         (when (pos? advance-counter) [:div.darkbg.advance-counter.counter advance-counter])]
        (when (and current-strength (not= strength current-strength))
              current-strength [:div.darkbg.strength current-strength])
        (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
        (when named-target [:div.darkbg.named-target named-target])
        (when (and (= zone ["hand"]) (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
          (let [centrals ["HQ" "R&D" "Archives"]
                remotes (conj (remote-list remotes) "New remote")
                servers (case type
                          ("Upgrade" "ICE") (concat remotes centrals)
                          ("Agenda" "Asset") remotes)]
            [:div.blue-shade.panel.servers-menu {:ref "servers"}
             (map (fn [label]
                    [:div {:on-click #(do (send-command "play" {:card @cursor :server label})
                                          (-> (om/get-node owner "servers") js/$ .fadeOut))}
                     label])
                  servers)]))
        (let [actions (action-list cursor)]
          (when (or (> (+ (count actions) (count abilities)) 1)
                    (= (first actions) "derez"))
            [:div.blue-shade.panel.abilities {:ref "abilities"}
             (map (fn [action]
                    [:div {:on-click #(do (send-command action {:card @cursor}))} (capitalize action)])
                  actions)
             (map-indexed
              (fn [i ab]
                (if (:auto-pump ab)
                  [:div {:on-click #(do (send-command "auto-pump" {:card @cursor}))
                         :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]
                  [:div {:on-click #(do (send-command "ability" {:card @cursor
                                                                 :ability (if (some (fn [a] (:auto-pump a)) abilities)
                                                                            (dec i) i)})
                                        (-> (om/get-node owner "abilities") js/$ .fadeOut))
                         :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]))
              abilities)]))
        (when (= (first zone) "servers")
          (cond
            (and (= type "Agenda") (>= advance-counter (or current-cost advancementcost)))
            [:div.blue-shade.panel.menu.abilities {:ref "agenda"}
             [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
             [:div {:on-click #(send-command "score" {:card @cursor})} "Score"]]
            (or (= advanceable "always") (and rezzed (= advanceable "rezzed-only")))
            [:div.blue-shade.panel.menu.abilities {:ref "advance"}
             [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
             [:div {:on-click #(send-command "rez" {:card @cursor})} "Rez"]]))]
       (when (pos? (count hosted))
         [:div.hosted
          (om/build-all card-view hosted {:key :cid})])]))))

(defn drop-area [side server hmap]
  (merge hmap {:on-drop #(handle-drop % server)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-server server}))

(defn label [cursor owner opts]
  (om/component
   (sab/html
    (let [fn (or (:fn opts) count)]
      [:div.header {:class (when (> (count cursor) 0) "darkbg")}
       (str (:name opts) " (" (fn cursor) ")")]))))

(defn hand-view [{:keys [player remotes] :as cursor}]
  (om/component
   (sab/html
    (let [side (get-in player [:identity :side])
          size (count (:hand player))
          name (if (= side "Corp") "HQ" "Grip")]
      [:div.panel.blue-shade.hand
       (drop-area (:side @game-state) name {:class (when (> size 6) "squeeze")})
       (om/build label (:hand player) {:opts {:name name}})
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:class (if (and (not= "select" (get-in player [:prompt 0 :prompt-type]))
                                                          (not (:selected card)) (playable? card))
                                                   "playable" "")
                                          :style {:left (* (/ 320 (dec size)) i)}}
                       (if (= (:user player) (:user @app-state))
                         (om/build card-view (assoc card :remotes remotes))
                         [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}])])
                    (:hand player))]))))

(defn show-deck [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "system-msg" {:msg "looks at their deck"}))

(defn close-popup [event owner ref msg shuffle?]
  (-> (om/get-node owner ref) js/$ .fadeOut)
  (when shuffle?
    (send-command "shuffle" {:close "true"}))
  (when msg
    (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defmulti deck-view #(get-in % [:identity :side]))

(defmethod deck-view "Runner" [{:keys [deck] :as cursor} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck
     (drop-area (:side @game-state) "Stack"
                {:on-click #(-> (om/get-node owner "stack-menu") js/$ .toggle)})
     (om/build label deck {:opts {:name "Stack"}})
     (when (= (:side @game-state) :runner)
       [:div.panel.blue-shade.menu {:ref "stack-menu"}
        [:div {:on-click #(do (send-command "shuffle")
                              (-> (om/get-node owner "stack-menu") js/$ .fadeOut))} "Shuffle"]
        [:div {:on-click #(show-deck % owner "stack")} "Show"]])
     (when (= (:side @game-state) :runner)
       [:div.panel.blue-shade.popup {:ref "stack-content"}
        [:div
         [:a {:on-click #(close-popup % owner "stack-content" "stops looking at their deck" false)}
          "Close"]
         [:a {:on-click #(close-popup % owner "stack-content" "stops looking at their deck" true)}
          "Close & Shuffle"]]
        (om/build-all card-view deck {:key :cid})])
     (when (pos? (count deck))
       [:img.card.bg {:src "/img/runner.png"}])])))

(defmethod deck-view "Corp" [{:keys [deck servers] :as cursor} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade.deck
     (drop-area (:side @game-state) "R&D"
                {:on-click #(-> (om/get-node owner "rd-menu") js/$ .toggle)
                 :class (when (> (count (get-in servers [:rd :content])) 0) "shift")})
      (om/build label deck {:opts {:name "R&D"}})
     (when (= (:side @game-state) :corp)
       [:div.panel.blue-shade.menu {:ref "rd-menu"}
        [:div {:on-click #(do (send-command "shuffle")
                              (-> (om/get-node owner "rd-menu") js/$ .fadeOut))} "Shuffle"]
        [:div {:on-click #(show-deck % owner "rd")} "Show"]])
     (when (= (:side @game-state) :corp)
       [:div.panel.blue-shade.popup {:ref "rd-content"}
        [:div
         [:a {:on-click #(close-popup % owner "rd-content" "stops looking at their deck" false)} "Close"]
         [:a {:on-click #(close-popup % owner "rd-content" "stops looking at their deck" true)} "Close & Shuffle"]]
        (om/build-all card-view deck {:key :cid})])
     (when (pos? (count deck))
       [:img.card.bg {:src "/img/corp.png"}])])))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Runner" [{:keys [discard] :as cursor} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     (drop-area :runner "Heap" {:on-click #(-> (om/get-node owner "popup") js/$ .fadeIn)})
     (om/build label discard {:opts {:name "Heap"}})
     [:div.panel.blue-shade.popup {:ref "popup" :class (when-not (= (:side @game-state) :runner) "opponent")}
      [:div
       [:a {:on-click #(close-popup % owner "popup" nil false)} "Close"]]
      (om/build-all card-view discard {:key :cid})]
     (when-not (empty? discard)
       (om/build card-view (last discard)))])))

(defmethod discard-view "Corp" [{:keys [discard servers] :as cursor} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade.discard
     (drop-area :corp "Archives" {:class (when (> (count (get-in servers [:discard :content])) 0) "shift")
                                  :on-click #(-> (om/get-node owner "popup") js/$ .fadeIn)})
     (om/build label discard {:opts {:name "Archives"}})

     [:div.panel.blue-shade.popup {:ref "popup" :class (when (= (:side @game-state) :runner) "opponent")}
      [:div
       [:a {:on-click #(close-popup % owner "popup" nil false)} "Close"]]
      (for [c discard]
        (if (or (:seen c) (:rezzed c))
          (om/build card-view c)
          (if (not= (:side @game-state) :corp)
            [:img.card {:src "/img/corp.png"}]
            [:div.unseen (om/build card-view c)])))]

     (when-not (empty? discard)
       (let [c (last discard)]
         (if (= (:side @game-state) :corp)
           (om/build card-view c)
           (if (or (:seen c) (:rezzed c))
             (om/build card-view c)
             [:img.card {:src "/img/corp.png"}]))))])))

(defn rfg-view [{:keys [cards name] :as cursor}]
  (om/component
   (sab/html
    (when-not (empty? cards)
      (let [size (count cards)]
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (om/build label cards {:opts {:name name}})
         (map-indexed (fn [i card]
                        [:div.card-wrapper {:style {:left (* (/ 128 size) i)}}
                         [:div (om/build card-view card)]])
                      cards)])))))

(defn play-area-view [{:keys [name player] :as cursor}]
  (om/component
   (sab/html
    (let [cards (:play-area player)
          size (count cards)
          side (get-in player [:identity :side])]
      (when-not (empty? cards)
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (om/build label cards {:opts {:name name}})
         (map-indexed (fn [i card]
                        [:div.card-wrapper {:style {:left (* (/ 128 size) i)}}
                         (if (= (:user player) (:user @app-state))
                           (om/build card-view card)
                           [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}])])
                      cards)])))))

(defn scored-view [{:keys [scored] :as cursor}]
  (om/component
   (sab/html
    (let [size (count scored)]
      [:div.panel.blue-shade.scored.squeeze
       (om/build label scored {:opts {:name "Scored Area"}})
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:style {:left (* (/ 128 (dec size)) i)}}
                       [:div (om/build card-view card)]])
                    scored)]))))

(defn controls [key]
  (sab/html
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta 1}) :type "button"} "+"]
    [:button.small {:on-click #(send-command "change" {:key key :delta -1}) :type "button"} "-"]]))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Runner" [{:keys [user click credit run-credit memory link tag
                                        brain-damage agenda-point tagged hand-size-base
                                        hand-size-modification]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :runner)]
      [:div.stats.panel.blue-shade {}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (not= click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (not= credit 1) "s" "")
                  (when (pos? run-credit)
                    (str " (" run-credit " for run)")))
        (when me? (controls :credit))]
       [:div (str memory " Memory Unit" (if (not= memory 1) "s" "")) (when (neg? memory) [:div.warning "!"]) (when me? (controls :memory))]
       [:div (str link " Link Strength") (when me? (controls :link))]
       [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str tag " Tag" (if (not= tag 1) "s" "")) (when (or (pos? tag) (pos? tagged)) [:div.warning "!"]) (when me? (controls :tag))]
       [:div (str brain-damage " Brain Damage")
        (when me? (controls :brain-damage))]
       [:div (str (+ hand-size-base hand-size-modification) " Max hand size")
        (when me? (controls :hand-size-modification))]]))))

(defmethod stats-view "Corp" [{:keys [user click credit agenda-point bad-publicity
                                      hand-size-base hand-size-modification]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :corp)]
      [:div.stats.panel.blue-shade {}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (not= click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (not= credit 1) "s" "")) (when me? (controls :credit))]
       [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str bad-publicity " Bad Publicity")
        (when me? (controls :bad-publicity))]
       [:div (str (+ hand-size-base hand-size-modification) " Max hand size")
        (when me? (controls :hand-size-modification))]]))))

(defn server-view [{:keys [server central run] :as cursor} owner opts]
  (om/component
   (sab/html
    (let [content (:content server)]
      [:div.server
       (let [ices (:ices server)]
         [:div.ices
          (when run
            [:div.run-arrow {:style {:top (str (+ 20 (* 64 (:position run))) "px")}}])
          (for [ice ices]
            (om/build card-view ice {:opts {:flipped (not (:rezzed ice))}}))
          (when-let [run-card (:card (:run-effect run))]
            [:div.run-card (om/build card-img run-card)])])
       (when content
         [:div.content {:class (str (when (= (count content) 1) "center") " " (when central "shift"))}
          (for [card content]
            (om/build card-view card {:opts {:flipped (not (:rezzed card))}}))
          (om/build label content {:opts opts})])]))))

(defmulti board-view #(get-in % [:player :identity :side]))

(defmethod board-view "Corp" [{:keys [player run]}]
  (om/component
   (sab/html
    (let [servers (:servers player)
          s (:server run)
          server-type (first s)]
      [:div.corp-board {:class (when (= (:side @game-state) :runner) "opponent")}
       (om/build server-view {:server (:archives servers) :central true
                              :run (when (= server-type "archives") run)})
       (om/build server-view {:server (:rd servers) :central true
                              :run (when (= server-type "rd") run)})
       (om/build server-view {:server (:hq servers) :central true
                              :run (when (= server-type "hq") run)})
       (for [server (get-remotes servers)]
         (let [num (remote->num (first server))]
           (om/build server-view {:server (second server)
                                  :run (when (= server-type (str "remote" num)) run)}
                                 {:opts {:name (remote->name (first server))}})))]))))

(defmethod board-view "Runner" [{:keys [player run]}]
  (om/component
   (sab/html
    [:div.runner-board {:class (when (= (:side @game-state) :corp) "opponent")}
     (for [zone [:program :hardware :resource :facedown]]
       [:div (for [c (zone (:rig player))]
               [:div.card-wrapper {:class (when (playable? c) "playable")}
                (om/build card-view c)])])])))

(defn zones [{:keys [player remotes]} cursor]
  (om/component
   (sab/html
    [:div.dashboard
     (om/build hand-view {:player player :remotes remotes})
     (om/build discard-view player)
     (om/build deck-view player)
     [:div.panel.blue-shade.identity
      {:class (when (> (count (get-in player [:servers :hq :content])) 0) "shift")}
      (om/build card-view (:identity player))]])))

(defn cond-button [text cond f]
  (sab/html
   (if cond
     [:button {:on-click f} text]
     [:button.disabled text])))

(defn handle-end-turn [cursor owner]
  (let [me ((:side @game-state) @game-state)
        max-size (max (+ (:hand-size-base me) (:hand-size-modification me)) 0)]
    (if (> (count (:hand me)) max-size)
      (toast (str "Discard to " max-size " cards") "warning" nil)
      (send-command "end-turn"))))

(defn gameboard [{:keys [side gameid active-player run end-turn runner-phase-12 corp-phase-12] :as cursor} owner]
  (reify
    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card)))))

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (when (get-in cursor [side :prompt 0 :show-discard])
        (-> ".me .discard .popup" js/$ .fadeIn))
      (if (= "select" (get-in cursor [side :prompt 0 :prompt-type]))
        (set! (.-cursor (.-style (.-body js/document))) "url('/img/gold_crosshair.png') 12 12, crosshair")
        (set! (.-cursor (.-style (.-body js/document))) "default"))
      (doseq [{:keys [msg type options]} (get-in cursor [side :toast])]
        (toast msg type options)))

    om/IRenderState
    (render-state [this state]
      (sab/html
       (when side
         (let [me ((if (= side :runner) :runner :corp) cursor)
               opponent ((if (= side :runner) :corp :runner) cursor)]
           [:div.gameboard
            [:div.mainpane
             (om/build zones {:player opponent :remotes (get-remotes (get-in cursor [:corp :servers]))})
             [:div.centralpane
              [:div.leftpane
               [:div
                (om/build stats-view opponent)
                (om/build scored-view opponent)]
               [:div
                (om/build scored-view me)
                (om/build stats-view me)]]

              [:div.secondary-pane
               [:div
                (om/build rfg-view {:cards (:rfg opponent) :name "Removed from the game"})
                (om/build rfg-view {:cards (:rfg me) :name "Removed from the game"})
                (om/build play-area-view {:player opponent :name "Temporary Zone"})
                (om/build play-area-view {:player me :name "Temporary Zone"})
                (om/build rfg-view {:cards (:current opponent) :name "Current"})
                (om/build rfg-view {:cards (:current me) :name "Current"})]
               (when-not (= side :spectator)
                 [:div.button-pane { :on-mouse-over card-preview-mouse-over
                                     :on-mouse-out  card-preview-mouse-out  }
                  (if-let [prompt (first (:prompt me))]
                    [:div.panel.blue-shade
                     [:h4 (for [item (get-message-parts (:msg prompt))] (create-span item))]
                     (if-let [n (get-in prompt [:choices :number])]
                       [:div
                        [:div.credit-select
                         [:select#credit (for [i (range (inc n))]
                                           [:option {:value i} i])]]
                        [:button {:on-click #(send-command "choice"
                                                           {:choice (-> "#credit" js/$ .val js/parseInt)})}
                         "OK"]]
                       (case (:choices prompt)
                         "credit" [:div
                                   [:div.credit-select
                                    [:select#credit (for [i (range (inc (:credit me)))]
                                                      [:option {:value i} i])] " credits"]
                                   [:button {:on-click #(send-command "choice"
                                                                      {:choice (-> "#credit" js/$ .val js/parseInt)})}
                                    "OK"]]
                         "counter" [:div
                                    [:div.credit-select
                                     [:select#credit (for [i (range (inc (get-in prompt [:card :counter])))]
                                                       [:option {:value i} i])] " credits"]
                                    [:button {:on-click #(send-command "choice"
                                                                       {:choice (-> "#credit" js/$ .val js/parseInt)})}
                                     "OK"]]
                         (for [c (:choices prompt)]
                           (if (string? c)
                             [:button {:on-click #(send-command "choice" {:choice c})}
                              (for [item (get-message-parts c)] (create-span item))]
                             (let [[title code] (extract-card-info (add-image-codes (:title c)))]
                               [:button {:on-click #(send-command "choice" {:card @c}) :id code} title])))))]
                    (if run
                      (let [s (:server run)
                            kw (keyword (first s))
                            server (if-let [n (second s)]
                                     (get-in cursor [:corp :servers kw n])
                                     (get-in cursor [:corp :servers kw]))]
                        (if (= side :runner)
                          [:div.panel.blue-shade
                           (when-not (:no-action run) [:h4 "Waiting for Corp's actions" ])
                           (if (zero? (:position run))
                             (cond-button "Successful Run" (:no-action run) #(send-command "access"))
                             (cond-button "Continue" (:no-action run) #(send-command "continue")))
                           (cond-button "Jack Out" (not (get-in cursor [:run :cannot-jack-out]))
                                        #(send-command "jack-out"))]
                          [:div.panel.blue-shade
                           (when (zero? (:position run))
                             (cond-button "Action before access" (not (:no-action run))
                                          #(send-command "corp-phase-43")))
                           (cond-button "No more action" (not (:no-action run))
                                        #(send-command "no-action"))]))
                      [:div.panel.blue-shade
                       (if (= (keyword active-player) side)
                         (when (and (zero? (:click me)) (not end-turn) (not runner-phase-12) (not corp-phase-12))
                               [:button {:on-click #(handle-end-turn cursor owner)} "End Turn"])
                         (when end-turn
                           [:button {:on-click #(send-command "start-turn")} "Start Turn"]))
                       (when (and (= (keyword active-player) side)
                                  (or runner-phase-12 corp-phase-12))
                           [:button {:on-click #(send-command "end-phase-12")}
                            (if (= side :corp) "Mandatory Draw" "Take Clicks")])
                       (when (= side :runner)
                         [:div
                          (cond-button "Remove Tag"
                                       (and (pos? (:click me))
                                            (>= (:credit me) (- 2 (or (:tag-remove-bonus me) 0)))
                                            (pos? (:tag me)))
                                       #(send-command "remove-tag"))
                          [:div.run-button
                           (cond-button "Run" (and (pos? (:click me))
                                                   (not (get-in me [:register :cannot-run])))
                                        #(-> (om/get-node owner "servers") js/$ .toggle))
                           (let [remotes (get-remotes (get-in cursor [:corp :servers]))
                                 servers (concat (remote-list remotes) ["HQ" "R&D" "Archives"])]
                             [:div.blue-shade.panel.servers-menu {:ref "servers"}
                              (map (fn [label]
                                     [:div {:on-click #(do (send-command "run" {:server label})
                                                           (-> (om/get-node owner "servers") js/$ .fadeOut))}
                                      label])
                                   servers)])]])
                       (when (= side :corp)
                         (cond-button "Purge" (>= (:click me) 3) #(send-command "purge")))
                       (when (= side :corp)
                         (cond-button "Trash Resource" (and (pos? (:click me))
                                                            (>= (:credit me) (- 2 (or (:trash-cost-bonus me) 0)))
                                                            (or (pos? (:tagged opponent))
                                                                (pos? (:tag opponent))))
                                      #(send-command "trash-resource")))
                       (cond-button "Draw" (pos? (:click me)) #(send-command "draw"))
                       (cond-button "Gain Credit" (pos? (:click me)) #(send-command "credit"))]))])]

              [:div.board
               (om/build board-view {:player opponent :run run})
               (om/build board-view {:player me :run run})]]
             [:div.me
              (om/build zones {:player me :remotes (get-remotes (get-in cursor [:corp :servers]))})]]
            [:div.rightpane {}
             [:div.card-zoom
              (when-let [card (om/get-state owner :zoom)]
                (om/build cb/card-view card))]
             (om/build log-pane (:log cursor))]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
