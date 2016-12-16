(ns netrunner.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize includes? join lower-case split]]
            [netrunner.appstate :refer [app-state]]
            [netrunner.auth :refer [avatar] :as auth]
            [netrunner.cardbrowser :refer [add-symbols] :as cb]
            [differ.core :as differ]
            [om.dom :as dom]))

(defonce game-state (atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defn image-url
  [{:keys [side code] :as card} player]
  (let [version (if (and (not= (:side @game-state) (keyword (lower-case side)))
                         (not (get-in @app-state [:options :opponent-alt-art])))
                  "default"
                  (get-in @game-state [player :user :options :alt-arts (keyword (:code card))] "default"))]
    (str "/img/cards/" (:code card) (when-not (= version "default") (str "-" version)) ".png")))

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
          "hideMethod" "fadeOut"
          "tapToDismiss" (:tap-to-dismiss options true)))

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

(defn build-exception-msg [msg error]
  (letfn [(build-report-url [error]
            (js/escape (str "Please describe the circumstances of your error here.\n\n\nStack Trace:\n```clojure\n"
                            error
                            "\n```")))]
    (str "<div>"
         msg
         "<br/>"
         "<button type=\"button\" class=\"reportbtn\" style=\"margin-top: 5px\" "
         "onclick=\"window.open('https://github.com/mtgred/netrunner/issues/new?body="
         (build-report-url error)
         "');\">Report on GitHub</button></div>")))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr (if (= "exception" type) "error" type))]
    (f (if (= "exception" type) (build-exception-msg msg (:last-error @game-state)) msg))
    (send-command "toast")))

(defn play-sfx
  "Plays a list of sounds one after another."
  [sfx soundbank]
  (when-not (empty? sfx)
    (when-let [sfx-key (keyword (first sfx))]
      (.play (sfx-key soundbank)))
    (play-sfx (rest sfx) soundbank)))

(defn action-list [{:keys [type zone rezzed advanceable advance-counter advancementcost current-cost] :as card}]
  (-> []
      (#(if (or (and (= type "Agenda")
                     (#{"servers" "onhost"} (first zone)))
                (= advanceable "always")
                (and rezzed
                     (= advanceable "while-rezzed"))
                (and (not rezzed)
                     (= advanceable "while-unrezzed")))
          (cons "advance" %) %))
      (#(if (and (= type "Agenda") (>= advance-counter current-cost))
          (cons "score" %) %))
      (#(if (#{"Asset" "ICE" "Upgrade"} type)
          (if-not rezzed (cons "rez" %) (cons "derez" %))
          %))))

(defn handle-abilities [{:keys [abilities facedown side type] :as card} owner]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))]
    (when-not (and (= side "Runner") facedown)
      (cond
        ;; Open panel
        (or (> c 1)
            (some #{"derez" "advance"} actions)
            (and (= type "ICE")
                 (not (:run @game-state))))  ; Horrible hack to check if currently in a run
        (-> (om/get-node owner "abilities") js/$ .toggle)
        ;; Trigger first (and only) ability / action
        (= c 1)
        (if (= (count abilities) 1)
          (send-command "ability" {:card card :ability 0})
          (send-command (first actions) {:card card}))))))

(defn handle-card-click [{:keys [type zone root] :as card} owner]
  (let [side (:side @game-state)]
    (when (not-spectator? game-state app-state)
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt 0 :prompt-type]) "select")
        (send-command "select" {:card card})
        ;; Card is an identity of player's side
        (and (= (:type card) "Identity")
             (= side (keyword (.toLowerCase (:side card)))))
        (handle-abilities card owner)
        ;; Runner side
        (= side :runner)
        (case (first zone)
          "hand" (if (:host card)
                   (when (:installed card)
                     (handle-abilities card owner))
                   (send-command "play" {:card card}))
          ("rig" "current" "onhost" "play-area") (handle-abilities card owner)
          ("servers") (when (and (= type "ICE") (:rezzed card))
                        ;; ICE that should show list of abilities that send messages to fire sub
                        (-> (om/get-node owner "runner-abilities") js/$ .toggle))
          nil)
        ;; Corp side
        (= side :corp)
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
          nil)))))

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

(def ci-open "\u2664")
(def ci-seperator "\u2665")
(def ci-close "\u2666")

(defn is-card-item [item]
  (and (> (.indexOf item ci-seperator) -1)
       (= 0 (.indexOf item ci-open))))

(defn extract-card-info [item]
  (if (is-card-item item)
    [(.substring item 1 (.indexOf item ci-seperator))
     (.substring item (inc (.indexOf item ci-seperator)) (dec (count item)))]))

(defn create-span-impl [item]
  (if (= "[hr]" item)
    [:hr]
    (if (= "[!]" item)
      [:div.smallwarning "!"]
      (if-let [class (anr-icons item)]
        [:span {:class (str "anr-icon " class)}]
        (if-let [[title code] (extract-card-info item)]
          [:span {:class "fake-link" :id code} title]
          [:span item])))))

(defn get-non-alt-art [[title cards]]
  (let [s (sort-by #(= (:setname %) "Alternates") cards)]
    {:title title :code (:code (first s))}))

(defn prepare-cards []
  (->> (:cards @app-state)
       (group-by :title)
       (map get-non-alt-art)
       (sort-by #(count (:title %1)))
       (reverse)))

(def prepared-cards (memoize prepare-cards))

(def create-span (memoize create-span-impl))

(defn find-card-regex-impl [title]
  (str "(^|[^" ci-open "\\S])" title "(?![" ci-seperator "\\w]|([^" ci-open "]+" ci-close "))"))

(def find-card-regex (memoize find-card-regex-impl))

(defn card-image-token-impl [title code]
  (str "$1" ci-open title ci-seperator code ci-close))

(def card-image-token (memoize card-image-token-impl))

(defn card-image-reducer [text card]
  (.replace text (js/RegExp. (find-card-regex (:title card)) "g") (card-image-token (:title card) (:code card))))

(defn add-image-codes-impl [text]
  (reduce card-image-reducer text (prepared-cards)))

(def add-image-codes (memoize add-image-codes-impl))

(defn get-message-parts-impl [text]
  (let [with-image-codes (add-image-codes (if (nil? text) "" text))
        splitted (.split with-image-codes (js/RegExp. (str "(" ci-open "[^" ci-close "]*" ci-close ")") "g"))
        oldstyle (for [i splitted]
                   (seq (.split i (js/RegExp. (str "([1-3]\\[mu\\]|\\[[^\\]]*\\])") "g"))))]
    (flatten oldstyle)))

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
          (aset div "scrollTop" height)))) om/IDidMount
    (did-mount [this]
      (-> ".log" js/$ (.resizable #js {:handles "w"})))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.log {:on-mouse-over card-preview-mouse-over
                  :on-mouse-out  card-preview-mouse-out}
        [:div.panel.blue-shade.messages {:ref "msg-list"}
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
                        (clojure.string/join "" (repeat (second c) (str "[" (capitalize (name (first c))) "]"))))))) ": ")))

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last js/parseInt))

(defn remote->name [server]
  (let [num (remote->num server)]
    (str "Server " num)))

(defn central->name [zone]
  "Converts a central zone keyword to a string."
  (case (if (keyword? zone) zone (last zone))
    :hq "HQ"
    :rd "R&D"
    :archives "Archives"
    nil))

(defn zone->name [zone]
  "Converts a zone to a string."
  (or (central->name zone)
      (remote->name zone)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (js/parseInt
     (last (clojure.string/split (str zone) #":remote")))))

(defn zones->sorted-names [zones]
  (->> zones (sort-by zone->sort-key) (map zone->name)))

(defn get-remotes [servers]
  (->> servers
       (filter #(not (#{:hq :rd :archives} (first %))))
       (sort-by #(zone->sort-key (first %)))))

(defn remote-list [remotes]
  (->> remotes (map first) zones->sorted-names))

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
       [:div.blue-shade.card {:on-mouse-enter #(put! zoom-channel cursor)
                              :on-mouse-leave #(put! zoom-channel false)}
        (when-let [url (image-url cursor (keyword (lower-case (:side cursor))))]
          [:div
           [:span.cardname title]
           [:img.card.bg {:src url :onError #(-> % .-target js/$ .hide)}]])]]))))

(defn face-down?
  "Returns true if the installed card should be drawn face down."
  [{:keys [side type facedown rezzed host] :as card}]
  (if (= side "Corp")
    (and (not= type "Operation")
         (not rezzed)
         (not= (:side host) "Runner"))
    facedown))

(defn card-view [{:keys [zone code type abilities counter advance-counter advancementcost current-cost subtype
                         advanceable rezzed strength current-strength title remotes selected hosted
                         side rec-counter facedown server-target subtype-target icon new runner-abilities subroutines]
                  :as cursor}
                 owner {:keys [flipped] :as opts}]
  (om/component
   (sab/html
    [:div.card-frame
     [:div.blue-shade.card {:class (str (when selected "selected") (when new " new"))
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
      (when-let [url (image-url cursor (keyword (lower-case (:side cursor))))]
        (if (or (not code) flipped facedown)
          [:img.card.bg {:src (str "/img/" (.toLowerCase side) ".png")}]
          [:div
           [:span.cardname title]
           [:img.card.bg {:src url :onError #(-> % .-target js/$ .hide)}]]))
      [:div.counters
       (when counter
         (map (fn [[type num-counters]]
                (when (pos? num-counters)
                  (let [selector (str "div.darkbg." (lower-case (name type)) "-counter.counter")]
                    [(keyword selector) num-counters])))
              counter))
       (when (pos? rec-counter) [:div.darkbg.recurring-counter.counter rec-counter])
       (when (pos? advance-counter) [:div.darkbg.advance-counter.counter advance-counter])]
      (when (and current-strength (not= strength current-strength))
        current-strength [:div.darkbg.strength current-strength])
      (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
      (when server-target [:div.darkbg.server-target server-target])
      (when subtype-target
        (let [colour-type (case subtype-target
                            ("Barrier" "Sentry") (lower-case subtype-target)
                            "Code Gate" "code-gate"
                            nil)
              label (if (includes? subtype-target " - ")
                      (->> (split subtype-target #" - ")
                           (map first)
                           (join " - "))
                      subtype-target)]
          [:div.darkbg.subtype-target {:class colour-type} label]))
      (when (and (= zone ["hand"]) (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
        (let [centrals ["Archives" "R&D" "HQ"]
              remotes (concat (remote-list remotes) ["New remote"])
              servers (case type
                        ("Upgrade" "ICE") (concat centrals remotes)
                        ("Agenda" "Asset") remotes)]
          [:div.panel.blue-shade.servers-menu {:ref "servers"}
           (map (fn [label]
                  [:div {:on-click #(do (send-command "play" {:card @cursor :server label})
                                        (-> (om/get-node owner "servers") js/$ .fadeOut))}
                   label])
                servers)]))
      (when (pos? (+ (count runner-abilities) (count subroutines)))
        [:div.panel.blue-shade.runner-abilities {:ref "runner-abilities"}
         (map-indexed
          (fn [i ab]
            [:div {:on-click #(do (send-command "runner-ability" {:card @cursor
                                                                  :ability i}))
                   :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}])
          runner-abilities)
         (when (> (count subroutines) 1)
           [:div {:on-click #(send-command "system-msg"
                                           {:msg (str "indicates to fire all subroutines on " title)})}
            "Let all subroutines fire"])
         (map (fn [sub]
                [:div {:on-click #(send-command "system-msg"
                                                {:msg (str "indicates to fire the \"" (:label sub)
                                                           "\" subroutine on " title)})
                       :dangerouslySetInnerHTML #js {:__html (add-symbols (str "Let fire: \"" (:label sub) "\""))}}])
              subroutines)])
      (let [actions (action-list cursor)
            dynabi-count (count (filter #(= (first %) :dynamic) abilities))]
        (when (or (> (+ (count actions) (count abilities) (count subroutines)) 1)
                  (some #{"derez" "advance"} actions)
                  (= type "ICE"))
          [:div.panel.blue-shade.abilities {:ref "abilities"}
           (map (fn [action]
                  [:div {:on-click #(do (send-command action {:card @cursor}))} (capitalize action)])
                actions)
           (map-indexed
            (fn [i ab]
              (if (:dynamic ab)
                [:div {:on-click #(do (send-command "dynamic-ability" (assoc (select-keys ab [:dynamic :source :index])
                                                                             :card @cursor)))
                       :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]
                [:div {:on-click #(do (send-command "ability" {:card @cursor
                                                               :ability (- i dynabi-count)})
                                      (-> (om/get-node owner "abilities") js/$ .fadeOut))
                       :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]))
            abilities)
           (map-indexed
            (fn [i sub]
              [:div {:on-click #(do (send-command "subroutine" {:card @cursor :subroutine i})
                                    (-> (om/get-node owner "abilities") js/$ .fadeOut))
                     :dangerouslySetInnerHTML #js {:__html (add-symbols (str "[Subroutine]" (:label sub)))}}])
            subroutines)]))
      (when (#{"servers" "onhost"} (first zone))
        (cond
          (and (= type "Agenda") (>= advance-counter (or current-cost advancementcost)))
          [:div.panel.blue-shade.menu.abilities {:ref "agenda"}
           [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
           [:div {:on-click #(send-command "score" {:card @cursor})} "Score"]]
          (or (= advanceable "always") (and rezzed (= advanceable "rezzed-only")))
          [:div.panel.blue-shade.menu.abilities {:ref "advance"}
           [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
           [:div {:on-click #(send-command "rez" {:card @cursor})} "Rez"]]))]
     (when (pos? (count hosted))
       [:div.hosted
        (for [card hosted]
          (om/build card-view card {:opts {:flipped (face-down? card)}}))])])))

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
       [:div
        (map-indexed (fn [i card]
                       [:div.card-wrapper {:class (if (and (not= "select" (get-in player [:prompt 0 :prompt-type]))
                                                           (= (:user player) (:user @app-state))
                                                           (not (:selected card)) (playable? card))
                                                    "playable" "")
                                           :style {:left (* (/ 320 (dec size)) i)}}
                        (if (or (= (:user player) (:user @app-state))
                                (:openhand player)
                                (and (get-in @game-state [:options :spectatorhands])
                                     (not (not-spectator? game-state app-state))))
                          (om/build card-view (assoc card :remotes remotes))
                          [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}])])
                     (:hand player))]
       (om/build label (:hand player) {:opts {:name name}})]))))

(defn show-deck [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "view-deck"))

(defn close-popup [event owner ref msg shuffle? deck?]
  (-> (om/get-node owner ref) js/$ .fadeOut)
  (cond
    shuffle? (send-command "shuffle" {:close "true"})
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn identity-view [player owner]
  (om/component
   (sab/html
    [:div.blue-shade.identity
     (om/build card-view (:identity player))])))

(defn deck-view [{:keys [identity deck] :as cursor} owner]
  (om/component
   (sab/html
    (let [is-runner (= "Runner" (:side identity))
          side (if is-runner :runner :corp)
          name (if is-runner "Stack" "R&D")
          ref (if is-runner "stack" "rd")
          menu-ref (str ref "-menu")
          content-ref (str ref "-content")]
      [:div.blue-shade.deck
       (drop-area (:side @game-state) name
                  {:on-click #(-> (om/get-node owner menu-ref) js/$ .toggle)})
       (if (pos? (count deck))
         [:img.card.bg {:src (if is-runner "/img/runner.png" "/img/corp.png")}])
       (om/build label deck {:opts {:name name}})
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.menu {:ref menu-ref}
          [:div {:on-click #(do (send-command "shuffle")
                                (-> (om/get-node owner menu-ref) js/$ .fadeOut))} "Shuffle"]
          [:div {:on-click #(show-deck % owner ref)} "Show"]])
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.popup {:ref content-ref}
          [:div
           [:a {:on-click #(close-popup % owner content-ref "stops looking at their deck" false true)}
            "Close"]
           [:a {:on-click #(close-popup % owner content-ref "stops looking at their deck" true true)}
            "Close & Shuffle"]]
          (om/build-all card-view deck {:key :cid})])]))))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Runner" [{:keys [discard] :as cursor} owner]
  (om/component
   (sab/html
    [:div.blue-shade.discard
     (drop-area :runner "Heap" {:on-click #(-> (om/get-node owner "popup") js/$ .fadeIn)})
     (when-not (empty? discard)
       (om/build card-view (last discard)))
     (om/build label discard {:opts {:name "Heap"}})
     [:div.panel.blue-shade.popup {:ref "popup" :class (if (= (:side @game-state) :runner) "me" "opponent")}
      [:div
       [:a {:on-click #(close-popup % owner "popup" nil false false)} "Close"]]
      (om/build-all card-view discard {:key :cid})]])))

(defmethod discard-view "Corp" [{:keys [discard servers] :as cursor} owner]
  (om/component
   (sab/html
    (let [faceup? #(or (:seen %) (:rezzed %))]
      [:div.blue-shade.discard
       (drop-area :corp "Archives" {:on-click #(-> (om/get-node owner "popup") js/$ .fadeIn)})

       (when-not (empty? discard)
         (let [c (last discard)]
           (if (= (:side @game-state) :corp)
             (om/build card-view c)
             (if (faceup? c)
               (om/build card-view c)
               [:img.card {:src "/img/corp.png"}]))))

       (om/build label discard {:opts {:name "Archives"
                                       :fn (fn [cursor] (let [total (count cursor)
                                                              face-up (count (filter faceup? cursor))]
                                                         ;; use non-breaking space to keep counts on same line.
                                                          (str face-up "\u2191\u00A0" (- total face-up) "\u2193")))}})

       [:div.panel.blue-shade.popup {:ref "popup" :class (if (= (:side @game-state) :runner) "opponent" "me")}
        [:div
         [:a {:on-click #(close-popup % owner "popup" nil false false)} "Close"]
         [:label (let [total (count discard)
                       face-up (count (filter faceup? discard))]
                   (str total " cards, " (- total face-up) " face-down."))]]
        (for [c discard]
          (if (faceup? c)
            (om/build card-view c)
            (if (not= (:side @game-state) :corp)
              [:img.card {:src "/img/corp.png"}]
              [:div.unseen (om/build card-view c)])))]]))))

(defn rfg-view [{:keys [cards name] :as cursor}]
  (om/component
   (sab/html
    (when-not (empty? cards)
      (let [size (count cards)]
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (map-indexed (fn [i card]
                        [:div.card-wrapper {:style {:left (* (/ 128 size) i)}}
                         [:div (om/build card-view card)]])
                      cards)
         (om/build label cards {:opts {:name name}})])))))

(defn play-area-view [{:keys [name player] :as cursor}]
  (om/component
   (sab/html
    (let [cards (:play-area player)
          size (count cards)
          side (get-in player [:identity :side])]
      (when-not (empty? cards)
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (map-indexed (fn [i card]
                        [:div.card-wrapper {:style {:left (* (/ 128 size) i)}}
                         (if (= (:user player) (:user @app-state))
                           (om/build card-view card)
                           [:img.card {:src (str "/img/" (.toLowerCase side) ".png")}])])
                      cards)
         (om/build label cards {:opts {:name name}})])))))

(defn scored-view [{:keys [scored] :as cursor}]
  (om/component
   (sab/html
    (let [size (count scored)]
      [:div.panel.blue-shade.scored.squeeze
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:style {:left (* (/ 128 (dec size)) i)}}
                       [:div (om/build card-view card)]])
                    scored)
       (om/build label scored {:opts {:name "Scored Area"}})]))))

(defn controls [key]
  (sab/html
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta 1}) :type "button"} "+"]
    [:button.small {:on-click #(send-command "change" {:key key :delta -1}) :type "button"} "-"]]))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Runner" [{:keys [user click credit run-credit memory link tag
                                        brain-damage agenda-point tagged hand-size-base
                                        hand-size-modification active]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :runner)]
      [:div.panel.blue-shade.stats {:class (when active "active-player")}
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

(defmethod stats-view "Corp" [{:keys [user click credit agenda-point bad-publicity has-bad-pub
                                      hand-size-base hand-size-modification active]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :corp)]
      [:div.panel.blue-shade.stats {:class (when active "active-player")}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (not= click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (not= credit 1) "s" "")) (when me? (controls :credit))]
       [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str (+ bad-publicity has-bad-pub) " Bad Publicity")
        (when me? (controls :bad-publicity))]
       [:div (str (+ hand-size-base hand-size-modification) " Max hand size")
        (when me? (controls :hand-size-modification))]]))))

(defn server-view [{:keys [server central-view run] :as cursor} owner opts]
  (om/component
   (sab/html
    (let [content (:content server)]
      [:div.server
       (let [ices (:ices server)
             run-pos (:position run)
             current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                           (nth ices (dec run-pos)))
             run-arrow (sab/html [:div.run-arrow [:div]])
             max-hosted (apply max (map #(count (:hosted %)) ices))]
         [:div.ices {:style {:width (when (pos? max-hosted)
                                      (+ 84 3 (* 42 (dec max-hosted))))}}
          (when-let [run-card (:card (:run-effect run))]
            [:div.run-card (om/build card-img run-card)])
          (for [ice (reverse ices)]
            [:div.ice {:class (when (not-empty (:hosted ice)) "host")}
             (om/build card-view ice {:opts {:flipped (not (:rezzed ice))}})
             (when (and current-ice (= (:cid current-ice) (:cid ice)))
               run-arrow)])
          (when (and run (not current-ice))
            run-arrow)])
       [:div.content
        (when central-view
          central-view)
        (when (not-empty content)
          (for [card content
                :let [is-first (= card (first content))]]
            [:div.server-card {:class (str (when central-view "central ")
                                           (when (or central-view
                                                     (and (< 1 (count content))
                                                          (not is-first)))
                                             "shift"))}
             (om/build card-view card {:opts {:flipped (not (:rezzed card))}})
             (when (and (not central-view) is-first)
               (om/build label content {:opts opts}))]))]]))))

(defmulti board-view #(get-in % [:player :identity :side]))

(defmethod board-view "Corp" [{:keys [player run]}]
  (om/component
   (sab/html
    (let [servers (:servers player)
          s (:server run)
          server-type (first s)]
      [:div.corp-board {:class (if (= (:side @game-state) :runner) "opponent" "me")}
       (for [server (reverse (get-remotes servers))
             :let [num (remote->num (first server))]]
         (om/build server-view {:server (second server)
                                :run (when (= server-type (str "remote" num)) run)}
                   {:opts {:name (remote->name (first server))}}))
       (om/build server-view {:server (:hq servers)
                              :central-view (om/build identity-view player)
                              :run (when (= server-type "hq") run)})
       (om/build server-view {:server (:rd servers)
                              :central-view (om/build deck-view player)
                              :run (when (= server-type "rd") run)})
       (om/build server-view {:server (:archives servers)
                              :central-view (om/build discard-view player)
                              :run (when (= server-type "archives") run)})]))))

(defmethod board-view "Runner" [{:keys [player run]}]
  (om/component
   (sab/html
    (let [is-me (= (:side @game-state) :runner)
          centrals (sab/html
                    [:div.runner-centrals
                     (om/build discard-view player)
                     (om/build deck-view player)
                     (om/build identity-view player)])]
      [:div.runner-board {:class (if is-me "me" "opponent")}
       (when-not is-me centrals)
       (for [zone [:program :hardware :resource :facedown]]
         [:div
          (for [c (zone (:rig player))]
            [:div.card-wrapper {:class (when (playable? c) "playable")}
             (om/build card-view c)])])
       (when is-me centrals)]))))

(defn cond-button [text cond f]
  (sab/html
   (if cond
     [:button {:on-click f} text]
     [:button.disabled text])))

(defn handle-end-turn []
  (let [me ((:side @game-state) @game-state)
        max-size (max (+ (:hand-size-base me) (:hand-size-modification me)) 0)]
    (if (> (count (:hand me)) max-size)
      (toast (str "Discard to " max-size " card" (when (not= 1 max-size) "s")) "warning" nil)
      (send-command "end-turn"))))

(defn runnable-servers
  "List of servers the runner can run on."
  [corp runner]
  (let [servers (keys (:servers corp))
        restricted-servers (keys (get-in runner [:register :cannot-run-on-server]))]
    ;; remove restricted servers from all servers to just return allowed servers
    (remove (set restricted-servers) servers)))

(defn button-pane [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 corp runner me opponent] :as cursor} owner]
  (om/component
   (sab/html
    [:div.button-pane {:on-mouse-over card-preview-mouse-over
                       :on-mouse-out  card-preview-mouse-out}
     (if-let [prompt (first (:prompt me))]
       [:div.panel.blue-shade
        [:h4 (for [item (get-message-parts (:msg prompt))] (create-span item))]
        (if-let [n (get-in prompt [:choices :number])]
          [:div
           [:div.credit-select
            [:select#credit {:default-value (get-in prompt [:choices :default] 0)}
             (for [i (range (inc n))]
               [:option {:value i} i])]]
           [:button {:on-click #(send-command "choice"
                                              {:choice (-> "#credit" js/$ .val js/parseInt)})}
            "OK"]]
          (cond
               ;; choice of number of credits
            (= (:choices prompt) "credit")
            [:div
             [:div.credit-select
              [:select#credit (for [i (range (inc (:credit me)))]
                                [:option {:value i} i])] " credits"]
             [:button {:on-click #(send-command "choice"
                                                {:choice (-> "#credit" js/$ .val js/parseInt)})}
              "OK"]]
               ;; choice of specified counters on card
            (:card-title (:choices prompt))
            [:div
             [:div.credit-select
              [:input#card-title {:placeholder "Enter a card title"
                                  :onKeyUp #(when (= 13 (.-keyCode %))
                                              (-> "#card-submit" js/$ .click)
                                              (.stopPropagation %))}]]
             [:button#card-submit {:on-click #(send-command "choice"
                                                            {:choice (-> "#card-title" js/$ .val)})}
              "OK"]
             (when-let [autocomp (:autocomplete (:choices prompt))]
               (-> "#card-title" js/$ (.autocomplete (clj->js {"source" autocomp})))
               nil)]
            (:counter (:choices prompt))
            (let [counter-type (keyword (:counter (:choices prompt)))
                  num-counters (get-in prompt [:card :counter counter-type] 0)]
              [:div
               [:div.credit-select
                [:select#credit (for [i (range (inc num-counters))]
                                  [:option {:value i} i])] " credits"]
               [:button {:on-click #(send-command "choice"
                                                  {:choice (-> "#credit" js/$ .val js/parseInt)})}
                "OK"]])
               ;; otherwise choice of all present choices
            :else
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
                        (get-in corp [:servers kw n])
                        (get-in corp [:servers kw]))]
           (if (= side :runner)
             [:div.panel.blue-shade
              (when-not (:no-action run) [:h4 "Waiting for Corp's actions"])
              (if (zero? (:position run))
                (cond-button "Successful Run" (:no-action run) #(send-command "access"))
                (cond-button "Continue" (:no-action run) #(send-command "continue")))
              (cond-button "Jack Out" (not (:cannot-jack-out run))
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
              [:button {:on-click #(handle-end-turn)} "End Turn"])
            (when end-turn
              [:button {:on-click #(send-command "start-turn")} "Start Turn"]))
          (when (and (= (keyword active-player) side)
                     (or runner-phase-12 corp-phase-12))
            [:button {:on-click #(send-command "end-phase-12")}
             (if (= side :corp) "Mandatory Draw" "Take Clicks")])
          (when (= side :runner)
            (cond-button "Remove Tag"
                         (and (pos? (:click me))
                              (>= (:credit me) (- 2 (or (:tag-remove-bonus me) 0)))
                              (pos? (:tag me)))
                         #(send-command "remove-tag"))
            [:div.run-button
             (cond-button "Run" (and (pos? (:click me))
                                     (not (get-in me [:register :cannot-run])))
                          #(-> (om/get-node owner "servers") js/$ .toggle))
             [:div.panel.blue-shade.servers-menu {:ref "servers"}
              (map (fn [label]
                     [:div {:on-click #(do (send-command "run" {:server label})
                                           (-> (om/get-node owner "servers") js/$ .fadeOut))}
                      label])
                   (zones->sorted-names (runnable-servers corp runner)))]])
          (when (= side :corp)
            (cond-button "Purge" (>= (:click me) 3) #(send-command "purge")))
          (when (= side :corp)
            (cond-button "Trash Resource" (and (pos? (:click me))
                                               (>= (:credit me) (- 2 (or (:trash-cost-bonus me) 0)))
                                               (or (pos? (:tagged opponent))
                                                   (pos? (:tag opponent))))
                         #(send-command "trash-resource")))
          (cond-button "Draw" (pos? (:click me)) #(send-command "draw"))
          (cond-button "Gain Credit" (pos? (:click me)) #(send-command "credit"))]))])))

(defn update-audio [{:keys [gameid sfx sfx-current-id] :as cursor} owner]
  ;; When it's the first game played with this state or when the sound history comes from different game, we skip the cacophony
  (let [sfx-last-played (om/get-state owner :sfx-last-played)]
    (when (and (get-in @app-state [:options :sounds])
               (not (nil? sfx-last-played))
               (= gameid (:gameid sfx-last-played)))
      ;; Skip the SFX from queue with id smaller than the one last played, queue the rest
      (let [sfx-to-play (reduce (fn [sfx-list {:keys [id name]}]
                                  (if (> id (:id sfx-last-played))
                                    (conj sfx-list name)
                                    sfx-list)) [] sfx)]
        (play-sfx sfx-to-play (om/get-state owner :soundbank)))))
  ;; Remember the most recent sfx id as last played so we don't repeat it later
  (om/set-state! owner :sfx-last-played {:gameid gameid :id sfx-current-id}))

(defn gameboard [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 turn corp runner] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      (let [audio-sfx (fn [name] (list (keyword name)
                                       (new js/Howl (clj->js {:urls [(str "/sound/" name ".ogg")
                                                                     (str "/sound/" name ".mp3")]}))))]
        {:soundbank
         (apply hash-map (concat
                          (audio-sfx "agenda-score")
                          (audio-sfx "agenda-steal")
                          (audio-sfx "click-advance")
                          (audio-sfx "click-card")
                          (audio-sfx "click-credit")
                          (audio-sfx "click-run")
                          (audio-sfx "click-remove-tag")
                          (audio-sfx "game-end")
                          (audio-sfx "install-corp")
                          (audio-sfx "install-runner")
                          (audio-sfx "play-instant")
                          (audio-sfx "rez-ice")
                          (audio-sfx "rez-other")
                          (audio-sfx "run-successful")
                          (audio-sfx "run-unsuccessful")
                          (audio-sfx "virus-purge")))}))

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
      (when (= "card-title" (get-in cursor [side :prompt 0 :prompt-type]))
        (-> "#card-title" js/$ .focus))
      (doseq [{:keys [msg type options]} (get-in cursor [side :toast])]
        (toast msg type options))
      (update-audio cursor owner))

    om/IRenderState
    (render-state [this state]
      (sab/html
       (when side
         (let [me       (assoc ((if (= side :runner) :runner :corp) cursor) :active (and (pos? turn) (= (keyword active-player) side)))
               opponent (assoc ((if (= side :runner) :corp :runner) cursor) :active (and (pos? turn) (not= (keyword active-player) side)))]
           [:div.gameboard
            [:div.gameboard-bg {:class (:background (:options @app-state))}]

            [:div.rightpane
             [:div.card-zoom
              (when-let [card (om/get-state owner :zoom)]
                (om/build cb/card-view card))]
             ;; card implementation info
             (when-let [card (om/get-state owner :zoom)]
               (let [implemented (:implementation card)]
                 (case implemented
                   (:full "full") nil
                   [:div.panel.blue-shade.implementation
                    (case implemented
                      nil [:span.unimplemented "Unimplemented"]
                      [:span.impl-msg implemented])])))
             (om/build log-pane (:log cursor))]

            [:div.centralpane
             (om/build board-view {:player opponent :run run})
             (om/build board-view {:player me :run run})]

            [:div.leftpane
             [:div.opponent
              (om/build hand-view {:player opponent :remotes (get-remotes (:servers corp))})]

             [:div.inner-leftpane
              [:div.left-inner-leftpane
               [:div
                (om/build stats-view opponent)
                (om/build scored-view opponent)]
               [:div
                (om/build scored-view me)
                (om/build stats-view me)]]

              [:div.right-inner-leftpane
               [:div
                (om/build rfg-view {:cards (:rfg opponent) :name "Removed from the game"})
                (om/build rfg-view {:cards (:rfg me) :name "Removed from the game"})
                (om/build play-area-view {:player opponent :name "Temporary Zone"})
                (om/build play-area-view {:player me :name "Temporary Zone"})
                (om/build rfg-view {:cards (:current opponent) :name "Current"})
                (om/build rfg-view {:cards (:current me) :name "Current"})]
               (when-not (= side :spectator)
                 (om/build button-pane {:side side :active-player active-player :run run :end-turn end-turn :runner-phase-12 runner-phase-12 :corp-phase-12 corp-phase-12 :corp corp :runner runner :me me :opponent opponent}))]]

             [:div.me
              (om/build hand-view {:player me :remotes (get-remotes (:servers corp))})]]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
