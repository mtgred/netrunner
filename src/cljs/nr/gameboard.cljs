(ns nr.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :as s :refer [capitalize includes? join lower-case split]]
            [differ.core :as differ]
            [game.core.card :refer [active? has-subtype? asset? rezzed? ice? corp?
                                    faceup? installed? same-card?]]
            [jinteki.utils :refer [str->int is-tagged?] :as utils]
            [jinteki.cards :refer [all-cards]]
            [nr.appstate :refer [app-state]]
            [nr.auth :as auth]
            [nr.avatar :refer [avatar]]
            [nr.end-of-game-stats :refer [build-game-stats]]
            [nr.utils :refer [banned-span influence-dot influence-dots map-longest
                              toastr-options render-icons render-message
                              checkbox-button cond-button]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defonce game-state (r/atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defonce board-dom (atom {}))
(defonce sfx-state (atom {}))

(defn image-url [{:keys [side code] :as card}]
  (let [art (or (:art card) ; use the art set on the card itself, or fall back to the user's preferences.
                (get-in @game-state [(keyword (lower-case side)) :user :options :alt-arts (keyword code)]))
        art-options (:alt_art (get (:alt-arts @app-state) code))
        special-user (get-in @game-state [(keyword (lower-case side)) :user :special])
        special-wants-art (get-in @game-state [(keyword (lower-case side)) :user :options :show-alt-art])
        viewer-wants-art (get-in @app-state [:options :show-alt-art])
        show-art (and special-user special-wants-art viewer-wants-art)
        art-available (and art-options (not-empty art-options))
        has-art (and art-options
                     art
                     (contains? art-options (keyword art)))
        version-path (if (and has-art show-art)
                       (get art-options (keyword art) (:code card))
                       (:code card))]
    (str "/img/cards/" version-path ".png")))

(defn get-side [state]
  (let [user-id (:_id (:user @app-state))]
    (cond
      (= (get-in state [:runner :user :_id]) user-id) :runner
      (= (get-in state [:corp :user :_id]) user-id) :corp
      :else :spectator)))

(defn not-spectator? []
  (not= :spectator (get-side @game-state)))

(defn init-game [state]
  (let [side (get-side state)]
    (.setItem js/localStorage "gameid" (:gameid @app-state))
    (reset! game-state state)
    (swap! game-state assoc :side side)
    (reset! last-state @game-state)
    (reset! lock false)))

(defn launch-game [{:keys [state]}]
  (init-game state)
  (set! (.-onbeforeunload js/window) #(clj->js "Leaving this page will disconnect you from the game."))
  (-> "#gamelobby" js/$ .fadeOut)
  (-> "#gameboard" js/$ .fadeIn))

(declare toast)

(defn notify
  "Send a notification to the chat, and a toast to the current player of the specified severity"
  [text severity]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text}))
  (toast text severity nil))

(def zoom-channel (chan))

(def button-channel (chan))

(defn check-lock?
  "Check if we can clear client lock based on action-id"
  []
  (let [aid [(:side @game-state) :aid]]
    (when (not= (get-in @game-state aid)
                (get-in @last-state aid))
      (reset! lock false))))

(defn handle-state [{:keys [state]}] (init-game state))

(defn handle-diff [{:keys [gameid diff]}]
  (when (= gameid (:gameid @game-state))
    (swap! game-state #(differ/patch @last-state diff))
    (check-lock?)
    (reset! last-state @game-state)))

(defn handle-timeout [{:keys [gameid]}]
  (when (= gameid (:gameid @game-state))
    (toast "Game closed due to inactivity" "error" {:time-out 0 :close-button true})))

(defn parse-state [state]
  (js->clj (.parse js/JSON state) :keywordize-keys true))

(ws/register-ws-handler! :netrunner/state #(handle-state (parse-state %)))
(ws/register-ws-handler! :netrunner/start #(launch-game (parse-state %)))
(ws/register-ws-handler! :netrunner/diff #(handle-diff (parse-state %)))
(ws/register-ws-handler! :netrunner/timeout #(handle-timeout (parse-state %)))

(defn send-command
  ([command] (send-command command nil))
  ([command {:keys [no-lock] :as args}]
   (when (or (not @lock) no-lock)
     (try (js/ga "send" "event" "game" command) (catch js/Error e))
     (when-not no-lock (reset! lock true))
     (ws/ws-send! [:netrunner/action {:gameid-str (:gameid @game-state)
                                      :command command
                                      :args args}]))))

(defn mute-spectators [mute-state]
  (ws/ws-send! [:netrunner/mute-spectators {:gameid-str (:gameid @game-state)
                                            :mute-state mute-state}]))

(defn stack-servers []
  (swap! app-state update-in [:options :stacked-servers] not))

(defn flip-runner-board []
  (let [layout (if (= "irl" (get-in @app-state [:options :runner-board-order])) "jnet" "irl")]
    (swap! app-state assoc-in [:options :runner-board-order] layout)))

(defn concede []
  (ws/ws-send! [:netrunner/concede {:gameid-str (:gameid @game-state)}]))

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

(defn action-list
  [{:keys [type zone rezzed advanceable advance-counter advancementcost current-cost] :as card}]
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
      (#(if (#{"ICE" "Program"} type)
          (cons "trash" %) %))
      (#(if (#{"Asset" "ICE" "Upgrade"} type)
          (if-not rezzed (cons "rez" %) (cons "derez" %))
          %))))

(defn handle-abilities
  [side {:keys [abilities corp-abilities runner-abilities facedown type] :as card} c-state]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))
        card-side (keyword (.toLowerCase (:side card)))]
    (when-not (and (= card-side :runner) facedown)
      (cond

        ;; Toggle abilities panel
        (or (< 1 c)
            (pos? (+ (count corp-abilities)
                     (count runner-abilities)))
            (some #{"rez" "derez" "advance" "trash"} actions)
            (and (= type "ICE")
                 (not (:run @game-state)))
            (and (corp? card)
                 (not (faceup? card))))
        (do (when (= side card-side)
              (if (:abilities @c-state)
                (swap! c-state dissoc :abilities)
                (swap! c-state assoc :abilities true)))
            (when (and (= :runner card-side)
                       (= :corp side)
                       (:corp-abilities card))
              (if (:corp-abilities @c-state)
                (swap! c-state dissoc :corp-abilities)
                (swap! c-state assoc :corp-abilities true)))
            (when (and (= :corp card-side)
                       (= :runner side)
                       (:runner-abilities card))
              (if (:runner-abilities @c-state)
                (swap! c-state dissoc :runner-abilities)
                (swap! c-state assoc :runner-abilities true))))

        ;; Trigger first (and only) ability / action
        (and (= c 1)
             (= side card-side))
        (if (= (count abilities) 1)
          (send-command "ability" {:card card :ability 0})
          (send-command (first actions) {:card card}))))))

(defn handle-card-click [{:keys [type zone root] :as card} c-state]
  (let [side (:side @game-state)]
    (when (not-spectator?)
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt 0 :prompt-type]) "select")
        (send-command "select" {:card card})

        ;; Card is an identity of player's side
        (and (= (:type card) "Identity")
             (= side (keyword (.toLowerCase (:side card)))))
        (handle-abilities side card c-state)

        ;; Runner side
        (= side :runner)
        (case (first zone)
          "hand" (if (:host card)
                   (when (:installed card)
                     (handle-abilities side card c-state))
                   (send-command "play" {:card card}))
          ("current" "onhost" "play-area" "scored" "servers" "rig")
          (handle-abilities side card c-state)
          nil)

        ;; Corp side
        (= side :corp)
        (case (first zone)
          "hand" (case type
                   ("Agenda" "Asset" "ICE" "Upgrade")
                   (if (:servers @c-state)
                     (do (swap! c-state dissoc :servers)
                         (send-command "generate-install-list" nil))
                     (do (swap! c-state assoc :servers true)
                         (send-command "generate-install-list" {:card card})))
                   (send-command "play" {:card card}))
          ("current" "onhost" "play-area" "scored" "servers" "rig")
          (handle-abilities side card c-state)
          nil)))))

(defn in-play? [card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @game-state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn playable? [{:keys [title side zone cost type uniqueness abilities] :as card}]
  (let [my-side (:side @game-state)
        me (my-side @game-state)]
    (and (= (keyword (.toLowerCase side)) my-side)

         (cond

           (has-subtype? card "Double")
           (if (>= (:click me) 2) true false)

           (has-subtype? card "Triple")
           (if (>= (:click me) 3) true false)

           (= (:code card) "07036") ; Day Job
           (if (>= (:click me) 4) true false)

           (has-subtype? card "Priority")
           (if (get-in @game-state [my-side :register :spent-click]) false true)

           :else
           true)

         (and (= zone ["hand"])
              (or (not uniqueness) (not (in-play? card)))
              (or (#{"Agenda" "Asset" "Upgrade" "ICE"} type) (>= (:credit me) cost))
              (pos? (:click me))))))

(defn spectator-view-hidden?
  "Checks if spectators are allowed to see hidden information, such as hands and face-down cards"
  []
  (and (get-in @game-state [:options :spectatorhands])
       (not (not-spectator?))))

(defn get-card-code [e]
  (let [code (str (.. e -target -id))]
    (when (pos? (count code))
      code)))

(defn card-preview-mouse-over [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (when-let [card (some #(when (= (:code %) code) %) @all-cards)]
      (put! channel (assoc card :implementation :full))))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (put! channel false))
  nil)

(defn card-highlight-mouse-over [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel value))
  nil)

(defn card-highlight-mouse-out [e value channel]
  (.preventDefault e)
  (when (:cid value)
    (put! channel false))
  nil)

(defn scrolled-to-end?
  [el tolerance]
  (> tolerance (- (.-scrollHeight el) (.-scrollTop el) (.-clientHeight el))))

(def should-scroll (r/atom {:update true :send-msg false}))

(defn resize-card-zoom []
  "Resizes the card zoom based on the values in the app-state"
  (let [width (get-in @app-state [:options :log-width])
        top (get-in @app-state [:options :log-top])
        max-card-width (- width 5)
        max-card-height (- top 10)
        card-ratio (/ 418 300)]
    (if (> (/ max-card-height max-card-width) card-ratio)
      (-> ".card-zoom" js/$
          (.css "width" max-card-width)
          (.css "height" (int (* max-card-width card-ratio))))
      (-> ".card-zoom" js/$
          (.css "width" (int (/ max-card-height card-ratio)))
          (.css "height" max-card-height)))
    (-> ".rightpane" js/$ (.css "width" width))
    (-> ".log" js/$
        (.css "left" 0)
        (.css "top" top)
        (.css "width" width))))

(defn log-resize [event ui]
  "Resize the card zoom to fit the available space"
  (let [width (.. ui -size -width)
        top (.. ui -position -top)]
    (swap! app-state assoc-in [:options :log-width] width)
    (swap! app-state assoc-in [:options :log-top] top)
    (.setItem js/localStorage "log-width" width)
    (.setItem js/localStorage "log-top" top)
    (resize-card-zoom)))

(defn log-start-resize [event ui]
  "Display a zoomed card when resizing so the user can visualize how the
  resulting zoom will look."
  (when-let [card (get-in @game-state [:runner :identity])]
    (put! zoom-channel card)))

(defn log-stop-resize [event ui]
  (put! zoom-channel false))

(defn log-pane []
  (r/create-class
    (let [log (r/cursor game-state [:log])]
      {:display-name "log-pane"

       :component-did-mount
       (fn [this]
         (-> ".log" js/$ (.resizable #js {:handles "w, n, nw"
                                          :resize log-resize
                                          :start log-start-resize
                                          :stop log-stop-resize})))

       :component-will-update
       (fn [this]
         (let [n (r/dom-node this)]
           (reset! should-scroll {:update (or (:send-msg @should-scroll)
                                                  (scrolled-to-end? n 15))
                                  :send-msg false})))

       :component-did-update
       (fn [this]
         (when (:update @should-scroll)
           (let [n (r/dom-node this)]
             (set! (.-scrollTop n) (.-scrollHeight n)))))

       :reagent-render
       (fn []
         [:div.panel.blue-shade.messages {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                                          :on-mouse-out #(card-preview-mouse-out % zoom-channel)}
          (doall (map-indexed
                   (fn [i msg]
                     (when-not (and (= (:user msg) "__system__") (= (:text msg) "typing"))
                       (if (= (:user msg) "__system__")
                         [:div.system {:key i} (render-message (:text msg))]
                         [:div.message {:key i}
                          [avatar (:user msg) {:opts {:size 38}}]
                          [:div.content
                           [:div.username (get-in msg [:user :username])]
                           [:div (render-message (:text msg))]]])))
                   @log))])})))

(defn log-typing []
  (let [typing (r/cursor game-state [:typing])
        username (get-in @app-state [:user :username])]
    (when (seq (remove nil? (remove #{username} @typing)))
      [:div [:p.typing
             (doall
               (for [i (range 10)]
                 ^{:key i}
                 [:span " " influence-dot " "]))]])))

(defn send-msg [s]
  (let [text (:msg @s)]
    (when-not (empty? text)
      (reset! should-scroll {:update false :send-msg true})
      (ws/ws-send! [:netrunner/say {:gameid-str (:gameid @game-state)
                                    :msg text}])
      (swap! s assoc :msg ""))))

(defn send-typing [s]
  "Send a typing event to server for this user if it is not already set in game state"
  (let [text (:msg @s)
        username (get-in @app-state [:user :username])]
    (if (empty? text)
      (ws/ws-send! [:netrunner/typing {:gameid-str (:gameid @game-state)
                                       :typing false}])
      (when (not-any? #{username} (:typing @game-state))
        (ws/ws-send! [:netrunner/typing {:gameid-str (:gameid @game-state)
                                         :typing true}])))))

(defn indicate-action []
  (when (not-spectator?)
    [:button {:style {:width "98%"}
              :on-click #(do (.preventDefault %)
                             (send-command "indicate-action"))
              :key "Indicate action"}
     "Indicate action"]))

(defn log-input []
  (let [gameid (r/cursor game-state [:gameid])
        games (r/cursor app-state [:games])
        s (r/atom {})]
    (fn []
      (let [game (some #(when (= @gameid (str (:gameid %))) %) @games)]
        (when (or (not-spectator?)
                  (not (:mutespectators game)))
          [:div
           [:form {:on-submit #(do (.preventDefault %)
                                   (send-msg s))
                   :on-input #(do (.preventDefault %)
                                  (send-typing s))}
            [:input {:placeholder "Say something"
                     :type "text"
                     :value (:msg @s)
                     :on-change #(swap! s assoc :msg (-> % .-target .-value))}]]
           [indicate-action]])))))

(defn handle-dragstart [e card]
  (-> e .-target js/$ (.addClass "dragged"))
  (-> e .-dataTransfer (.setData "card" (.stringify js/JSON (clj->js card)))))

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

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last str->int))

(defn remote->name [server]
  (let [num (remote->num server)]
    (str "Server " num)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (str->int
      (last (clojure.string/split (str zone) #":remote")))))

(defn get-remotes [servers]
  (->> servers
       (filter #(not (#{:hq :rd :archives} (first %))))
       (sort-by #(zone->sort-key (first %)))))

(defn facedown-card
  "Image element of a facedown card"
  ([side] (facedown-card side [] nil))
  ([side class-list alt-alt-text]
   (let [s (lower-case side)
         alt (if (nil? alt-alt-text)
               (str "Facedown " s " card")
               alt-alt-text)
         tag (->> class-list
                  vec
                  (concat ["img" "card"])
                  (join ".")
                  keyword)]
     [tag {:src (str "/img/" s ".png")
           :alt alt}])))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [code title] :as card}]
  (when code
    [:div.card-frame
     [:div.blue-shade.card {:on-mouse-enter #(put! zoom-channel card)
                            :on-mouse-leave #(put! zoom-channel false)}
      (when-let [url (image-url card)]
        [:div
         [:span.cardname title]
         [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]))

(defn face-down?
  "Returns true if the installed card should be drawn face down."
  [{:keys [side type facedown rezzed host] :as card}]
  (if (= side "Corp")
    (and (not= type "Operation")
         (not rezzed)
         (not= (:side host) "Runner"))
    facedown))

(defn card-implementation [zoom-card]
  (when-let [card @zoom-card]
    (let [implemented (:implementation card)]
      (case implemented
        (:full "full") nil
        [:div.panel.blue-shade.implementation {:style {:right (get-in @app-state [:options :log-width])}}
         (case implemented
           nil [:span.unimplemented "Unimplemented"]
           [:span.impl-msg implemented])]))))

(defn card-as-text
  [card]
  [:div.card-preview.blue-shade
   [:h4 (:title card)]
   (when-let [memory (:memoryunits card)]
     (if (< memory 3)
       [:div.anr-icon {:class (str "mu" memory)} ""]
       [:div.heading (str "Memory: " memory) [:span.anr-icon.mu]]))
   (when-let [cost (:cost card)]
     [:div.heading (str "Cost: " cost)])
   (when-let [trash-cost (:trash card)]
     [:div.heading (str "Trash cost: " trash-cost)])
   (when-let [strength (:strength card)]
     [:div.heading (str "Strength: " strength)])
   (when-let [requirement (:advancementcost card)]
     [:div.heading (str "Advancement requirement: " requirement)])
   (when-let [agenda-point (:agendatpoints card)]
     [:div.heading (str "Agenda points: " agenda-point)])
   (when-let [min-deck-size (:minimumdecksize card)]
     [:div.heading (str "Minimum deck size: " min-deck-size)])
   (when-let [influence-limit (:influencelimit card)]
     [:div.heading (str "Influence limit: " influence-limit)])
   (when-let [influence (:factioncost card)]
     (when-let [faction (:faction card)]
       [:div.heading "Influence "
        [:span.influence
         {:class (-> faction lower-case (s/replace " " "-"))}
         (influence-dots influence)]]))
   [:div.heading
    [:span.type (str (:type card))]
    (when-let [subtypes (seq (:subtype card))]
      (str ": " subtypes))]
   [:div.text
    (render-icons (:text (first (filter #(= (:title %) (:title card)) @all-cards))))]
   (when-let [url (image-url card)]
     [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}])])

(defn card-zoom [zoom-card]
  (if-let [card @zoom-card]
    (do (-> ".card-zoom" js/$ (.addClass "fade"))
        [card-as-text card])
    (do (-> ".card-zoom" js/$ (.removeClass "fade")) nil)))

(defn server-menu
  "The pop-up on a card in hand when clicked"
  [card c-state]
  (let [servers (get-in @game-state [:corp :install-list])]
    (when servers
      [:div.panel.blue-shade.servers-menu {:style (when (:servers @c-state) {:display "inline"})}
       (map-indexed
         (fn [i label]
           [:div {:key i
                  :on-click #(do (send-command "play" {:card card :server label})
                                 (swap! c-state dissoc :servers))}
            label])
         servers)])))

(defn runner-abs [card c-state runner-abilities subroutines title]
  (when (:runner-abilities @c-state)
    [:div.panel.blue-shade.runner-abilities {:style {:display "inline"}}
     (when (or (seq runner-abilities)
               (seq subroutines))
       [:span.float-center "Abilities:"])
     (map-indexed
       (fn [i ab]
         [:div {:key i
                :on-click #(send-command "runner-ability" {:card card
                                                           :ability i})}
          (render-icons (:label ab))])
       runner-abilities)
     (when (seq subroutines)
       [:div {:on-click #(send-command "system-msg"
                                       {:msg (str "indicates to fire all unbroken subroutines on " title)})}
        "Let all subroutines fire"])
     (when (seq subroutines)
       [:span.float-center "Subroutines:"])
     (map-indexed
       (fn [i sub]
         [:span {:style {:display "block"}
                 :key i}
          [:span (cond (:broken sub)
                       {:class :disabled
                        :style {:font-style :italic}}
                       (false? (:resolve sub))
                       {:class :dont-resolve
                        :style {:text-decoration :line-through}})
           (render-icons (str " [Subroutine]" " " (:label sub)))]
          [:span.float-right
           (cond (:broken sub) banned-span
                 (:fired sub) "✅")]])
       subroutines)]))

(defn corp-abs [card c-state corp-abilities]
  (when (:corp-abilities @c-state)
    [:div.panel.blue-shade.corp-abilities {:style {:display "inline"}}
     (when (seq corp-abilities)
       [:span.float-center "Abilities:"])
     (map-indexed
       (fn [i ab]
         [:div {:on-click #(send-command "corp-ability" {:card card
                                                         :ability i})}
          (render-icons (:label ab))])
       corp-abilities)]))

(defn card-abilities [card c-state abilities subroutines]
  (let [actions (action-list card)
        dynabi-count (count (filter :dynamic abilities))]
    (when (and (:abilities @c-state)
               (or (pos? (+ (count actions)
                            (count abilities)
                            (count subroutines)))
                   (some #{"derez" "rez" "advance" "trash"} actions)
                   (= type "ICE")))
      [:div.panel.blue-shade.abilities {:style {:display "inline"}}
       (when (seq actions)
         [:span.float-center "Actions:"])
       (when (seq actions)
         (map-indexed
           (fn [i action]
             [:div {:key i
                    :on-click #(do (send-command action {:card card}))}
              (capitalize action)])
           actions))
       (when (seq abilities)
         [:span.float-center "Abilities:"])
       (when (seq abilities)
         (map-indexed
           (fn [i ab]
             (if (:dynamic ab)
               [:div {:key i
                      :on-click #(send-command "dynamic-ability" (assoc (select-keys ab [:dynamic :source :index])
                                                                        :card card))}
                (render-icons (:label ab))]
               [:div {:key i
                      :on-click #(send-command "ability" {:card card
                                                          :ability (- i dynabi-count)})}
                (render-icons (:label ab))]))
           abilities))
       (when (seq (remove :fired subroutines))
         [:div {:on-click #(send-command "unbroken-subroutines" {:card card})}
          "Fire unbroken subroutines"])
       (when (seq subroutines)
         [:span.float-center "Subroutines:"])
       (when (seq subroutines)
         (map-indexed
           (fn [i sub]
             [:div {:key i
                    :on-click #(send-command "subroutine" {:card card
                                                           :subroutine i})}
              [:span (cond (:broken sub)
                           {:class :disabled
                            :style {:font-style :italic}}
                           (false? (:resolve sub))
                           {:class :dont-resolve
                            :style {:text-decoration :line-through}})
               (render-icons (str " [Subroutine]" " " (:label sub)))]
              [:span.float-right
               (cond (:broken sub) banned-span
                     (:fired sub) "✅")]])
           subroutines))])))

(defn card-view
  [card filpped]
  (let [c-state (r/atom {})]
    (fn [{:keys [zone code type abilities counter advance-counter advancementcost current-cost subtype
                 advanceable rezzed strength current-strength title remotes selected hosted
                 side rec-counter facedown server-target subtype-target icon new runner-abilities subroutines
                 corp-abilities]
          :as card}
         flipped]
      [:div.card-frame
       [:div.blue-shade.card {:class (str (when selected "selected")
                                          (when new " new")
                                          (when (same-card? card (:button @app-state)) " hovered"))
                              :draggable (when (not-spectator?) true)
                              :on-touch-start #(handle-touchstart % card)
                              :on-touch-end   #(handle-touchend %)
                              :on-touch-move  #(handle-touchmove %)
                              :on-drag-start #(handle-dragstart % card)
                              :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                              :on-mouse-enter #(when (or (not (or (not code) flipped facedown))
                                                         (spectator-view-hidden?)
                                                         (= (:side @game-state) (keyword (lower-case side))))
                                                 (put! zoom-channel card))
                              :on-mouse-leave #(put! zoom-channel false)
                              :on-click #(handle-card-click card c-state)}
        (when-let [url (image-url card)]
          (if (or (not code) flipped facedown)
            (let [facedown-but-known (or (not (or (not code) flipped facedown))
                                         (spectator-view-hidden?)
                                         (= (:side @game-state) (keyword (lower-case side))))
                  alt-str (if facedown-but-known (str "Facedown " title) nil)]
              [facedown-card side ["bg"] alt-str])
            [:div
             [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]]))
        [:span.cardname title]
        [:div.counters
         (when counter
           (map-indexed (fn [i [type num-counters]]
                          (when (pos? num-counters)
                            (let [selector (str "div.darkbg." (lower-case (name type)) "-counter.counter")]
                              [(keyword selector) {:key type} num-counters])))
                        counter))
         (when (pos? rec-counter) [:div.darkbg.recurring-counter.counter {:key "rec"} rec-counter])
         (when (pos? advance-counter) [:div.darkbg.advance-counter.counter {:key "adv"} advance-counter])]
        (when (and (or current-strength strength)
                   (or (ice? card)
                       (has-subtype? card "Icebreaker"))
                   (active? card))
          [:div.darkbg.strength (or current-strength strength)])
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

      (when (and (= zone ["hand"])
                 (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
        [server-menu card c-state])

      (when (pos? (+ (count runner-abilities) (count subroutines)))
        [runner-abs card c-state runner-abilities subroutines title])

      (when (pos? (count corp-abilities))
        [corp-abs card c-state corp-abilities])

      [card-abilities card c-state abilities subroutines]

      (when (#{"servers" "onhost"} (first zone))
        (cond
          (and (= type "Agenda") (>= advance-counter (or current-cost advancementcost)))
          [:div.panel.blue-shade.menu.abilities
           [:div {:on-click #(send-command "advance" {:card card})} "Advance"]
           [:div {:on-click #(send-command "score" {:card card})} "Score"]]
          (or (= advanceable "always") (and rezzed (= advanceable "rezzed-only")))
          [:div.panel.blue-shade.menu.abilities
           [:div {:on-click #(send-command "advance" {:card card})} "Advance"]
           [:div {:on-click #(send-command "rez" {:card card})} "Rez"]]))]
     (when (pos? (count hosted))
       [:div.hosted
        (doall
          (for [card hosted]
            (let [flipped (face-down? card)]
              ^{:key (:cid card)}
              [card-view card flipped])))])])))

(defn drop-area [server hmap]
  (merge hmap {:on-drop #(handle-drop % server)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-server server}))

(defn close-popup [event ref msg shuffle? deck?]
  (-> ref js/$ .fadeOut)
  (cond
    shuffle? (send-command "shuffle" {:close "true"})
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn label [cursor opts]
  (let [fn (or (get-in opts [:opts :fn]) count)
        classes (str (when (pos? (count cursor)) "darkbg ")
                     (get-in opts [:opts :classes]))]
    [:div.header {:class classes}
     (str (get-in opts [:opts :name])
          (when (not (get-in opts [:opts :hide-cursor])) (str " (" (fn cursor) ")")))]))

(defn- this-user?
  [user]
  (= (:_id user) (-> @app-state :user :_id)))

(defn build-hand-card-view
  [user hand prompt remotes wrapper-class]
  (let [size (count @hand)]
    [:div
     (doall (map-indexed
              (fn [i card]
                [:div {:key (:cid card)
                       :class (str
                                (if (and (not= "select" (-> @prompt first :prompt-type))
                                         (this-user? @user)
                                         (not (:selected card)) (playable? card))
                                  "playable" "")
                                " "
                                wrapper-class)
                       :style {:left (when (< 1 size) (* (/ 320 (dec size)) i))}}
                 (if (or (this-user? @user)
                         (get-in @game-state [(utils/other-side (get-side @game-state)) :openhand]) ;; TODO: this rebuilds the hand UI on any state change
                         (spectator-view-hidden?))
                   [card-view (assoc card :remotes @remotes)]
                   [facedown-card (:side card)])])
              @hand))]))

(defn hand-view [user name hand prompt remotes popup popup-direction]
  (let [s (r/atom {})]
    (fn [user name hand prompt remotes popup popup-direction]
      (let [size (count @hand)]
        [:div.hand-container
         [:div.hand-controls
          [:div.panel.blue-shade.hand
           (drop-area name {:class (when (> size 6) "squeeze")})
           [build-hand-card-view user hand prompt remotes "card-wrapper"]
           [label @hand {:opts {:name name}}]]
          (when popup
            [:div.panel.blue-shade.hand-expand
             {:on-click #(-> (:hand-popup @s) js/$ .fadeToggle)}
             "+"])]
         (when popup
           [:div.panel.blue-shade.popup {:ref #(swap! s assoc :hand-popup %) :class popup-direction}
            [:div
             [:a {:on-click #(close-popup % (:hand-popup @s) nil false false)} "Close"]
             [:label (str size " card" (when (not= 1 size) "s") ".")]
             [build-hand-card-view user hand prompt remotes "card-popup-wrapper"]]])]))))

(defn show-deck [event ref]
  (-> ((keyword (str ref "-content")) @board-dom) js/$ .fadeIn)
  (-> ((keyword (str ref "-menu")) @board-dom) js/$ .fadeOut)
  (send-command "view-deck"))

(defn identity-view [identity]
  [:div.blue-shade.identity
   [card-view @identity]])

(defn deck-view [render-side player-side identity deck]
   (let [is-runner (= :runner render-side)
         name (if is-runner "Stack" "R&D")
         ref (if is-runner "stack" "rd")
         menu-ref (keyword (str ref "-menu"))
         content-ref (keyword (str ref "-content"))]
     (fn [render-side player-side identity deck]
       [:div.blue-shade.deck
        (drop-area name {:on-click #(-> (menu-ref @board-dom) js/$ .toggle)})
        (when (pos? (count @deck))
          [facedown-card (:side @identity) ["bg"] nil])
        [label @deck {:opts {:name name :classes "server-label"}}]
        (when (= render-side player-side)
          [:div.panel.blue-shade.menu {:ref #(swap! board-dom assoc menu-ref %)}
           [:div {:on-click #(do (send-command "shuffle")
                                 (-> (menu-ref @board-dom) js/$ .fadeOut))} "Shuffle"]
           [:div {:on-click #(show-deck % ref)} "Show"]])
        (when (= render-side player-side)
          [:div.panel.blue-shade.popup {:ref #(swap! board-dom assoc content-ref %)}
           [:div
            [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" false true)}
             "Close"]
            [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" true true)}
             "Close & Shuffle"]]
           (doall
             (for [card @deck]
               ^{:key (:cid card)}
               [card-view card]))])])))

(defn discard-view-runner [player-side discard]
  (let [s (r/atom {})]
    (fn [player-side discard]
      [:div.blue-shade.discard
       (drop-area "Heap" {:on-click #(-> (:popup @s) js/$ .fadeToggle)})
       (when-not (empty? @discard)
         [:<> [card-view (last @discard)]])
       [label @discard {:opts {:name "Heap" :classes "server-label"}}]
       [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                     :class (if (= player-side :runner) "me" "opponent")}
        [:div
         [:a {:on-click #(close-popup % (:popup @s) nil false false)} "Close"]]
        (doall
          (for [card @discard]
            ^{:key (:cid card)}
            [card-view card]))]])))

(defn discard-view-corp [player-side discard]
  (let [s (r/atom {})]
    (fn [player-side discard]
      (let [faceup? #(or (:seen %) (:rezzed %))
            draw-card #(if (faceup? %)
                         [card-view %]
                         (if (or (= player-side :corp)
                                 (spectator-view-hidden?))
                           [:div.unseen [card-view %]]
                           [facedown-card "corp"]))]
        [:div.blue-shade.discard
         (drop-area "Archives" {:on-click #(-> (:popup @s) js/$ .fadeToggle)})
         (when-not (empty? @discard) [:<> {:key "discard"} (draw-card (last @discard))])

         [label @discard {:opts {:name "Archives"
                                 :classes "server-label"
                                 :fn (fn [cursor] (let [total (count cursor)
                                                        face-up (count (filter faceup? cursor))]
                                                    ;; use non-breaking space to keep counts on same line.
                                                    (str face-up "↑ " (- total face-up) "↓")))}}]

         [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                       :class (if (= (:side @game-state) :runner) "opponent" "me")}
          [:div
           [:a {:on-click #(close-popup % (:popup @s) nil false false)} "Close"]
           [:label (let [total (count @discard)
                         face-up (count (filter faceup? @discard))]
                     (str total " cards, " (- total face-up) " face-down."))]]
          (doall (for [c @discard]
                   ^{:key (:cid c)}
                   [:div (draw-card c)]))]]))))

(defn rfg-view [cards name popup]
  (let [dom (atom {})]
    (fn [cards name popup]
      (when-not (empty? @cards)
        (let [size (count @cards)]
          [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")
                                      :on-click (when popup #(-> (:rfg-popup @dom) js/$ .fadeToggle))}
           (doall
             (map-indexed (fn [i card]
                            [:div.card-wrapper {:key i
                                                :style {:left (when (> size 1) (* (/ 128 size) i))}}
                             [:div [card-view card]]])
                          @cards))
           [label @cards {:opts {:name name}}]

           (when popup
             [:div.panel.blue-shade.popup {:ref #(swap! dom assoc :rfg-popup %)
                                           :class "opponent"}
              [:div
               [:a {:on-click #(close-popup % (:rfg-popup @dom) nil false false)} "Close"]
               [:label (str size " card" (when (not= 1 size) "s") ".")]]
              (doall
                (for [c @cards]
                  ^{:key (:cid c)}
                  [card-view c]))])])))))

(defn play-area-view [user name cards]
  (fn [user name cards]
    (let [size (count @cards)]
      (when (pos? size)
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (doall
           (map-indexed (fn [i card]
                          [:div.card-wrapper {:key i
                                              :style {:left (when (> size 1) (* (/ 128 size) i))}}
                           (if (or (:seen card)
                                   (this-user? @user))
                             [card-view card]
                             [facedown-card (:side card)])])
                        @cards))
         [label @cards {:opts {:name name}}]]))))

(defn scored-view [scored]
  (let [size (count @scored)]
    [:div.panel.blue-shade.scored.squeeze
     (doall
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:key i
                                          :style {:left (when (> size 1) (* (/ 128 (dec size)) i))}}
                       [:div [card-view card]]])
                    @scored))
     [label @scored {:opts {:name "Scored Area"}}]]))

(defn controls
  "Create the control buttons for the side displays."
  ([key] (controls key 1 -1))
  ([key increment decrement]
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
    [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]]))

(defmulti stats-view #(get-in @% [:identity :side]))

(defmethod stats-view "Runner" [runner]
  (let [me? (= (:side @game-state) :runner)]
    (fn [runner]
      (let [{:keys [user click credit run-credit memory link tag
                    brain-damage agenda-point hand-size active]} @runner]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         [:h4.ellipsis [avatar user {:opts {:size 22}}] (:username user)]
         [:div (str click " Click" (if (not= click 1) "s" ""))
          (when me? (controls :click))]
         [:div (str credit " Credit" (if (not= credit 1) "s" "")
                    (when (pos? run-credit)
                      (str " (" run-credit " for run)")))
          (when me? (controls :credit))]
         (let [{:keys [base mod used]} memory
               max-mu (+ base mod)
               unused (- max-mu used)]
           [:div (str unused " of " max-mu " MU unused")
            (when (neg? unused) [:div.warning "!"]) (when me? (controls :memory))])
         [:div (str link " Link Strength")
          (when me? (controls :link))]
         [:div (str agenda-point " Agenda Point"
                    (when (not= agenda-point 1) "s"))
          (when me? (controls :agenda-point))]
         (let [{:keys [base additional is-tagged]} tag
               tag-count (+ base additional)
               show-tagged (or (pos? tag-count) (pos? is-tagged))]
           [:div (str base (when (pos? additional) (str " + " additional)) " Tag" (if (not= tag-count 1) "s" ""))
            (when show-tagged [:div.warning "!"])
            (when me? (controls :tag))])
         [:div (str brain-damage " Brain Damage")
          (when me? (controls :brain-damage))]
         (let [{:keys [base mod]} hand-size]
           [:div (str (+ base mod) " Max hand size")
            (when me? (controls :hand-size))])]))))

(defmethod stats-view "Corp" [corp]
  (let [me? (= (:side @game-state) :corp)]
    (fn [corp]
      (let [{:keys [user click credit agenda-point bad-publicity hand-size active]} @corp]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         [:h4.ellipsis [avatar user {:opts {:size 22}}] (:username user)]
         [:div (str click " Click" (if (not= click 1) "s" ""))
          (when me? (controls :click))]
         [:div (str credit " Credit" (if (not= credit 1) "s" ""))
          (when me? (controls :credit))]
         [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
          (when me? (controls :agenda-point))]
         (let [{:keys [base additional]} bad-publicity]
           [:div (str (str base (when (pos? additional) (str " + " additional)) " Bad Publicity"))
            (when me? (controls :bad-publicity))])
         (let [{:keys [base mod]} hand-size]
           [:div (str (+ base mod) " Max hand size")
            (when me? (controls :hand-size))])]))))

(defn run-arrow [run]
  [:div.run-arrow [:div {:class (cond
                                  (= "approach-ice" (:phase run))
                                  "approach"
                                  (= "encounter-ice" (:phase run))
                                  "encounter"
                                  :else
                                  "")}]])

(enable-console-print!)

(defn server-view [{:keys [server central-view run]} opts]
  (let [content (:content server)
        ices (:ices server)
        run-pos (:position run)
        current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                      (nth ices (dec run-pos)))
        max-hosted (apply max (map #(count (:hosted %)) ices))]
    [:div.server
     [:div.ices {:style {:width (when (pos? max-hosted)
                                  (+ 84 3 (* 42 (dec max-hosted))))}}
      (when-let [run-cards (seq (filter :card (:run-effects run)))]
        [:div
         (doall (for [card (map :card (reverse run-cards))]
                  [:div.run-card {:key (:cid card)}
                   [card-img card]]))])
      (doall
        (for [ice (reverse ices)]
          [:div.ice {:key (:cid ice)
                     :class (when (not-empty (:hosted ice)) "host")}
           (let [flipped (not (:rezzed ice))]
             [card-view ice flipped])
           (when (and current-ice (= (:cid current-ice) (:cid ice)))
             [run-arrow run])]))
      (when (and run (not current-ice))
        [run-arrow run])]
     [:div.content
      (when central-view
        central-view)
      (when (not-empty content)
        (doall
          (for [card content]
            (let [is-first (= card (first content))
                  flipped (not (:rezzed card))]
              [:div.server-card {:key (:cid card)
                                 :class (str (when central-view "central ")
                                             (when (or central-view
                                                       (and (< 1 (count content)) (not is-first)))
                                               "shift"))}
               [card-view card flipped]]))))
      [label content (update-in opts [:opts] assoc :classes "server-label" :hide-cursor true)]]]))

(defn stacked-label [cursor similar-servers opts]
  (let [similar-server-names (->> similar-servers
                                  (map first)
                                  (map remote->name))
        full-server-names (cons (get-in opts [:opts :name]) similar-server-names)
        numbers (map #(second (split % " ")) full-server-names)]
    (label full-server-names (update-in opts [:opts] assoc
                                        :classes "server-label"
                                        :name (str "Servers " (join ", " numbers))
                                        :hide-cursor true))))

(defn stacked-view [{:keys [key server similar-servers central-view run]} opts]
  (let [content (apply conj
                       (:content server)
                       ; this unfolds all servers and picks the first item in it
                       ; since this creates a sequence, we need to apply it to conj
                       (map #(-> % second :content first) similar-servers))
        ices (:ices server)
        run-pos (:position run)
        current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                      (nth ices (dec run-pos)))]
    [:div.server
     [:div.ices
      (when-let [run-card (:card (:run-effect run))]
        [:div.run-card [card-img run-card]])
      (when (and run (not current-ice))
        [run-arrow run])]
     [:div.content
      (doall (for [card content]
               (let [is-first (= card (first content))
                     flipped (not (:rezzed card))]
                 [:div.server-card {:key (:cid card)
                                    :class (str (when (and (< 1 (count content)) (not is-first))
                                                  "shift"))}
                  [card-view card flipped]])))
      [stacked-label content similar-servers opts]]]))

(defn compare-servers-for-stacking [s1]
  (fn [s2]
    (let [ss1 (second s1)
          ss2 (second s2)]
      (and (= (-> ss1 :content first :normalizedtitle)
              (-> ss2 :content first :normalizedtitle))
           (not= s1 s2)
           (empty? (:ices ss1))
           (empty? (:ices ss2))
           (= 1 (count (:content ss1)))
           (= 1 (count (:content ss2)))
           (-> ss1 :content first asset?)
           (-> ss2 :content first asset?)
           (-> ss1 :content first :rezzed)
           (-> ss2 :content first :rezzed)
           (-> ss1 :content first :hosted empty?)
           (-> ss2 :content first :hosted empty?)))))

(defn board-view-corp [player-side identity deck discard servers run]
  (let [rs (:server @run)
        server-type (first rs)]
    [:div.corp-board {:class (if (= player-side :runner) "opponent" "me")}
     (doall
       (for [server (reverse (get-remotes @servers))
             :let [num (remote->num (first server))
                   similar-servers (filter #((compare-servers-for-stacking server) %) (get-remotes @servers))
                   all-servers (conj similar-servers server)]
             :when (or (empty? similar-servers)                                     ; it is a normal server-view
                       (not (get-in @app-state [:options :stacked-servers] false))  ; we're not in stacked mode
                       ; otherwise only show one view for the stacked remote
                       (< num (remote->num (first (first similar-servers)))))]
         (if (or (empty? similar-servers)
                 (not (get-in @app-state [:options :stacked-servers] false)))
           [server-view {:key num
                         :server (second server)
                         :run (when (= server-type (str "remote" num)) @run)}
            {:opts {:name (remote->name (first server))}}]
           [stacked-view {:key num
                          :server (second server)
                          :similar-servers similar-servers
                          :run (when
                                 (some #(= server-type (str "remote" %)) (map #(remote->num (first %)) all-servers))
                                 (= server-type (str "remote" num)) @run)}
            {:opts {:name (remote->name (first server))}}])))
     [server-view {:key "hq"
                   :server (:hq @servers)
                   :central-view [identity-view identity]
                   :run (when (= server-type "hq") @run)}]
     [server-view {:key "rd"
                   :server (:rd @servers)
                   :central-view [deck-view :corp player-side identity deck]
                   :run (when (= server-type "rd") @run)}]
     [server-view {:key "archives"
                   :server (:archives @servers)
                   :central-view [discard-view-corp player-side discard]
                   :run (when (= server-type "archives") @run)}]]))

(defn board-view-runner [player-side identity deck discard rig run]
  (let [is-me (= player-side :runner)
        centrals [:div.runner-centrals
                  [discard-view-runner player-side discard]
                  [deck-view :runner player-side identity deck]
                  [identity-view identity]]
        runner-f (if (and (not is-me)
                          (= "irl" (get-in @app-state [:options :runner-board-order])))
                   reverse
                   seq)]
    [:div.runner-board {:class (if is-me "me" "opponent")}
     (when-not is-me centrals)
     (doall
       (for [zone (runner-f [:program :hardware :resource :facedown])]
         ^{:key zone}
         [:div
          (doall (for [c (zone @rig)]
                   ^{:key (:cid c)}
                   [:div.card-wrapper {:class (when (playable? c) "playable")}
                    [card-view c]]))]))
     (when is-me centrals)]))

(defn play-sfx
  "Plays a list of sounds one after another."
  [sfx soundbank]
  (when-not (empty? sfx)
    (when-let [sfx-key (keyword (first sfx))]
      (.volume (sfx-key soundbank) (/ (str->int (get-in @app-state [:options :sounds-volume])) 100))
      (.play (sfx-key soundbank)))
    (play-sfx (rest sfx) soundbank)))

(defn update-audio [{:keys [gameid sfx sfx-current-id]} soundbank]
  ;; When it's the first game played with this state or when the sound history comes from different game, we skip the cacophony
  (let [sfx-last-played (:sfx-last-played @sfx-state)]
    (when (and (get-in @app-state [:options :sounds])
               (not (nil? sfx-last-played))
               (= gameid (:gameid sfx-last-played)))
      ;; Skip the SFX from queue with id smaller than the one last played, queue the rest
      (let [sfx-to-play (reduce (fn [sfx-list {:keys [id name]}]
                                  (if (> id (:id sfx-last-played))
                                    (conj sfx-list name)
                                    sfx-list)) [] sfx)]
        (play-sfx sfx-to-play soundbank)))
  ;; Remember the most recent sfx id as last played so we don't repeat it later
  (when sfx-current-id
    (swap! sfx-state assoc :sfx-last-played {:gameid gameid :id sfx-current-id}))))

(defn build-win-box
  "Builds the end of game pop up game end"
  [game-state]
  (let [win-shown (r/atom false)]
    (fn [game-state]
      (when (and (:winner @game-state)
                 (not @win-shown))
        (let [winner (:winner @game-state)
              winning-user (:winning-user @game-state)
              turn (:turn @game-state)
              reason (:reason @game-state)
              time (get-in @game-state [:stats :time :elapsed])]
          [:div.win.centered.blue-shade
           [:div
            winning-user
            " ("
            (capitalize winner)
            (cond
              (= "Decked" (capitalize reason))
              (str ") wins due to the Corp being decked on turn " turn)

              (= "Flatline" (capitalize reason))
              (str ") wins by flatline on turn " turn)

              (= "Concede" (capitalize reason))
              (str ") wins by concession on turn " turn)

              :else
              (str ") wins by scoring agenda points on turn " turn))]
           [:div "Time taken: " time " minutes"]
           [:br]
           [build-game-stats (get-in @game-state [:stats :corp]) (get-in @game-state [:stats :runner])]
           [:button.win-right {:on-click #(reset! win-shown true) :type "button"} "✘"]])))))

(defn build-start-box
  "Builds the start-of-game pop up box"
  [my-ident my-user my-hand my-prompt my-keep op-ident op-user op-keep me-quote op-quote my-side]
  (let [visible-quote (r/atom true)
        mulliganed (r/atom false)
        start-shown (r/cursor app-state [:start-shown])]
    (fn [my-ident my-user my-hand my-prompt my-keep op-ident op-user op-keep me-quote op-quote my-side]
      (when (and (not @start-shown)
                 (:username @op-user)
                 (pos? (count @my-hand)))
        (let [squeeze (< 5 (count @my-hand))]
          [:div.win.centered.blue-shade.start-game
           [:div
            [:div
             [:div.box
              [:div.start-game.ident.column
               {:class (case @my-keep "mulligan" "mulligan-me" "keep" "keep-me" "")}
               (when-let [url (image-url @my-ident)]
                 [:img {:src     url :alt (:title @my-ident) :onLoad #(-> % .-target js/$ .show)
                        :class   (when @visible-quote "selected")
                        :onClick #(reset! visible-quote true)}])]
              [:div.column.contestants
               [:div (:username @my-user)]
               [:div.vs "VS"]
               [:div (:username @op-user)]
               [:div.intro-blurb
                (if @visible-quote
                  (str "\"" @me-quote "\"")
                  (str "\"" @op-quote "\""))]]
              [:div.start-game.ident.column
               {:class (case @op-keep "mulligan" "mulligan-op" "keep" "keep-op" "")}
               (when-let [url (image-url @op-ident)]
                 [:img {:src url
                        :alt (:title @op-ident)
                        :onLoad #(-> % .-target js/$ .show)
                        :class (when-not @visible-quote "selected")
                        :onClick #(reset! visible-quote false)}])]]
             (when (not= :spectator @my-side)
               [:div.start-hand
                [:div {:class (when squeeze "squeeze")}
                 (doall (map-indexed
                          (fn [i {:keys [title] :as card}]
                            [:div.start-card-frame {:style (when squeeze
                                                             {:left (* (/ 610 (dec (count @my-hand))) i)
                                                              :position "absolute"})
                                                    :id (str "startcard" i)
                                                    :key (str (:cid card) "-" i "-" @mulliganed)}
                             [:div.flipper
                              [:div.card-back
                               [:img.start-card {:src (str "/img/" (.toLowerCase (:side @my-ident)) ".png")}]]
                              [:div.card-front
                               (when-let [url (image-url card)]
                                 [:div {:on-mouse-enter #(put! zoom-channel card)
                                        :on-mouse-leave #(put! zoom-channel false)}
                                  [:img.start-card {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]
                             (when-let [elem (.querySelector js/document (str "#startcard" i))]
                               (js/setTimeout #(.add (.-classList elem) "flip") (+ 1000 (* i 300))))])
                          @my-hand))]])
             [:div.mulligan
              (if (or (= :spectator @my-side)
                      (and @my-keep @op-keep))
                [cond-button (if (= :spectator @my-side) "Close" "Start Game") true #(swap! app-state assoc :start-shown true)]
                (list ^{:key "keepbtn"} [cond-button "Keep"
                                         (= "mulligan" (-> @my-prompt first :prompt-type))
                                         #(send-command "choice" {:choice {:uuid (->> (-> @my-prompt first :choices)
                                                                                      (filter (fn [c] (= "Keep" (:value c))))
                                                                                      first
                                                                                      :uuid)}})]
                      ^{:key "mullbtn"} [cond-button "Mulligan"
                                         (= "mulligan" (-> @my-prompt first :prompt-type))
                                         #(do (send-command "choice" {:choice {:uuid (->> (-> @my-prompt first :choices)
                                                                                          (filter (fn [c] (= "Mulligan" (:value c))))
                                                                                          first
                                                                                          :uuid)}})
                                              (reset! mulliganed true))]))]]]
           [:br]
           [:button.win-right {:on-click #(swap! app-state assoc :start-shown true) :type "button"} "✘"]])))))

(defn audio-component [{:keys [sfx] :as cursor}]
    (let [s (r/atom {})
        audio-sfx (fn [name] (list (keyword name)
                                   (new js/Howl (clj->js {:src [(str "/sound/" name ".ogg")
                                                                (str "/sound/" name ".mp3")]}))))
        soundbank (apply hash-map (concat
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
                                    (audio-sfx "virus-purge")))]
        (r/create-class
            {:display-name "audio-component"
             :component-did-update
             (fn []
                 (update-audio (select-keys @game-state [:sfx :sfx-current-id :gameid]) soundbank))
             :reagent-render
             (fn [{:keys [sfx] :as cursor}]
              (let [_ @sfx]))}))) ;; make this component rebuild when sfx changes.

(def phase->title
  {"initiation" "Initiation"
   "approach-ice" "Approach ice"
   "encounter-ice" "Encounter ice"
   "pass-ice" "Pass ice"
   "approach-server" "Approach server"})

(defn phase->next-phase-title
  [run]
  (case (:phase @run)
    "initiation" "Approach ice"
    "approach-ice" "Encounter ice"
    "encounter-ice" "Pass ice"
    "pass-ice" (if (zero? (:position @run))
                 "Approach server"
                 "Approach ice")
    "approach-server" "Approach server"
    ;; Error
    "No current run"))

(defn get-run-ices []
  (let [server (-> (:run @game-state)
                   :server
                   first
                   keyword)]
    (get-in @game-state (concat [:corp :servers] [server] [:ices]))))

(defn get-current-ice []
  (let [run-ice (get-run-ices)
        pos (get-in @game-state [:run :position])]
    (when (and pos
               (pos? pos)
               (<= pos (count run-ice)))
      (nth run-ice (dec pos)))))

(defn corp-run-div
  [run]
  [:div.panel.blue-shade
   [:h4 "Current phase:" [:br] (get phase->title (:phase @run))]
   (cond
     (= "approach-ice" (:phase @run))
     (let [current-ice (get-current-ice)]
       [cond-button
        (str "Rez " (:title current-ice))
        (not (rezzed? current-ice))
        #(send-command "rez" {:card current-ice :press-continue true})])

     (= "encounter-ice" (:phase @run))
     (let [current-ice (get-current-ice)]
       [cond-button
        "Fire unbroken subs"
        (and (seq (remove :fired (:subroutines current-ice)))
             (not (every? :broken (:subroutines current-ice))))
        #(send-command "unbroken-subroutines" {:card current-ice})])

     (and (not (:next-phase @run))
          (zero? (:position @run)))
     [checkbox-button
      "Action before access"
      (:corp-phase-43 @run)
      #(send-command "corp-phase-43")])

   [cond-button
    (let [next-phase (:next-phase @run)]
      (if (or next-phase (zero? (:position @run)))
        "No further actions"
        (str "Continue to " (phase->next-phase-title run))))
    (and (not= "initiation" (:phase @run))
         (not= "pass-ice" (:phase @run))
         (not= "corp" (:no-action @run)))
    #(send-command "continue")]

   (when (not= "approach-server" (:phase @run))
     [checkbox-button
      "Stop auto-passing priority"
      "Auto-pass priority"
      (:corp-auto-no-action @run)
      #(send-command "toggle-auto-no-action")])])

(defn runner-run-div
  [run]
  (let [phase (:phase @run)
        next-phase (:next-phase @run)]
    [:div.panel.blue-shade
     [:h4 "Current phase:" [:br] (get phase->title phase)]
     (cond
       (:next-phase @run)
       [cond-button
        (phase->title next-phase)
        (and next-phase
             (not (:no-action @run)))
        #(send-command "start-next-phase")]

       (and (not (:next-phase @run))
            (not (zero? (:position @run)))
            (not= "encounter-ice" (:phase @run)))
       [cond-button
        (str "Continue to " (phase->next-phase-title run))
        (not= "runner" (:no-action @run))
        #(send-command "continue")]

       (zero? (:position @run))
       [cond-button "Successful Run"
        (:no-action @run)
        #(send-command "successful-run")])

     (when (= "encounter-ice" (:phase @run))
       (let [current-ice (get-current-ice)
             title (:title current-ice)]
         [cond-button
          "Let all subroutines fire"
          (and (seq (:subroutines current-ice))
               (not (every? #(or (:broken %) (false? (:resolve %))) (:subroutines current-ice))))
          #(send-command "system-msg"
                         {:msg (str "indicates to fire all unbroken subroutines on " title)})]))

     (when (or (= "approach-server" (:phase @run))
               (= "approach-ice" (:phase @run)))
       [cond-button
        (if (:jack-out @run) "Jack Out" "Undo click")
        (and (or (not= "approach-server" (:phase @run))
                 (:no-action @run))
             (not (:cannot-jack-out @run)))
        (if (:jack-out @run)
          #(send-command "jack-out")
          #(send-msg (r/atom {:msg "/undo-click"})))])

     (when (= "encounter-ice" (:phase @run))
       [cond-button
        "Pass ice and continue"
        (or (not= "runner" (:no-action @run))
            (:jack-out-after-pass @run))
        #(send-command "continue" {:jack-out false})])

     (when (= "encounter-ice" (:phase @run))
       [cond-button
        "Pass ice and jack out"
        (or (not= "runner" (:no-action @run))
            (not (:jack-out-after-pass @run)))
        #(send-command "continue" {:jack-out true})])]))

(defn run-div
  [side run]
  (if (= side :corp)
    [corp-run-div run]
    [runner-run-div run]))

(defn trace-div
  [prompt]
  [:div
   (when-let [base (:base prompt)]
     ;; This is the initial trace prompt
     (if (nil? (:strength prompt))
       (if (= "corp" (:player prompt))
         ;; This is a trace prompt for the corp, show runner link + credits
         [:div.info "Runner: " (:link prompt) [:span {:class "anr-icon link"}]
          " + " (:runner-credits prompt) [:span {:class "anr-icon credit"}]]
         ;; Trace in which the runner pays first, showing base trace strength and corp credits
         [:div.info "Trace: " (when (:bonus prompt) (+ base (:bonus prompt)) base)
          " + " (:corp-credits prompt) [:span {:class "anr-icon credit"}]])
       ;; This is a trace prompt for the responder to the trace, show strength
       (if (= "corp" (:player prompt))
         [:div.info "vs Trace: " (:strength prompt)]
         [:div.info "vs Runner: " (:strength prompt) [:span {:class "anr-icon link"}]])))
   [:div.credit-select
    ;; Inform user of base trace / link and any bonuses
    (when-let [base (:base prompt)]
      (if (nil? (:strength prompt))
        (if (= "corp" (:player prompt))
          (let [strength (when (:bonus prompt) (+ base (:bonus prompt)) base)]
            [:span (str strength " + ")])
          [:span (:link prompt) " " [:span {:class "anr-icon link"}] (str " + " )])
        (if (= "corp" (:player prompt))
          [:span (:link prompt) " " [:span {:class "anr-icon link"}] (str " + " )]
          (let [strength (when (:bonus prompt) (+ base (:bonus prompt)) base)]
            [:span (str strength " + ")]))))
    [:select#credit
     (doall (for [i (range (inc (:choices prompt)))]
              [:option {:value i :key i} i]))] " credits"]
   [:button {:on-click #(send-command "choice"
                                      {:choice (-> "#credit" js/$ .val str->int)})}
    "OK"]])

(defn button-pane [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 corp runner me opponent] :as cursor}]
  (let [s (r/atom {})
        autocomp (r/track (fn [] (get-in @game-state [side :prompt 0 :choices :autocomplete])))]
    (r/create-class
      {:display-name "button-pane"

       :component-did-update
       (fn []
         (when (pos? (count @autocomp))
           (-> "#card-title" js/$ (.autocomplete (clj->js {"source" @autocomp}))))
         (when (get-in @game-state [side :prompt 0 :show-discard])
           (-> ".me .discard .popup" js/$ .fadeIn))
         (if (= "select" (get-in @game-state [side :prompt 0 :prompt-type]))
           (set! (.-cursor (.-style (.-body js/document))) "url('/img/gold_crosshair.png') 12 12, crosshair")
           (set! (.-cursor (.-style (.-body js/document))) "default"))
         (when (= "card-title" (get-in @game-state [side :prompt 0 :prompt-type]))
           (-> "#card-title" js/$ .focus))
         (doseq [{:keys [msg type options]} (get-in @game-state [side :toast])]
           (toast msg type options)))

    :reagent-render
    (fn [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 corp runner me opponent] :as cursor}]
      [:div.button-pane {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                         :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
       (if-let [prompt (first (:prompt @me))]
         [:div.panel.blue-shade
          (when-let [card (:card prompt)]
            [:div {:style {:text-align "center"}
                   :on-mouse-over #(card-highlight-mouse-over % card button-channel)
                   :on-mouse-out #(card-highlight-mouse-out % card button-channel)}
             "Card: " (render-message (:title card))])
          (when (:card prompt)
            [:hr])
          [:h4 (render-message (:msg prompt))]
          (cond
            ;; number prompt
            (get-in prompt [:choices :number])
            (let [n (get-in prompt [:choices :number])]
              [:div
               [:div.credit-select
                [:select#credit {:default-value (get-in prompt [:choices :default] 0)}
                 (doall (for [i (range (inc n))]
                          [:option {:key i :value i} i]))]]
               [:button {:on-click #(send-command "choice"
                                                  {:choice (-> "#credit" js/$ .val str->int)})}
                "OK"]])
            ;; trace prompts require their own logic
            (= (:prompt-type prompt) "trace")
            [trace-div prompt]

            ;; choice of number of credits
            (= (:choices prompt) "credit")
            [:div
             [:div.credit-select
              [:select#credit
               (doall (for [i (range (inc (:credit @me)))]
                        [:option {:value i :key i} i]))] " credits"]
             [:button {:on-click #(send-command "choice"
                                                {:choice (-> "#credit" js/$ .val str->int)})}
              "OK"]]

            ;; auto-complete text box
            (:card-title (:choices prompt))
            [:div
             [:div.credit-select
              [:input#card-title {:placeholder "Enter a card title"
                                  :onKeyUp #(when (= 13 (.-keyCode %))
                                              (-> "#card-submit" js/$ .click)
                                              (.stopPropagation %))}]]
             [:button#card-submit {:on-click #(send-command "choice" {:choice (-> "#card-title" js/$ .val)})}
              "OK"]]

            ;; choice of specified counters on card
            (:counter (:choices prompt))
            (let [counter-type (keyword (:counter (:choices prompt)))
                  num-counters (get-in prompt [:card :counter counter-type] 0)]
              [:div
               [:div.credit-select
                [:select#credit
                 (doall (for [i (range (inc num-counters))]
                          [:option {:key i :value i} i]))] " credits"]
               [:button {:on-click #(send-command "choice"
                                                  {:choice (-> "#credit" js/$ .val str->int)})}
                "OK"]])

            ;; otherwise choice of all present choices
            :else
            (map-indexed (fn [i {:keys [uuid value]}]
                           (when (not= value "Hide")
                             [:button {:key i
                                       :on-click #(send-command "choice" {:choice {:uuid uuid}})
                                       :on-mouse-over
                                       #(card-highlight-mouse-over % value button-channel)
                                       :on-mouse-out
                                       #(card-highlight-mouse-out % value button-channel)
                                       :id {:code value}}
                              (render-message
                                (if-let [title (:title value)]
                                  title
                                  value))]))
                         (:choices prompt)))]
         (if @run
           [run-div side run]
           [:div.panel.blue-shade
            (if (= (keyword @active-player) side)
              (when (and (not (or @runner-phase-12 @corp-phase-12))
                         (zero? (:click @me))
                         (not @end-turn))
                [:button {:on-click #(send-command "end-turn")} "End Turn"])
              (when @end-turn
                [:button {:on-click #(send-command "start-turn")} "Start Turn"]))
            (when (and (= (keyword @active-player) side)
                       (or @runner-phase-12 @corp-phase-12))
              [:button {:on-click #(send-command "end-phase-12")}
               (if (= side :corp) "Mandatory Draw" "Take Clicks")])
            (when (= side :runner)
              [:div
               [cond-button "Remove Tag"
                (and (not (or @runner-phase-12 @corp-phase-12))
                     (pos? (:click @me))
                     (>= (:credit @me) (- 2 (or (:tag-remove-bonus @me) 0)))
                     (pos? (get-in @me [:tag :base])))
                #(send-command "remove-tag")]
               [:div.run-button
                [cond-button "Run" (and (not (or @runner-phase-12 @corp-phase-12))
                                        (pos? (:click @me)))
                 #(do (send-command "generate-runnable-zones")
                      (swap! s update :servers not))]
                [:div.panel.blue-shade.servers-menu {:style (when (:servers @s) {:display "inline"})}
                 (let [servers (get-in @game-state [:runner :runnable-list])]
                   (map-indexed (fn [i label]
                                  [:div {:key i
                                         :on-click #(do (send-command "run" {:server label})
                                                        (swap! s update :servers not))}
                                   label])
                                servers))]]])
            (when (= side :corp)
              [cond-button "Purge"
               (and (not (or @runner-phase-12 @corp-phase-12))
                    (>= (:click @me) 3))
               #(send-command "purge")])
            (when (= side :corp)
              [cond-button "Trash Resource"
               (and (not (or @runner-phase-12 @corp-phase-12))
                    (pos? (:click @me))
                    (>= (:credit @me) (- 2 (or (:trash-cost-bonus @me) 0)))
                    (is-tagged? game-state))
               #(send-command "trash-resource")])
            [cond-button "Draw"
             (and (not (or @runner-phase-12 @corp-phase-12))
                  (pos? (:click @me))
                  (not-empty (:deck @me)))
             #(send-command "draw")]
            [cond-button "Gain Credit"
             (and (not (or @runner-phase-12 @corp-phase-12))
                  (pos? (:click @me)))
             #(send-command "credit")]]))])})))

(defn starting-timestamp []
  [:div.panel.blue-shade
   [:span.float-center
    (str "Game start: " (.toLocaleTimeString (js/Date.)))]])

(defn gameboard []
  (r/with-let [active (r/cursor app-state [:active-page])]
    (when (= "/play" (first @active))
      (let [run (r/cursor game-state [:run])
            side (r/cursor game-state [:side])
            turn (r/cursor game-state [:turn])
            end-turn (r/cursor game-state [:end-turn])
            corp-phase-12 (r/cursor game-state [:corp-phase-12])
            runner-phase-12 (r/cursor game-state [:runner-phase-12])
            corp (r/cursor game-state [:corp])

            runner (r/cursor game-state [:runner])
            active-player (r/cursor game-state [:active-player])
            render-board? (r/track (fn [] (and corp runner side)))
            zoom-card (r/cursor app-state [:zoom])
            background (r/cursor app-state [:options :background])]

        (go (while true
              (let [zoom (<! zoom-channel)]
                (swap! app-state assoc :zoom zoom))))

        (go (while true
              (let [button (<! button-channel)]
                (swap! app-state assoc :button button))))

        (r/create-class
          {:display-name "gameboard"

           :reagent-render
           (fn []
             (when @@render-board?
               (let [me-side (if (= :spectator @side) :corp @side)
                     op-side (utils/other-side me-side)
                     me (r/cursor game-state [me-side])
                     opponent (r/cursor game-state [op-side])
                     me-hand (r/cursor game-state [me-side :hand])
                     op-hand (r/cursor game-state [op-side :hand])
                     me-deck (r/cursor game-state [me-side :deck])
                     op-deck (r/cursor game-state [op-side :deck])
                     me-discard (r/cursor game-state [me-side :discard])
                     op-discard (r/cursor game-state [op-side :discard])
                     me-user (r/cursor game-state [me-side :user])
                     op-user (r/cursor game-state [op-side :user])
                     me-prompt (r/cursor game-state [me-side :prompt])
                     op-prompt (r/cursor game-state [op-side :prompt])
                     me-ident (r/cursor game-state [me-side :identity])
                     op-ident (r/cursor game-state [op-side :identity])
                     me-scored (r/cursor game-state [me-side :scored])
                     op-scored (r/cursor game-state [op-side :scored])
                     corp-servers (r/cursor game-state [:corp :servers])
                     corp-remotes (r/track (fn [] (get-remotes (get-in @game-state [:corp :servers]))))
                     runner-rig (r/cursor game-state [:runner :rig])
                     sfx (r/cursor game-state [:sfx])]
                 [:div.gameboard

                  (let [me-keep (r/cursor game-state [me-side :keep])
                        op-keep (r/cursor game-state [op-side :keep])
                        me-quote (r/cursor game-state [me-side :quote])
                        op-quote (r/cursor game-state [op-side :quote])]
                    [build-start-box me-ident me-user me-hand me-prompt me-keep op-ident op-user op-keep me-quote op-quote side])

                  [build-win-box game-state]

                  [:div {:class @background}]

                  [:div.rightpane
                   [:div.card-zoom
                    [card-zoom zoom-card]]
                   [card-implementation zoom-card]
                   [:div.log
                    [log-pane]
                    [log-typing]
                    [log-input]]]
                  (do (resize-card-zoom) nil)

                  [:div.centralpane
                   (if (= op-side :corp)
                     [board-view-corp me-side op-ident op-deck op-discard corp-servers run]
                     [board-view-runner me-side op-ident op-deck op-discard runner-rig run])
                   (if (= me-side :corp)
                     [board-view-corp me-side me-ident me-deck me-discard corp-servers run]
                     [board-view-runner me-side me-ident me-deck me-discard runner-rig run])]

                  [:div.leftpane
                   [:div.opponent
                    [hand-view op-user (if (= :corp op-side) "HQ" "Grip") op-hand op-prompt corp-remotes
                     (= @side :spectator) "opponent"]]

                   [:div.inner-leftpane
                    [audio-component {:sfx sfx}]

                    [:div.left-inner-leftpane
                     [:div
                      [stats-view opponent]
                      [scored-view op-scored]]
                     [:div
                      [scored-view me-scored]
                      [stats-view me]]]

                    [:div.right-inner-leftpane
                     (let [op-rfg (r/cursor game-state [op-side :rfg])
                           op-current (r/cursor game-state [op-side :current])
                           op-play-area (r/cursor game-state [op-side :play-area])
                           me-rfg (r/cursor game-state [me-side :rfg])
                           me-current (r/cursor game-state [me-side :current])
                           me-play-area (r/cursor game-state [me-side :play-area])]
                       [:div
                        [starting-timestamp]
                        [rfg-view op-rfg "Removed from the game" true]
                        [rfg-view me-rfg "Removed from the game" true]
                        [play-area-view op-user "Play Area" op-play-area]
                        [play-area-view me-user "Play Area" me-play-area]
                        [rfg-view op-current "Current" false]
                        [rfg-view me-current "Current" false]])
                     (when-not (= @side :spectator)
                       [button-pane {:side me-side :active-player active-player :run run :end-turn end-turn
                                     :runner-phase-12 runner-phase-12 :corp-phase-12 corp-phase-12
                                     :corp corp :runner runner :me me :opponent opponent}])]]

                   [:div.me
                    [hand-view me-user (if (= :corp me-side) "HQ" "Grip") me-hand me-prompt
                     corp-remotes true "me"]]]])))})))))
