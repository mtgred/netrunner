(ns netrunner.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize]]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [avatar] :as auth]
            [netrunner.cardbrowser :refer [image-url add-symbols] :as cb]
            [differ.core :as differ]))

(defonce game-state (atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defn init-game [game side]
  (.setItem js/localStorage "gameid" (:gameid @app-state))
  (swap! game-state merge game)
  (swap! game-state assoc :side side)
  (swap! last-state #(identity @game-state)))

(defn notify [text]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text})))

(def zoom-channel (chan))
(def socket (.connect js/io (str js/iourl "/lobby")))
(def socket-channel (chan))
(.on socket "netrunner" #(put! socket-channel (js->clj % :keywordize-keys true)))
(.on socket "disconnect" #(notify "Connection to the server lost. Attempting to reconnect."))
(.on socket "reconnect" #(when (.-onbeforeunload js/window)
                           (notify "Reconnected to the server.")
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

(defn send-command
  ([command] (send-command command nil))
  ([command args]
     (when-not @lock
       (try (js/ga "send" "event" "game" command) (catch js/Error e))
       (reset! lock true)
       (send {:action "do" :gameid (:gameid @game-state) :side (:side @game-state)
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

(defn action-list [{:keys [type zone rezzed advanceable advance-counter advancementcost current-cost] :as card}]
  (-> []
      (#(if (and (= type "Agenda") (>= advance-counter current-cost))
          (cons "score" %) %))
      (#(if (or (and (= type "Agenda") (= (first zone) "servers"))
                (= advanceable "always")
                (and rezzed (= advanceable "while-rezzed"))
                (and (not rezzed) (= advanceable "while-unrezzed")))
          (cons "advance" %) %))
      (#(if (#{"Asset" "ICE" "Upgrade"} type)
          (if (not rezzed) (cons "rez" %) (cons "derez" %))
          %))))

(defn handle-abilities [{:keys [abilities facedown side] :as card} owner]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))]
    (when (not (and (= side "Runner") facedown))
      (cond (> c 1) (-> (om/get-node owner "abilities") js/$ .toggle)
            (= c 1) (if (= (count abilities) 1)
                          (send-command "ability" {:card card :ability 0})
                          (send-command (first actions) {:card card}))))))

(defn handle-card-click [{:keys [type zone counter advance-counter advancementcost advanceable
                                 root] :as card} owner]
  (let [side (:side @game-state)]
    (when (#{(get-in @game-state [:corp :user]) (get-in @game-state [:runner :user])} (:user @app-state))
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
              (> (:click me) 0)))))

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
    (if (> (count code) 0)
      code)))

(defn card-preview-mouse-over [e]
  (if-let [code (get-card-code e)] (put! zoom-channel {:code code})))

(defn card-preview-mouse-out [e]
  (if-let [code (get-card-code e)] (put! zoom-channel false)))

(defn command-panel [data]
  (om/component
   (sab/html
    [:div.commandbuttons 
      [:div.buttoncontainer.panel.blue-shade
        [:button.command {:on-click #(send-command "say" {:user (:user @app-state) :text "STOP!"}) :type "button"} "STOP"]
        [:button.command {:on-click #(send-command "say" {:user (:user @app-state) :text "Thinking..."}) :type "button"} "Thinking..."]
        [:button.command {:on-click #(send-command "say" {:user (:user @app-state) :text "Sorry, I'll be right back."}) :type "button"} "BRB"]]])))

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

(defn ability-costs [ab]
  (when-let [cost (:cost ab)]
    (str (clojure.string/join
           ", " (for [c (partition 2 cost)]
                  (str (case (first c)
                         "credit" (str (second c) " [" (capitalize (name (first c))) "]")
                         (clojure.string/join "" (repeat (second c) (str "[" (capitalize (name (first c))) "]")))
                         )))) ": ")))

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last))

(defn remote->name [server]
  (let [num (remote->num server)]
    (str "Server " num)))

(defn get-remotes [servers]
 (->> servers 
     (filter #(not (#{:hq :rd :archives} (first %))))
     (sort-by #(remote->name (first %)))))

(defn remote-list [remotes]
  (->> remotes (map #(remote->name (first %))) sort))
  
(defn card-view [{:keys [zone code type abilities counter advance-counter advancementcost current-cost subtype
                         advanceable rezzed strength current-strength title remotes selected hosted
                         side rec-counter facedown]
                  :as cursor}
                 owner {:keys [flipped] :as opts}]
  (om/component
   (when code
     (sab/html
      [:div.card-frame
       [:div.blue-shade.card {:class (when selected "selected") :draggable true
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
         (when (> counter 0) [:div.darkbg.counter counter])
         (when (> rec-counter 0) [:div.darkbg.recurring.counter rec-counter])
         (when (> advance-counter 0) [:div.darkbg.advance.counter advance-counter])]
        (when (and current-strength (not= strength current-strength))
              current-strength [:div.darkbg.strength current-strength])
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
          (when (> (+ (count actions) (count abilities)) 1)
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
       (when (> (count hosted) 0)
         [:div.hosted
          (om/build-all card-view hosted {:key :cid})])]))))

(defn drop-area [side server hmap]
  (merge hmap {:on-drop #(handle-drop % server)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)}))

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
                      [:div.card-wrapper {:class (if (and (not (:selected card)) (playable? card))
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
     (when (> (count deck) 0)
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
     (when (> (count deck) 0)
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
          (if (= (:side @game-state) :runner)
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

(defmethod stats-view "Runner" [{:keys [user click credit run-credit memory link tag brain-damage agenda-point
                                        tagged max-hand-size]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :runner)]
      [:div.stats.panel.blue-shade {}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (> click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (> credit 1) "s" "")
                  (when (> run-credit 0)
                    (str " (" run-credit " for run)")))
        (when me? (controls :credit))]
       [:div (str memory " Memory Unit" (if (not= memory 0) "s" "")) (when (< memory 0) [:div.warning "!"]) (when me? (controls :memory))]
       [:div (str link " Link" (if (> link 1) "s" "")) (when me? (controls :link))]
       [:div (str agenda-point " Agenda Point" (when (> agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str tag " Tag" (if (> tag 1) "s" "")) (when (or (> tag 0) (> tagged 0)) [:div.warning "!"]) (when me? (controls :tag))]
       [:div (str brain-damage " Brain Damage" (if (> brain-damage 1) "s" ""))
        (when me? (controls :brain-damage))]
       [:div (str max-hand-size " Max hand size") (when me? (controls :max-hand-size))]]))))

(defmethod stats-view "Corp" [{:keys [user click credit agenda-point bad-publicity max-hand-size]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :corp)]
      [:div.stats.panel.blue-shade {}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (> click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (> credit 1) "s" "")) (when me? (controls :credit))]
       [:div (str agenda-point " Agenda Point" (when (> agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str bad-publicity " Bad Publicit" (if (> bad-publicity 1) "ies" "y"))
        (when me? (controls :bad-publicity))]
       [:div (str max-hand-size " Max hand size") (when me? (controls :max-hand-size))]]))))

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
            (om/build card-view ice {:opts {:flipped (not (:rezzed ice))}}))])
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
    [:div.runner-board
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
        max-size (:max-hand-size me)]
    (if (> (count (:hand me)) max-size)
      (om/set-state! owner :warning (str "Discard to " max-size " cards"))
      (do (om/set-state! owner :warning nil)
          (send-command "end-turn")))))

(defn gameboard [{:keys [side gameid active-player run end-turn] :as cursor} owner]
  (reify
    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card)))))

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (when (get-in cursor [side :prompt 0 :show-discard])
        (-> ".me .discard .popup" js/$ .fadeIn)))

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
                (om/build rfg-view {:cards (:play-area me)})
                (om/build rfg-view {:cards (:current opponent) :name "Current"})
                (om/build rfg-view {:cards (:current me) :name "Current"})]
               (when-not (= side :spectator)
                 [:div.button-pane { :on-mouse-over card-preview-mouse-over
                                     :on-mouse-out  card-preview-mouse-out  }
                  (when-not (:keep me)
                    [:div.panel.blue-shade
                     [:h4 "Keep hand?"]
                     [:button {:on-click #(send-command "keep")} "Keep"]
                     [:button {:on-click #(send-command "mulligan")} "Mulligan"]])

                  (when (:keep me)
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
                               (cond-button "Succesful Run" (:no-action run) #(send-command "access"))
                               (cond-button "Continue" (:no-action run) #(send-command "continue")))
                             (cond-button "Jack Out" (not (get-in cursor [:run :cannot-jack-out]))
                                          #(send-command "jack-out"))]
                            [:div.panel.blue-shade
                             (cond-button "No more action" (not (:no-action run))
                                          #(send-command "no-action"))]))
                        [:div.panel.blue-shade
                         (when-let [warning (:warning state)] [:h4 warning])
                         (if (= (keyword active-player) side)
                           (when (and (zero? (:click me)) (not end-turn))
                             [:button {:on-click #(handle-end-turn cursor owner)} "End Turn"])
                           (when end-turn
                             [:button {:on-click #(send-command "start-turn")} "Start Turn"]))
                         (when (= side :runner)
                           [:div
                            (cond-button "Remove Tag"
                                         (and (>= (:click me) 1) (>= (:credit me) (- 2 (or (:tag-remove-bonus me) 0))) (>= (:tag me) 1))
                                         #(send-command "remove-tag"))
                            [:div.run-button
                             (cond-button "Run" (and (>= (:click me) 1)
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
                           (cond-button "Trash Resource" (and (> (:click me) 0) (>= (:credit me) (- 2 (or (:trash-cost-bonus me) 0)))
                                                              (or (> (:tagged opponent) 0)
                                                                  (> (:tag opponent) 0)))
                                        #(send-command "trash-resource")))
                         (cond-button "Draw" (>= (:click me) 1) #(send-command "draw"))
                         (cond-button "Gain Credit" (>= (:click me) 1) #(send-command "credit"))])))])]

              [:div.board
               (om/build board-view {:player opponent :run run})
               (om/build board-view {:player me :run run})]]
             [:div.me
              (om/build zones {:player me :remotes (get-remotes (get-in cursor [:corp :servers]))})]]
            [:div.rightpane {}
             [:div.card-zoom
              (when-let [card (om/get-state owner :zoom)]
                (om/build cb/card-view card))]
             (om/build command-panel cursor)
             (om/build log-pane (:log cursor))]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
