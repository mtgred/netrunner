(ns nr.gameboard.board
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :as s :refer [capitalize includes? join lower-case split blank? starts-with? ends-with?]]
            [differ.core :as differ]
            [game.core.card :refer [has-subtype? asset? rezzed? ice? corp?
                                    faceup? installed? same-card? in-scored?
                                    get-counters]]
            [jinteki.utils :refer [str->int is-tagged? add-cost-to-label] :as utils]
            [jinteki.cards :refer [all-cards]]
            [nr.ajax :refer [GET PUT DELETE]]
            [nr.appstate :refer [app-state]]
            [nr.auth :as auth]
            [nr.cardbrowser :refer [card-as-text]]
            [nr.end-of-game-stats :refer [build-game-stats]]
            [nr.gameboard.actions :refer [send-command toast]]
            [nr.gameboard.log :refer [send-msg should-scroll]]
            [nr.gameboard.card-preview :refer [card-preview-mouse-over card-preview-mouse-out
                                               card-highlight-mouse-over card-highlight-mouse-out zoom-channel]]
            [nr.gameboard.right-pane :refer [content-pane]]
            [nr.gameboard.player-stats :refer [stat-controls stats-view]]
            [nr.gameboard.replay :refer [replay-panel]]
            [nr.gameboard.state :refer [game-state replay-side not-spectator?]]
            [nr.translations :refer [tr tr-side]]
            [nr.utils :refer [banned-span influence-dot influence-dots map-longest
                              toastr-options render-icons render-message
                              checkbox-button cond-button get-image-path
                              non-game-toast image-or-face]]
            [reagent.core :as r]))

(declare stacked-card-view show-distinct-cards)

(defonce board-dom (atom {}))
(defonce sfx-state (atom {}))

(defn- image-url [{:keys [side code title] :as card}]
  (let [lang (get-in @app-state [:options :language] "en")
        res (get-in @app-state [:options :card-resolution] "default")
        special-user (get-in @game-state [(keyword (lower-case side)) :user :special])
        special-wants-art (get-in @game-state [(keyword (lower-case side)) :user :options :show-alt-art])
        viewer-wants-art (get-in @app-state [:options :show-alt-art])
        show-art (and special-user special-wants-art viewer-wants-art)
        art (if show-art
              (get-in @game-state [(keyword (lower-case side)) :user :options :alt-arts (keyword code)] "stock")
              "stock")
        card (if (or (:face card) (:images card)) card (get @all-cards title))
        images (image-or-face card)]
    (get-image-path images (keyword lang) (keyword res) (keyword art))))


(defn notify
  "Send a notification to the chat, and a toast to the current player of the specified severity"
  [text severity]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text}))
  (toast text severity nil))

(defonce button-channel (chan))

(defn action-list
  [{:keys [type zone rezzed advanceable advance-counter
           advancementcost current-advancement-requirement] :as card}]
  (cond->> []
    ;; advance
    (or (and (= type "Agenda")
             (#{"servers" "onhost"} (first zone)))
        (= advanceable "always")
        (and rezzed
             (= advanceable "while-rezzed"))
        (and (not rezzed)
             (= advanceable "while-unrezzed")))
    (cons "advance")
    ;; score
    (and (= type "Agenda") (>= advance-counter (or current-advancement-requirement advancementcost)))
    (cons "score")
    ;; trash
    (#{"ICE" "Program"} type)
    (cons "trash")
    ;; rez
    (and (#{"Asset" "ICE" "Upgrade"} type)
         (not rezzed))
    (cons "rez")
    ;; derez
    (and (#{"Asset" "ICE" "Upgrade"} type)
         rezzed)
    (cons "derez")))

(defn handle-abilities
  [side {:keys [abilities corp-abilities runner-abilities facedown type] :as card} c-state]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))
        card-side (keyword (.toLowerCase (:side card)))]
    (swap! c-state dissoc :keep-menu-open)
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

(defn close-abilities
  [c-state]
  (swap! c-state dissoc :abilities :corp-abilities :runner-abilities :keep-menu-open))

(defn playable? "Checks whether a card or ability is playable"
  [action]
  (:playable action))

(defn handle-card-click [{:keys [type zone] :as card} c-state]
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

        ;; Runner clicking on a runner card
        (and (= side :runner)
             (= "Runner" (:side card))
             (= "hand" (first zone))
             (playable? card))
        (send-command "play" {:card card})

        ;; Corp clicking on a corp card
        (and (= side :corp)
             (= "Corp" (:side card))
             (= "hand" (first zone))
             (playable? card))
        (if (= "Operation" type)
          (send-command "play" {:card card})
          (if (:servers @c-state)
            (do (swap! c-state dissoc :servers)
                (send-command "generate-install-list" nil))
            (do (swap! c-state assoc :servers true)
                (send-command "generate-install-list" {:card card}))))

        :else
        (case (first zone)
          ("current" "onhost" "play-area" "scored" "servers" "rig")
          (handle-abilities side card c-state)
          ; else
          nil)))))

(defn spectator-view-hidden?
  "Checks if spectators are allowed to see hidden information, such as hands and face-down cards"
  []
  (and (get-in @game-state [:options :spectatorhands])
       (not (not-spectator?))))

(defn handle-dragstart [e card]
  (-> e .-target js/$ (.addClass "dragged"))
  (-> e .-dataTransfer (.setData "card" (.stringify js/JSON (clj->js card)))))

(defn handle-drop [e server]
  (-> e .-target js/$ (.removeClass "dragover"))
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))
        side (if (#{"HQ" "R&D" "Archives"} server) "Corp" "Runner")]
    (when (not= "Identity" (:type card))
      (send-command "move" {:card card :server server}))))

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
    (str (tr [:game.server "Server"]) " " num)))

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
   (let [card-back (get-in @app-state [:options :card-back])
         s (lower-case side)
         alt (if (nil? alt-alt-text)
               (str "Facedown " s " card")
               alt-alt-text)
         tag (->> class-list
                  vec
                  (concat ["img" "card"])
                  (join ".")
                  keyword)]
     [tag {:src (str "/img/" card-back "-" s ".png")
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

(defn card-implementation [zoom-card]
  (when-let [card @zoom-card]
    (let [implemented (:implementation card)]
      (case implemented
        (:full "full") nil
        [:div.panel.blue-shade.implementation {:style {:right (get-in @app-state [:options :log-width])}}
         (case implemented
           nil [:span.unimplemented (tr [:game.unimplemented "Unimplemented"])]
           [:span.impl-msg implemented])]))))

(defn card-zoom-display
  [zoom-card img-side]
  (when-let [card @zoom-card]
    [:<>
     [:div.card-preview.blue-shade
      {:on-click #(reset! img-side (not @img-side))}
      (let [url (image-url card)
            show-img (= "image" (get-in @app-state [:options :card-zoom] "image"))]
        (if (and @img-side url show-img)
          [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}]
          [card-as-text card false]))]
     (when (get-in @app-state [:options :pin-zoom] false)
       [:button.win-right {:on-click #(reset! zoom-card false) :type "button"} "✘"])]))

(defn card-zoom [zoom-card img-side]
  (if @zoom-card
    (do (-> ".card-zoom" js/$ (.addClass "fade"))
        [card-zoom-display zoom-card img-side])
    (do (-> ".card-zoom" js/$ (.removeClass "fade")) nil)))

(defn card-zoom-view [zoom-card]
  (let [zoomed-card (r/atom nil)
        img-side (r/atom true)]
    (fn [zoom-card]
      (let [pin (get-in @app-state [:options :pin-zoom] false)]
        (when (or @zoom-card
                  (and (not @zoom-card) (not pin)))
          (reset! zoomed-card @zoom-card)
          (reset! img-side true))
        [:<>
         [:div.card-zoom
          [card-zoom zoomed-card img-side]]
         [card-implementation zoomed-card]]))))

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

(defn list-abilities
  [ab-type card c-state abilities]
  (map-indexed
    (fn [i ab]
      (let [command (case ab-type
                      :runner "runner-ability"
                      :corp "corp-ability"
                      :ability (if (:dynamic ab) "dynamic-ability" "ability"))
            args (merge {:card card}
                        (if (:dynamic ab)
                          (select-keys ab [:dynamic :source :index])
                          {:ability i}))]
        [:div {:key i
               :on-click #(do
                            (send-command command args)
                            (if (:keep-menu-open ab)
                              (swap! c-state assoc :keep-menu-open (keyword (:keep-menu-open ab)))
                              (close-abilities c-state)))}
         (render-icons (add-cost-to-label ab))]))
      abilities))

(defn check-keep-menu-open
  [card c-state]
  (let [side (:side @game-state)
        keep-menu-open (case (:keep-menu-open @c-state)
                        :while-credits-left
                        (pos? (get-in @game-state [side :credit]))

                        :while-clicks-left
                        (pos? (get-in @game-state [side :click]))

                        :while-2-clicks-left
                        (>= (get-in @game-state [side :click]) 2)

                        :while-3-clicks-left
                        (>= (get-in @game-state [side :click]) 3)

                        :while-4-clicks-left
                        (>= (get-in @game-state [side :click]) 4)

                        :while-cards-in-hand
                        (not-empty (get-in @game-state [side :hand]))

                        :while-power-tokens-left
                        (pos? (get-counters card :power))

                        :while-2-power-tokens-left
                        (>= (get-counters card :power) 2)

                        :while-3-power-tokens-left
                        (>= (get-counters card :power) 3)

                        :while-5-power-tokens-left
                        (>= (get-counters card :power) 5)

                        :while-advancement-tokens-left
                        (pos? (get-counters card :advancement))

                        :while-agenda-tokens-left
                        (pos? (get-counters card :agenda))

                        :while-virus-tokens-left
                        (pos? (get-counters card :virus))

                        :while-2-virus-tokens-left
                        (>= (get-counters card :virus) 2)

                        :if-abilities-available
                        (pos? (+ (count (:corp-abilities card))
                                 (count (:runner-abilities card))
                                 (count (remove #(or (starts-with? (:label % "") "Toggle auto-resolve on")
                                                     (ends-with? (:label % "") "(start of turn)"))
                                                (:abilities card)))))

                        :for-agendas
                        (or (some #(= "score" %) (action-list card))          ; can score
                            (not (zero? (get-in @game-state [side :click])))) ; clicks left

                        :forever true

                        false)]
    (when-not keep-menu-open (close-abilities c-state))
    keep-menu-open))

(defn runner-abs [card c-state runner-abilities subroutines title]
  (when (:runner-abilities @c-state)
    [:div.panel.blue-shade.runner-abilities {:style {:display "inline"}}
     [:button.win-right {:on-click #(close-abilities c-state) :type "button"} "✘"]
     (when (or (seq runner-abilities)
               (seq subroutines))
       [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
     (list-abilities :runner card c-state runner-abilities)
     (when (seq subroutines)
       [:div {:on-click #(do (send-command "system-msg"
                                           {:msg (str "indicates to fire all unbroken subroutines on " title)})
                             (close-abilities c-state))}
        (tr [:game.let-subs-fire "Let unbroken subroutines fire"])])
     (when (seq subroutines)
       [:span.float-center (tr [:game.subs "Subroutines"]) ":"])
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
     [:button.win-right {:on-click #(close-abilities c-state) :type "button"} "✘"]
     (when (seq corp-abilities)
       [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
     (list-abilities :corp card c-state corp-abilities)]))

;; TODO (2020-10-08): We're using json as the transport layer for server-client
;; communication, so every non-key keyword is converted to a string, which blows.
;; Until this is changed, it's better to redefine this stuff in here and just not
;; worry about it.

;; TODO (2021-04-24): If this ever gets fixed, remember to change functions in
;; button-pane as well.
(letfn
  [(is-type?  [card value] (= value (:type card)))
   (identity? [card] (or (is-type? card "Fake-Identity")
                         (is-type? card "Identity")))
   (get-nested-host [card] (if (:host card)
                             (recur (:host card))
                             card))
   (get-zone [card] (:zone (get-nested-host card)))
   (in-play-area? [card] (= (get-zone card) ["play-area"]))
   (in-current? [card] (= (get-zone card) ["current"]))
   (in-scored? [card] (= (get-zone card) ["scored"]))
   (corp? [card] (= (:side card) "Corp"))
   (installed? [card] (or (:installed card)
                          (= "servers" (first (get-zone card)))))
   (rezzed? [card] (:rezzed card))
   (runner? [card] (= (:side card) "Runner"))
   (condition-counter? [card] (and (:condition card)
                                   (or (is-type? card "Event")
                                       (is-type? card "Operation"))))
   (facedown? [card] (or (when (not (condition-counter? card))
                           (= (get-zone card) ["rig" "facedown"]))
                         (:facedown card)))]
  (defn active?
    "Checks if the card is active and should receive game events/triggers."
    [card]
    (or (identity? card)
        (in-play-area? card)
        (in-current? card)
        (in-scored? card)
        (and (corp? card)
             (installed? card)
             (rezzed? card))
        (and (runner? card)
             (installed? card)
             (not (facedown? card))))))

(defn card-abilities [card c-state abilities subroutines]
  (let [actions (action-list card)
        dynabi-count (count (filter :dynamic abilities))]
    (when (and (:abilities @c-state)
               (or (nil? (:keep-menu-open @c-state))
                   (check-keep-menu-open card c-state))
               (or (pos? (+ (count actions)
                            (count abilities)
                            (count subroutines)))
                   (some #{"derez" "rez" "advance" "trash"} actions)
                   (= type "ICE")))
      [:div.panel.blue-shade.abilities {:style {:display "inline"}}
       [:button.win-right {:on-click #(close-abilities c-state) :type "button"} "✘"]
       (when (seq actions)
         [:span.float-center (tr [:game.actions "Actions"]) ":"])
       (when (seq actions)
         (map-indexed
           (fn [i action]
             (let [keep-menu-open (case action
                                    "derez" false
                                    "rez" :if-abilities-available
                                    "trash" false
                                    "advance" :for-agendas
                                    "score" false
                                    false)]
               [:div {:key i
                      :on-click #(do (send-command action {:card card})
                                     (if keep-menu-open
                                       (swap! c-state assoc :keep-menu-open keep-menu-open)
                                       (close-abilities c-state)))}
                (capitalize action)]))
             actions))
       (when (and (active? card)
                  (seq abilities))
         [:span.float-center (tr [:game.abilities "Abilities"]) ":"])
       (when (and (active? card)
                  (seq abilities))
         (list-abilities :ability card c-state abilities))
       (when (seq (remove :fired subroutines))
         [:div {:on-click #(do (send-command "unbroken-subroutines" {:card card})
                               (close-abilities c-state))}
          (tr [:game.fire-unbroken "Fire unbroken subroutines"])])
       (when (seq subroutines)
         [:span.float-center (tr [:game.subs "Subroutines"]) ":"])
       (when (seq subroutines)
         (map-indexed
           (fn [i sub]
             [:div {:key i
                    :on-click #(do (send-command "subroutine" {:card card
                                                               :subroutine i})
                                   (close-abilities c-state))}
              [:span (cond (:broken sub)
                           {:class :disabled
                            :style {:font-style :italic}}
                           (false? (:resolve sub))
                           {:class :dont-resolve
                            :style {:text-decoration :line-through}})
               (render-icons (str " [Subroutine] " (:label sub)))]
              [:span.float-right
               (cond (:broken sub) banned-span
                     (:fired sub) "✅")]])
           subroutines))])))

(defn face-down?
  "Returns true if the installed card should be drawn face down."
  [{:keys [side type facedown rezzed host] :as card}]
  (if (= side "Corp")
    (and (not= type "Operation")
         (not rezzed)
         (not= (:side host) "Runner"))
    facedown))

(defn card-view
  [card flipped]
  (let [c-state (r/atom {})]
    (fn [{:keys [zone code type abilities counter advance-counter advancementcost current-advancement-requirement
                 subtype subtypes advanceable rezzed strength current-strength title selected hosted
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
        (if (or (not code) flipped facedown)
          (let [facedown-but-known (or (not (or (not code) flipped facedown))
                                       (spectator-view-hidden?)
                                       (= (:side @game-state) (keyword (lower-case side))))
                alt-str (when facedown-but-known (str "Facedown " title))]
            [facedown-card side ["bg"] alt-str])
          (when-let [url (image-url card)]
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
        (when (active? card)
          (let [server-card (get @all-cards title)]
            [:div.darkbg.additional-subtypes
             (join " - " (remove (into #{} (:subtypes server-card)) subtypes))]))

        (when (and (= zone ["hand"])
                   (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
          [server-menu card c-state])]
       (when (pos? (+ (count runner-abilities) (count subroutines)))
         [runner-abs card c-state runner-abilities subroutines title])

       (when (pos? (count corp-abilities))
         [corp-abs card c-state corp-abilities])

       [card-abilities card c-state abilities subroutines]
       (when (pos? (count hosted))
         [:div.hosted
          (if (and (not (ice? card))
                   (get-in @app-state [:options :stacked-cards] false))
            ; stacked mode
            (let [distinct-hosted (vals (group-by :title hosted))]
              (show-distinct-cards distinct-hosted))

            (doall
              (for [card hosted]
                (let [flipped (face-down? card)]
                  ^{:key (:cid card)}
                  [card-view card flipped]))))])])))

(defn show-distinct-cards
  [distinct-cards]
  (doall (apply concat (for [cards distinct-cards] ; apply concat for one-level flattening
                         (let [hosting (remove #(zero? (count (:hosted %))) cards) ; There are hosted cards on these
                               others (filter #(zero? (count (:hosted %))) cards)
                               facedowns (filter face-down? others)
                               others (remove face-down? others)]
                           [; Hosting
                            (for [c hosting]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c (face-down? c)]])
                            ; Facedown
                            (for [c facedowns]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c true]])
                            ; Rest
                            (if (not-empty others)
                              (if (= 1 (count others))
                                (let [c (first others)
                                      flipped (face-down? c)]
                                  ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                    [card-view c flipped]])
                                [stacked-card-view others]))])))))

(defn stacked-card-view
  [cards]
  [:div.stacked
   (doall
     (for [c cards]
       (let [flipped (face-down? c)]
         ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                           [card-view c flipped]])))])

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
  (if (:replay @game-state)
    (= (get-in @game-state [@replay-side :user :_id]) (:_id user))
    (= (:_id user) (-> @app-state :user :_id))))

(defn build-hand-card-view
  [hand size prompt wrapper-class]
  [:div
   (doall (map-indexed
            (fn [i card]
              [:div {:key (or (:cid card) i)
                     :class (str
                              (if (and (not= "select" (-> @prompt first :prompt-type))
                                       (not (:selected card))
                                       (playable? card))
                                "playable" "")
                              " "
                              wrapper-class)
                     :style {:left (when (< 1 size) (* (/ 320 (dec size)) i))}}
               (cond
                 (spectator-view-hidden?)
                 [card-view (dissoc card :new :selected)]
                 (:cid card)
                 [card-view card]
                 :else
                 [facedown-card (:side card)])])
            hand))])

(defn hand-view [name translated-name side hand hand-size hand-count prompt popup popup-direction]
  (let [s (r/atom {})]
    (fn [name translated-name side hand hand-size hand-count prompt popup popup-direction]
      (let [size (if (nil? @hand-count) (count @hand) @hand-count)
            filled-hand (concat @hand (repeat (- size (count @hand)) {:side (if (= :corp side) "Corp" "Runner")}))]
        [:div.hand-container
         [:div.hand-controls
          [:div.panel.blue-shade.hand
           (drop-area name {:class (when (> size 6) "squeeze")})
           [build-hand-card-view filled-hand size prompt "card-wrapper"]
           [label filled-hand {:opts {:name translated-name
                                      :fn (fn [cursor] (str size "/" (:total @hand-size)))}}]]
          (when popup
            [:div.panel.blue-shade.hand-expand
             {:on-click #(-> (:hand-popup @s) js/$ .fadeToggle)}
             "+"])]
         (when popup
           [:div.panel.blue-shade.popup {:ref #(swap! s assoc :hand-popup %) :class popup-direction}
            [:div
             [:a {:on-click #(close-popup % (:hand-popup @s) nil false false)} (tr [:game.close "Close"])]
             [:label (tr [:game.card-count] size)]
             (let [{:keys [total]} @hand-size]
               (stat-controls :hand-size [:div.hand-size (str total " " (tr [:game.max-hand "Max hand size"]))]))
             [build-hand-card-view filled-hand size prompt "card-popup-wrapper"]]])]))))

(defn show-deck [event ref]
  (-> ((keyword (str ref "-content")) @board-dom) js/$ .fadeIn)
  (-> ((keyword (str ref "-menu")) @board-dom) js/$ .fadeOut)
  (send-command "view-deck"))

(defn identity-view [render-side identity hand-count]
  (let [is-runner (= :runner render-side)
        title (if is-runner (tr [:game.grip "Grip"]) (tr [:game.hq "HQ"]))]
    [:div.blue-shade.identity
     [card-view @identity]
     [:div.header {:class "darkbg server-label"}
      (str title " (" hand-count ")")]]))

(defn deck-view [render-side player-side identity deck deck-count]
   (let [is-runner (= :runner render-side)
         title (if is-runner (tr [:game.stack "Stack"]) (tr [:game.r&d "R&D"]))
         ref (if is-runner "stack" "rd")
         menu-ref (keyword (str ref "-menu"))
         content-ref (keyword (str ref "-content"))]
     (fn [render-side player-side identity deck]
       ; deck-count is only sent to live games and does not exist in the replay
       (let [deck-count-number (if (nil? @deck-count) (count @deck) @deck-count)]
         [:div.blue-shade.deck
          (drop-area title {:on-click #(-> (menu-ref @board-dom) js/$ .toggle)})
          (when (pos? deck-count-number)
            [facedown-card (:side @identity) ["bg"] nil])
          [:div.header {:class "darkbg server-label"}
           (str title " (" deck-count-number ")")]
          (when (= render-side player-side)
            [:div.panel.blue-shade.menu {:ref #(swap! board-dom assoc menu-ref %)}
             [:div {:on-click #(do (send-command "shuffle")
                                   (-> (menu-ref @board-dom) js/$ .fadeOut))} (tr [:game.shuffle "Shuffle"])]
             [:div {:on-click #(show-deck % ref)} (tr [:game.show "Show"])]])
          (when (= render-side player-side)
            [:div.panel.blue-shade.popup {:ref #(swap! board-dom assoc content-ref %)}
             [:div
              [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" false true)}
               (tr [:game.close "Close"])]
              [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" true true)}
               (tr [:game.close-shuffle "Close & Shuffle"])]]
             (doall
               (for [card @deck]
                 ^{:key (:cid card)}
                 [card-view card]))])]))))

(defn discard-view-runner [player-side discard]
  (let [s (r/atom {})]
    (fn [player-side discard]
      [:div.blue-shade.discard
       (drop-area "Heap" {:on-click #(-> (:popup @s) js/$ .fadeToggle)})
       (when-not (empty? @discard)
         [card-view (last @discard)])
       [:div.header {:class "darkbg server-label"}
        (str (tr [:game.heap "Heap"]) " (" (count @discard) ")")]
       [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                     :class (if (= player-side :runner) "me" "opponent")}
        [:div
         [:a {:on-click #(close-popup % (:popup @s) nil false false)} (tr [:game.close "Close"])]]
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
         (when-not (empty? @discard)
           [:<> {:key "discard"} (draw-card (last @discard))])
         [:div.header {:class "darkbg server-label"}
          (let [total (count @discard)
                face-up (count (filter faceup? @discard))]
            (str (tr [:game.archives "Archives"])
                 ;; use non-breaking space to keep counts on same line
                 " (" (tr [:game.up-down-count] total face-up) ")"))]
         [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                       :class (if (= (:side @game-state) :runner) "opponent" "me")}
          [:div
           [:a {:on-click #(close-popup % (:popup @s) nil false false)} (tr [:game.close "Close"])]
           [:label (let [total (count @discard)
                         face-up (count (filter faceup? @discard))]
                     (tr [:game.face-down-count] total face-up))]]
          (doall
            (for [[idx c] (map-indexed vector @discard)]
              ^{:key idx}
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
               [:a {:on-click #(close-popup % (:rfg-popup @dom) nil false false)} (tr [:game.close "Close"])]
               [:label (tr [:game.card-count] size)]]
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

(defn scored-view [scored agenda-point me?]
  (let [size (count @scored)
        ctrl (if me? stat-controls (fn [key content] content))]
    [:div.panel.blue-shade.scored.squeeze
     (doall
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:key i
                                          :style {:left (when (> size 1) (* (/ 128 (dec size)) i))}}
                       [:div [card-view card]]])
                    @scored))
     [label @scored {:opts {:name (tr [:game.scored-area "Scored Area"])}}]
     [:div.stats-area
      (ctrl :agenda-point [:div (tr [:game.agenda-count] @agenda-point)])]]))

(defn run-arrow [run]
  [:div.run-arrow [:div {:class (cond
                                  (= "approach-ice" (:phase run))
                                  "approach"
                                  (= "encounter-ice" (:phase run))
                                  "encounter"
                                  :else
                                  "")}]])

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
      (when-let [run-card (:source-card run)]
        [:div.run-card [card-img run-card]])
      (doall
        (for [ice (reverse ices)]
          [:div.ice {:key (:cid ice)
                     :class (when (not-empty (:hosted ice)) "host")
                     ; Since CSS flex display does not work correctly on rotated objects
                     ; (and we're doing a lot of rotating in our front end), this hack is
                     ; necessary to align ice with hosted cards. -- lostgeek, 17.04.2021
                     :style {:left (when (not-empty (:hosted ice)) (* 21 (dec (count (:hosted ice)))))}}
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
    [label full-server-names (update-in opts [:opts] assoc
                                        :classes "server-label"
                                        :name (str "Servers " (join ", " numbers))
                                        :hide-cursor true)]))

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
      (when-let [run-card (:source-card run)]
        [:div.run-card [card-img run-card]])
      (when (and run (not current-ice))
        [run-arrow run])]
     [:div.content
      [:div.stacked
       (doall (for [card content]
                (let [is-first (= card (first content))
                      flipped (not (:rezzed card))]
                  [:div.server-card {:key (:cid card)
                                     :class (str (when (and (< 1 (count content)) (not is-first))
                                                   "shift"))}
                   [card-view card flipped]])))
       [stacked-label content similar-servers opts]]]]))

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

(defn board-view-corp [player-side identity deck deck-count hand hand-count discard servers run]
  (let [rs (:server @run)
        server-type (first rs)
        side-class (if (= player-side :runner) "opponent" "me")
        hand-count-number (if (nil? @hand-count) (count @hand) @hand-count)]
    [:div.outer-corp-board {:class [side-class
                                    (when (get-in @app-state [:options :sides-overlap]) "overlap")]}
     [:div.corp-board {:class side-class}
      (doall
        (for [server (reverse (get-remotes @servers))
              :let [num (remote->num (first server))
                    similar-servers (filter #((compare-servers-for-stacking server) %) (get-remotes @servers))
                    all-servers (conj similar-servers server)]
              :when (or (empty? similar-servers)            ; it is a normal server-view
                      (not (get-in @app-state [:options :stacked-cards] false)) ; we're not in stacked mode
                      ; otherwise only show one view for the stacked remote
                      (< num (remote->num (first (first similar-servers)))))]
          (if (or (empty? similar-servers)
                (not (get-in @app-state [:options :stacked-cards] false)))
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
                    :central-view [identity-view :corp identity hand-count-number]
                    :run (when (= server-type "hq") @run)}]
      [server-view {:key "rd"
                    :server (:rd @servers)
                    :central-view [deck-view :corp player-side identity deck deck-count]
                    :run (when (= server-type "rd") @run)}]
      [server-view {:key "archives"
                    :server (:archives @servers)
                    :central-view [discard-view-corp player-side discard]
                    :run (when (= server-type "archives") @run)}]]]))

(defn board-view-runner [player-side identity deck deck-count hand hand-count discard rig run]
  (let [is-me (= player-side :runner)
        hand-count-number (if (nil? @hand-count) (count @hand) @hand-count)
        centrals [:div.runner-centrals
                  [discard-view-runner player-side discard]
                  [deck-view :runner player-side identity deck deck-count]
                  [identity-view :runner identity hand-count-number]]
        runner-f (if (and (not is-me)
                          (= "irl" (get-in @app-state [:options :runner-board-order])))
                   reverse
                   seq)]
    [:div.runner-board {:class [(if is-me "me" "opponent")
                                (when (get-in @app-state [:options :sides-overlap]) "overlap")]}
     (when-not is-me centrals)
     (doall
       (for [zone (runner-f [:program :hardware :resource :facedown])]
         ^{:key zone}
         [:div
          (if (get-in @app-state [:options :stacked-cards] false)
            ; stacked mode
            (let [cards (get @rig zone)
                  distinct-cards (vals (group-by :title cards))]
              (show-distinct-cards distinct-cards))
            ; not in stacked mode
            (doall (for [c (get @rig zone)]
                     ^{:key (:cid c)}
                     [:div.card-wrapper {:class (when (playable? c) "playable")}
                      [card-view c]])))]))
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
            " (" (capitalize (tr-side winner)) ") "
            (cond
              (= "Decked" (capitalize reason))
              (tr [:game.win-decked] turn)

              (= "Flatline" (capitalize reason))
              (tr [:game.win-flatlined] turn)

              (= "Concede" (capitalize reason))
              (tr [:game.win-conceded] turn)

              :else
              (tr [:game.win-points] turn))]
           [:div (tr [:game.time-taken] time)]
           [:br]
           [build-game-stats (get-in @game-state [:stats :corp]) (get-in @game-state [:stats :runner])]
           [:button.win-right {:on-click #(reset! win-shown true) :type "button"} "✘"]])))))

(defn build-start-box
  "Builds the start-of-game pop up box"
  [my-ident my-user my-hand my-prompt my-keep op-ident op-user op-keep me-quote op-quote my-side]
  (let [visible-quote (r/atom true)
        mulliganed (r/atom false)
        start-shown (r/cursor app-state [:start-shown])
        card-back (get-in @app-state [:options :card-back])]
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
                               [:img.start-card {:src (str "/img/" card-back "-" (lower-case (:side @my-ident)) ".png")}]]
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
                [cond-button (if (= :spectator @my-side)
                               (tr [:game.close "Close"]) (tr [:game.start "Start Game"]))
                 true #(swap! app-state assoc :start-shown true)]
                (list ^{:key "keepbtn"} [cond-button (tr [:game.keep "Keep"])
                                         (= "mulligan" (-> @my-prompt first :prompt-type))
                                         #(send-command "choice" {:choice {:uuid (->> (-> @my-prompt first :choices)
                                                                                      (filter (fn [c] (= "Keep" (:value c))))
                                                                                      first
                                                                                      :uuid)}})]
                      ^{:key "mullbtn"} [cond-button (tr [:game.mulligan "Mulligan"])
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

(def phase->title
  {"initiation" (tr [:game.initiation "Initiation"])
   "approach-ice" (tr [:game.approach-ice "Approach ice"])
   "encounter-ice" (tr [:game.encouter-ice "Encounter ice"])
   "pass-ice" (tr [:game.pass-ice "Pass ice"])
   "approach-server" (tr [:game.approach-server "Approach server"])
   "corp-phase-43" (tr [:game.corp-phase-43 "Corp phase 4.3"])
   "access-server" (tr [:game.access-server "Access server"])})

(defn phase->next-phase-title
  [run]
  (case (:phase @run)
    "initiation" (tr [:game.approach-ice "Approach ice"])
    "approach-ice" (if (rezzed? (get-current-ice))
                     (tr [:game.encouter-ice "Encounter ice"])
                     (if (> (:position @run) 1)
                       (tr [:game.approach-ice "Approach ice"])
                       (tr [:game.approach-server "Approach server"])))
    "encounter-ice" (tr [:game.pass-ice "Pass ice"])
    "pass-ice" (if (zero? (:position @run))
                 (tr [:game.approach-server "Approach server"])
                 (tr [:game.approach-ice "Approach ice"]))
    "approach-server" (tr [:game.access-server "Access server"])
    "corp-phase-43" (tr [:game.access-server "Access server"])
    "access-server" (tr [:game.end-of-run "End of run"])
    ;; Error
    (tr [:game.no-current-run "No current run"])))

(defn corp-run-div
  [run]
  [:div.panel.blue-shade
   [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get phase->title (:phase @run) (tr [:game.unknown-phase "Unknown phase"]))]
   (cond
     (= "approach-ice" (:phase @run))
     (let [current-ice (get-current-ice)]
       [cond-button
        (str (tr [:game.rez "Rez"]) " " (:title current-ice))
        (not (rezzed? current-ice))
        #(send-command "rez" {:card current-ice :press-continue true})])

     (= "encounter-ice" (:phase @run))
     (let [current-ice (get-current-ice)]
       [cond-button
        (tr [:game.fire-unbroken "Fire unbroken subs"])
        (and (seq (remove :fired (:subroutines current-ice)))
             (not (every? :broken (:subroutines current-ice))))
        #(send-command "unbroken-subroutines" {:card current-ice})])

     (= "approach-server" (:phase @run))
     [checkbox-button
      (tr [:game.action-access "Action before access"])
      (tr [:game.action-access "Action before access"])
      (:corp-phase-43 @run)
      #(send-command "corp-phase-43")])

   [cond-button
    (let [next-phase (:next-phase @run)]
      (if (or next-phase (zero? (:position @run)))
        (tr [:game.no-further "No further actions"])
        (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))))
    (and (not= "initiation" (:phase @run))
         (not= "pass-ice" (:phase @run))
         (not= "access-server" (:phase @run))
         (not= "corp" (:no-action @run)))
    #(send-command "continue")]

   (when (and (not= "approach-server" (:phase @run))
              (not= "corp-phase-43" (:phase @run))
              (not= "access-server" (:phase @run)))
     [checkbox-button
      (tr [:game.stop-auto-pass "Stop auto-passing priority"])
      (tr [:game.auto-pass "Auto-pass priority"])
      (:corp-auto-no-action @run)
      #(send-command "toggle-auto-no-action")])])

(defn runner-run-div
  [run]
  (let [phase (:phase @run)
        next-phase (:next-phase @run)]
    [:div.panel.blue-shade
     [:h4 (tr [:game.current-phase "Current phase"]) ":" [:br] (get phase->title phase)]
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
        (str (tr [:game.continue-to "Continue to"]) " " (phase->next-phase-title run))
        (not= "runner" (:no-action @run))
        #(send-command "continue")]

       (zero? (:position @run))
       [cond-button (tr [:game.access-server "Access server"])
        (not= "runner" (:no-action @run))
        #(send-command "continue")])

     (when (= "encounter-ice" (:phase @run))
       (let [current-ice (get-current-ice)
             title (:title current-ice)]
         [cond-button
          (tr [:game.let-subs-fire "Let unbroken subroutines fire"])
          (and (seq (:subroutines current-ice))
               (not (every? #(or (:broken %) (false? (:resolve %))) (:subroutines current-ice))))
          #(send-command "system-msg"
                         {:msg (str "indicates to fire all unbroken subroutines on " title)})]))

     (when (or (= "approach-server" (:phase @run))
               (= "approach-ice" (:phase @run)))
       [cond-button
        (if (:jack-out @run) (tr [:game.jack-out "Jack Out"]) (tr [:game.undo-click "Undo click"]))
        (not (:cannot-jack-out @run))
        (if (:jack-out @run)
          #(send-command "jack-out")
          #(send-msg (r/atom {:msg "/undo-click"})))])

     (when (= "encounter-ice" (:phase @run))
       [cond-button
        (tr [:game.pass-continue "Pass ice and continue"])
        (or (not= "runner" (:no-action @run))
            (:jack-out-after-pass @run))
        #(send-command "continue" {:jack-out false})])

     (when (= "encounter-ice" (:phase @run))
       [cond-button
        (tr [:game.pass-jack "Pass ice and jack out"])
        (and (not (:cannot-jack-out @run))
             (or (not= "runner" (:no-action @run))
                 (not (:jack-out-after-pass @run))))
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
         [:div.info (tr [:side.runner "Runner"]) ": " (:link prompt) [:span {:class "anr-icon link"}]
          " + " (:runner-credits prompt) [:span {:class "anr-icon credit"}]]
         ;; Trace in which the runner pays first, showing base trace strength and corp credits
         [:div.info (tr [:game.trace "Trace"]) ": " (if (:bonus prompt) (+ base (:bonus prompt)) base)
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
          (let [strength (if (:bonus prompt) (+ base (:bonus prompt)) base)]
            [:span (str strength " + ")])
          [:span (:link prompt) " " [:span {:class "anr-icon link"}] (str " + " )])
        (if (= "corp" (:player prompt))
          [:span (:link prompt) " " [:span {:class "anr-icon link"}] (str " + " )]
          (let [strength (if (:bonus prompt) (+ base (:bonus prompt)) base)]
            [:span (str strength " + ")]))))
    [:select#credit
     (doall (for [i (range (inc (:choices prompt)))]
              [:option {:value i :key i} i]))] (str " " (tr [:game.credits "credits"]))]
   [:button {:on-click #(send-command "choice"
                                      {:choice (-> "#credit" js/$ .val str->int)})}
    (tr [:game.ok "OK"])]])

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
               (when (not= "Basic Action" (:type card))
                 [:<>
                  (let [get-nested-host (fn [card] (if (:host card)
                                                     (recur (:host card))
                                                     card))
                        get-zone (fn [card] (:zone (get-nested-host card)))
                        in-play-area? (fn [card] (= (get-zone card) ["play-area"]))
                        in-scored? (fn [card] (= (get-zone card) ["scored"]))
                        installed? (fn [card] (or (:installed card)
                                                  (= "servers" (first (get-zone card)))))]
                    (if (or (nil? (:side card))
                            (installed? card)
                            (in-scored? card)
                            (in-play-area? card))
                      [:div {:style {:text-align "center"}
                             :on-mouse-over #(card-highlight-mouse-over % card button-channel)
                             :on-mouse-out #(card-highlight-mouse-out % card button-channel)}
                       (tr [:game.card "Card"]) ": " (render-message (:title card))]
                      [:div.prompt-card-preview [card-view card false]]))
                  [:hr]]))
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
                   (tr [:game.ok "OK"])]])
               ;; trace prompts require their own logic
               (= (:prompt-type prompt) "trace")
               [trace-div prompt]

               ;; choice of number of credits
               (= (:choices prompt) "credit")
               [:div
                [:div.credit-select
                 [:select#credit
                  (doall (for [i (range (inc (:credit @me)))]
                           [:option {:value i :key i} i]))] (str " " (tr [:game.credits "credits"]))]
                [:button {:on-click #(send-command "choice"
                                                   {:choice (-> "#credit" js/$ .val str->int)})}
                 (tr [:game.ok "OK"])]]

               ;; auto-complete text box
               (:card-title (:choices prompt))
               [:div
                [:div.credit-select
                 [:input#card-title {:placeholder "Enter a card title"
                                     :onKeyUp #(when (= 13 (.-keyCode %))
                                                 (-> "#card-submit" js/$ .click)
                                                 (.stopPropagation %))}]]
                [:button#card-submit {:on-click #(send-command "choice" {:choice (-> "#card-title" js/$ .val)})}
                 (tr [:game.ok "OK"])]]

               ;; choice of specified counters on card
               (:counter (:choices prompt))
               (let [counter-type (keyword (:counter (:choices prompt)))
                     num-counters (get-in prompt [:card :counter counter-type] 0)]
                 [:div
                  [:div.credit-select
                   [:select#credit
                    (doall (for [i (range (inc num-counters))]
                             [:option {:key i :value i} i]))] (str " " (tr [:game.credits "credits"]))]
                  [:button {:on-click #(send-command "choice"
                                                     {:choice (-> "#credit" js/$ .val str->int)})}
                   (tr [:game.ok "OK"])]])

               ;; otherwise choice of all present choices
               :else
               (doall (for [{:keys [idx uuid value]} (:choices prompt)]
                        (when (not= value "Hide")
                          [:button {:key idx
                                    :on-click #(send-command "choice" {:choice {:uuid uuid}})
                                    :on-mouse-over
                                    #(card-highlight-mouse-over % value button-channel)
                                    :on-mouse-out
                                    #(card-highlight-mouse-out % value button-channel)
                                    :id (:title value)}
                           (render-message (or (not-empty (:title value)) value))]))))]
            (if @run
              [run-div side run]
              [:div.panel.blue-shade
               (if (= (keyword @active-player) side)
                 (when (and (not (or @runner-phase-12 @corp-phase-12))
                            (zero? (:click @me))
                            (not @end-turn))
                   [:button {:on-click #(send-command "end-turn")} (tr [:game.end-turn "End Turn"])])
                 (when @end-turn
                   [:button {:on-click #(send-command "start-turn")} (tr [:game.start-turn "Start Turn"])]))
               (when (and (= (keyword @active-player) side)
                          (or @runner-phase-12 @corp-phase-12))
                 [:button {:on-click #(send-command "end-phase-12")}
                  (if (= side :corp) (tr [:game.mandatory-draw "Mandatory Draw"]) (tr [:game.take-clicks "Take Clicks"]))])
               (when (= side :runner)
                 [:div
                  [cond-button (tr [:game.remove-tag "Remove Tag"])
                   (and (not (or @runner-phase-12 @corp-phase-12))
                        (playable? (get-in @me [:basic-action-card :abilities 5]))
                        (pos? (get-in @me [:tag :base])))
                   #(send-command "remove-tag")]
                  [:div.run-button
                   [cond-button (tr [:game.run "Run"])
                    (and (not (or @runner-phase-12 @corp-phase-12))
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
                 [cond-button (tr [:game.purge "Purge"])
                  (and (not (or @runner-phase-12 @corp-phase-12))
                       (playable? (get-in @me [:basic-action-card :abilities 6])))
                  #(send-command "purge")])
               (when (= side :corp)
                 [cond-button (tr [:game.trash-resource "Trash Resource"])
                  (and (not (or @runner-phase-12 @corp-phase-12))
                       (playable? (get-in @me [:basic-action-card :abilities 5]))
                       (is-tagged? game-state))
                  #(send-command "trash-resource")])
               [cond-button (tr [:game.draw "Draw"])
                (and (not (or @runner-phase-12 @corp-phase-12))
                     (playable? (get-in @me [:basic-action-card :abilities 1]))
                     (pos? (:deck-count @me)))
                #(send-command "draw")]
               [cond-button (tr [:game.gain-credit "Gain Credit"])
                (and (not (or @runner-phase-12 @corp-phase-12))
                     (playable? (get-in @me [:basic-action-card :abilities 0])))
                #(send-command "credit")]]))])})))

(defn- time-until
  "Helper method for timer. Computes how much time is left until `end`"
  [end]
  (let [now (js/moment)
        minutes (abs (. end (diff now "minutes")))
        positive (pos? (. end (diff now "seconds")))
        seconds (mod (abs (. end (diff now "seconds"))) 60)]
    {:minutes minutes :seconds seconds :pos positive}))

(defn- warning-class
  "Styling for the timer display."
  [{:keys [minutes pos]}]
  (if pos
    (condp >= minutes
      2 "red"
      5 "yellow"
      nil)
    "danger"))

(defn time-remaining
  "Component which displays a readout of the time remaining on the timer."
  [start-date timer hidden]
  (let [end-date (-> start-date js/moment (.add timer "minutes"))
        remaining (r/atom nil)
        interval (r/atom nil)]
    (r/create-class
      {:component-did-mount #(reset! interval
                               (js/setInterval ;; Update timer at most every 1 sec
                                 (fn [] (reset! remaining (time-until end-date)))
                                 1000))
       :component-will-unmount #(js/clearInterval interval)
       :reagent-render
       (fn []
         (when (and @remaining (not @hidden))
           [:span.float-center.timer
            {:class (warning-class @remaining)}
            (str
              (when-not (:pos @remaining) "-")
              (:minutes @remaining) "m:"
              (:seconds @remaining) "s remaining")]))})))

(defn starting-timestamp [start-date timer]
  (let [d (js/Date. start-date)
        hide-remaining (r/atom false)]
    (fn [] [:div.panel.blue-shade.timestamp
            [:span.float-center
             (str (tr [:game.game-start "Game start"]) ": " (.toLocaleTimeString d))]
            (when timer [:span.pm {:on-click #(swap! hide-remaining not)} (if @hide-remaining "+" "-")])
            (when timer [:span {:on-click #(swap! hide-remaining not)} [time-remaining start-date timer hide-remaining]])])))

(defn gameboard []
  (let [active (r/cursor app-state [:active-page])
        start-date (r/cursor game-state [:start-date])
        timer (r/cursor game-state [:options :timer])
        run (r/cursor game-state [:run])
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

    (add-watch active :change-page
      (fn [_k _r [o] [n]]
        (when (and (not= o "/play") (= n "/play"))
          (reset! should-scroll {:update true :send-msg false}))))

    (r/create-class
      {:display-name "gameboard"

       :reagent-render
       (fn []
         (when (= "/play" (first @active))
           (when @@render-board?
             (let [me-side (if (= :spectator @side) :corp @side)
                   op-side (utils/other-side me-side)
                   me (r/cursor game-state [me-side])
                   opponent (r/cursor game-state [op-side])
                   ;; hands
                   me-hand (r/cursor game-state [me-side :hand])
                   me-hand-count (r/cursor game-state [me-side :hand-count])
                   op-hand (r/cursor game-state [op-side :hand])
                   op-hand-count (r/cursor game-state [op-side :hand-count])
                   me-hand-size (r/cursor game-state [me-side :hand-size])
                   op-hand-size (r/cursor game-state [op-side :hand-size])
                   ;; decks
                   me-deck (r/cursor game-state [me-side :deck])
                   me-deck-count (r/cursor game-state [me-side :deck-count])
                   op-deck (r/cursor game-state [op-side :deck])
                   op-deck-count (r/cursor game-state [op-side :deck-count])
                   ;; discards
                   me-discard (r/cursor game-state [me-side :discard])
                   op-discard (r/cursor game-state [op-side :discard])
                   ;; user settings
                   me-user (r/cursor game-state [me-side :user])
                   op-user (r/cursor game-state [op-side :user])
                   ;; prompts
                   me-prompt (r/cursor game-state [me-side :prompt])
                   op-prompt (r/cursor game-state [op-side :prompt])
                   ;; identity cards
                   me-ident (r/cursor game-state [me-side :identity])
                   op-ident (r/cursor game-state [op-side :identity])
                   ;; score areas
                   me-scored (r/cursor game-state [me-side :scored])
                   op-scored (r/cursor game-state [op-side :scored])
                   me-agenda-point (r/cursor game-state [me-side :agenda-point])
                   op-agenda-point (r/cursor game-state [op-side :agenda-point])
                   ;; servers
                   corp-servers (r/cursor game-state [:corp :servers])
                   corp-remotes (r/track (fn [] (get-remotes (get-in @game-state [:corp :servers]))))
                   runner-rig (r/cursor game-state [:runner :rig])
                   sfx (r/cursor game-state [:sfx])]
               [:div.gameview
                [:div.gameboard

                 (let [me-keep (r/cursor game-state [me-side :keep])
                       op-keep (r/cursor game-state [op-side :keep])
                       me-quote (r/cursor game-state [me-side :quote])
                       op-quote (r/cursor game-state [op-side :quote])]
                   [build-start-box me-ident me-user me-hand me-prompt me-keep op-ident op-user op-keep me-quote op-quote side])

                 [build-win-box game-state]

                 [:div {:class (if (:replay @game-state)
                                 (case @replay-side
                                   :runner (get-in @game-state [:runner :user :options :background] "lobby-bg")
                                   :corp (get-in @game-state [:corp :user :options :background] "lobby-bg")
                                   :spectator @background)
                                 @background)}]

                 [:div.right-pane
                  [card-zoom-view zoom-card]

                  (if (:replay @game-state)
                    [content-pane :log :settings :notes :notes-shared]
                    [content-pane :log :settings])]

                 [:div.centralpane
                  (if (= op-side :corp)
                    [board-view-corp me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard corp-servers run]
                    [board-view-runner me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard runner-rig run])
                  (if (= me-side :corp)
                    [board-view-corp me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard corp-servers run]
                    [board-view-runner me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard runner-rig run])]

                 [:div.leftpane [:div.opponent
                                 (let [srv (if (= :corp op-side) "HQ" "Grip")
                                       translated-srv (if (= :corp op-side) (tr [:game.hq "HQ"]) (tr [:game.grip "Grip"]))]
                                   [hand-view srv translated-srv op-side op-hand op-hand-size op-hand-count op-prompt (= @side :spectator) "opponent"])]

                  [:div.inner-leftpane
                   [audio-component {:sfx sfx}]

                   [:div.left-inner-leftpane
                    [:div
                     [stats-view opponent]
                     [scored-view op-scored op-agenda-point false]]
                    [:div
                     [scored-view me-scored me-agenda-point true]
                     [stats-view me]]]

                   [:div.right-inner-leftpane
                    (let [op-rfg (r/cursor game-state [op-side :rfg])
                          op-current (r/cursor game-state [op-side :current])
                          op-play-area (r/cursor game-state [op-side :play-area])
                          me-rfg (r/cursor game-state [me-side :rfg])
                          me-current (r/cursor game-state [me-side :current])
                          me-play-area (r/cursor game-state [me-side :play-area])]
                      [:div
                       (when-not (:replay @game-state) [starting-timestamp @start-date @timer])
                       [rfg-view op-rfg (tr [:game.rfg "Removed from the game"]) true]
                       [rfg-view me-rfg (tr [:game.rfg "Removed from the game"]) true]
                       [play-area-view op-user (tr [:game.play-area "Play Area"]) op-play-area]
                       [play-area-view me-user (tr [:game.play-area "Play Area"]) me-play-area]
                       [rfg-view op-current (tr [:game.current "Current"]) false]
                       [rfg-view me-current (tr [:game.current "Current"]) false]])
                    (when-not (= @side :spectator)
                      [button-pane {:side me-side :active-player active-player :run run :end-turn end-turn
                                    :runner-phase-12 runner-phase-12 :corp-phase-12 corp-phase-12
                                    :corp corp :runner runner :me me :opponent opponent}])]]

                  [:div.me
                   (let [srv (if (= :corp me-side) "HQ" "Grip")
                         translated-srv (if (= :corp me-side) (tr [:game.hq "HQ"]) (tr [:game.grip "Grip"]))]
                     [hand-view srv translated-srv me-side me-hand me-hand-size me-hand-count me-prompt true "me"])]]]
                (when (:replay @game-state)
                  [:div.bottompane
                   [replay-panel]])]))))})))
