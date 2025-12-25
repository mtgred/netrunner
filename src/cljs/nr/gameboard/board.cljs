(ns nr.gameboard.board
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljc.java-time.duration :as duration]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [cljs.core.async :refer [<! chan put!] :as async]
   [clojure.string :as s :refer [capitalize ends-with? join lower-case split
                           starts-with?]]
   [game.core.card :refer [active? asset? corp? facedown? faceup?
                           get-counters get-title has-subtype? ice? program? rezzed?
                           same-card? operation? condition-counter?]]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :refer [add-cost-to-label is-tagged? select-non-nil-keys
                          str->int] :as utils]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.cardbrowser :refer [card-as-text]]
   [nr.end-of-game-stats :refer [build-game-stats]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.card-preview :refer [card-highlight-mouse-out
                                      card-highlight-mouse-over card-preview-mouse-out
                                      card-preview-mouse-over put-game-card-in-channel zoom-channel]]
   [nr.gameboard.player-stats :refer [stat-controls stats-view]]
   [nr.gameboard.replay :refer [replay-panel]]
   [nr.gameboard.right-pane :refer [content-pane]]
   [nr.gameboard.state :refer [game-state not-spectator? replay-side]]
   [nr.sounds :refer [update-audio]]
   [nr.translations :refer [tr tr-data tr-game-prompt tr-side tr-element tr-span]]
   [nr.utils :refer [banned-span checkbox-button cond-button get-image-path
                     image-or-face map-longest render-icons render-message]]
   [nr.ws :as ws]
   [jinteki.card-backs :as card-backs]
   [reagent.core :as r]))

(declare stacked-card-view show-distinct-cards)

(defonce board-dom (atom {}))
(defonce card-menu (r/atom {}))

(defonce corp-prompt-state (r/cursor game-state [:corp :prompt-state]))
(defonce runner-prompt-state (r/cursor game-state [:runner :prompt-state]))

(defn is-replay? [] (= "local-replay" (:gameid @app-state [:gameid])))

(defn- prompt-eid [side] (get-in @game-state [side :prompt-state :eid]))

(defn- any-prompt-open?
  [side]
  (if (= side :corp)
    @corp-prompt-state
    @runner-prompt-state))

(defn- image-url
  ([card] (image-url card nil))
  ([{:keys [side code] :as card} {:keys [zoom?] :as opts}]
   (let [lang (get-in @app-state [:options :card-language] "en")
         res (get-in @app-state [:options :card-resolution] "default")
         special-user (get-in @game-state [(keyword (lower-case side)) :user :special])
         special-wants-art (get-in @game-state [(keyword (lower-case side)) :user :options :show-alt-art])
         viewer-wants-art (and (get-in @app-state [:options :show-alt-art])
                               (not (and zoom? (get-in @app-state [:options :pin-base-art]))))
         show-art (and special-user special-wants-art viewer-wants-art)
         art (if show-art
               (get-in @game-state [(keyword (lower-case side)) :user :options :alt-arts (keyword code)] "stock")
               "stock")
         card (if (or (:face card) (:images card)) card (get @all-cards (get-title card)))
         images (image-or-face card)]
     (if (sequential? art)
       (let [art-urls (get-image-path images (keyword lang) (keyword res) (keyword (first art)))
             chosen-art (nth art-urls (second art))]
         [chosen-art])
       (first (get-image-path images (keyword lang) (keyword res) (keyword art)))))))


(defonce button-channel (chan))

(defn open-card-menu
  ([source] (open-card-menu source nil))
  ([source ghost]
   (swap! card-menu assoc :source source :ghost ghost)))

(defn close-card-menu []
  (swap! card-menu dissoc :source :keep-menu-open))

(defn action-list
  [{:keys [type zone rezzed advanceable
           advancementcost current-advancement-requirement] :as card}]
  (r/with-let [active-player (r/cursor game-state [:active-player])
               side (r/cursor game-state [:side])]
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
      (and (= type "Agenda")
           (#{"servers" "onhost"} (first zone))
           (= (keyword @active-player) @side)
           (>= (get-counters card :advancement)
               (or current-advancement-requirement advancementcost)))
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
      (cons "derez"))))

(def click-card-keys
  [:cid :side :host :type :zone :ghost :flashback-fake-in-hand])

(defn card-for-click [card]
  (select-non-nil-keys
    (if (:host card) (update card :host card-for-click) card)
    click-card-keys))

(defn playable?
  "Checks whether a card or ability is playable"
  [action]
  (:playable action))

(defn handle-abilities
  [side {:keys [abilities corp-abilities runner-abilities subroutines facedown] :as card}]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))
        card-side (keyword (lower-case (:side card)))]
    (swap! card-menu dissoc :keep-menu-open)
    (when-not (and (= card-side :runner) facedown)
      (cond

        ;; Toggle abilities panel
        (or (< 1 c)
            (pos? (+ (count corp-abilities)
                     (count runner-abilities)
                     (count subroutines)))
            (some #{"rez" "derez" "advance" "trash"} actions)
            (and (corp? card)
                 (not (faceup? card))))
        (do (when (= side card-side)
              (if (and (= (:cid card) (:source @card-menu))
                       (= (:ghost card) (:ghost @card-menu)))
                (close-card-menu)
                (open-card-menu (:cid card) (:ghost card))))
            (when (and (= :runner card-side)
                       (= :corp side)
                       corp-abilities)
              (if (= (:cid card) (:source @card-menu))
                (close-card-menu)
                (open-card-menu (:cid card) (:ghost card))))
            (when (and (= :corp card-side)
                       (= :runner side)
                       (or subroutines runner-abilities))
              (if (= (:cid card) (:source @card-menu))
                (close-card-menu)
                (open-card-menu (:cid card) (:ghost card)))))

        ;; Trigger first (and only) ability / action
        (and (= c 1)
             (= side card-side))
        (if (= (count abilities) 1)
          (when (playable? (first abilities))
            (send-command "ability" {:card (card-for-click card) :ability 0}))
          (send-command (first actions) {:card (card-for-click card)}))))))

(defn- graveyard-highlight-card?
  [card]
  (and
    (= (first (:zone card)) "discard")
    (or (= "Agenda" (:type card))
        (:poison card)
        (:highlight-in-discard card))))

(defn- prompt-button-from-card?
  [clicked-card {:keys [card msg prompt-type choices] :as prompt-state}]
  (when-not (or (some #{:counter :card-title :number} choices)
                (= choices "credit")
                (= prompt-type "trace"))
    (some (fn [{:keys [_ uuid value]}]
            (when (= (:cid value) (:cid clicked-card)) uuid))
          choices)))

(defn send-play-command [{:keys [type zone] :as card} shift-key-held]
  (if (and (= "discard" (first zone)) (:flashback-fake-in-hand card))
    (send-command "flashback" {:card (card-for-click card) :shift-key-held shift-key-held})
    (send-command "play" {:card (card-for-click card) :shift-key-held shift-key-held})))

(defn handle-card-click [{:keys [type zone] :as card} shift-key-held]
  (let [side (:side @game-state)]
    (when (not-spectator?)
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt-state :prompt-type]) "select")
        (send-command "select" {:card (card-for-click card) :eid (prompt-eid side) :shift-key-held shift-key-held})

        ;; A selectable card is clicked outside of a select prompt (ie it's a button on a choices prompt)
        (contains? (into #{} (get-in @game-state [side :prompt-state :selectable])) (:cid card))
        (send-command "choice" {:eid (prompt-eid side) :choice {:uuid (prompt-button-from-card? card (get-in @game-state [side :prompt-state]))}})

        ;; Card is an identity of player's side
        (and (= (:type card) "Identity")
             (= side (keyword (lower-case (:side card)))))
        (handle-abilities side card)

        ;; Runner clicking on a runner card
        (and (= side :runner)
             (= "Runner" (:side card))
             (not (any-prompt-open? side))
             (or (and (= "hand" (first zone))
                      (playable? card))
                 (:playable-as-if-in-hand card)
                 (and (= "discard" (first zone))
                      (:flashback-playable card))))
        (send-play-command (card-for-click card) shift-key-held)

        ;; Corp clicking on a corp card
        (and (= side :corp)
             (= "Corp" (:side card))
             (not (any-prompt-open? side))
             (or (and (= "hand" (first zone))
                      (playable? card))
                 (and (= "discard" (first zone))
                      (= "Operation" type)
                      (:flashback-fake-in-hand card)
                      (:flashback-playable card))))
        (if (= "Operation" type)
          (send-play-command (card-for-click card) shift-key-held)
          (if (= (:cid card) (:source @card-menu))
            (do (send-command "generate-install-list" nil)
                (close-card-menu))
            (do (send-command "generate-install-list" {:card (card-for-click card)
                                                       :shift-key-held shift-key-held})
                (open-card-menu (:cid card)))))

        :else
        (case (first zone)
          ("current" "onhost" "play-area" "scored" "servers" "rig")
          (handle-abilities side card)
          ; else
          nil)))))

(defn spectate-side []
  (let [corp-specs (get-in @app-state [:current-game :corp-spectators])
        runner-specs (get-in @app-state [:current-game :runner-spectators])
        me (:user @app-state)]
    (cond
      (some #(= (-> % :user :username) (:username me)) corp-specs)
      :corp
      (some #(= (-> % :user :username) (:username me)) runner-specs)
      :runner
      :else
      nil)))

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
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))]
    (when (not= "Identity" (:type card))
      (send-command "move" {:card card :server server}))))

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last str->int))

(defn remote->name [server]
  (let [num (remote->num server)]
    [tr-span [:game_server "Server"] {:num num}]))

(defn remote->str-name [server]
  (let [num (remote->num server)]
   (tr [:game_server "Server"] {:num num})))

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

(defn facedown-card-url
  [side]
  (let [my-side (:side @game-state)
        side-key (keyword (lower-case side))
        display-options (or (get-in @app-state [:options :card-back-display]) "them")
        card-back (if (= side-key my-side)
                    (get-in @game-state [my-side :user :options (if (= side-key :corp) :corp-card-sleeve :runner-card-sleeve)] "nsg-card-back")
                    (case display-options
                      "them" (get-in @game-state [side-key :user :options (if (= side-key :corp) :corp-card-sleeve :runner-card-sleeve)] "nsg-card-back")
                      "me"   (get-in @game-state [my-side :user :options (if (= side-key :corp) :corp-card-sleeve :runner-card-sleeve)] "nsg-card-back")
                      "ffg" "ffg-card-back"
                      "nsg" "nsg-card-back"
                      "nsg-card-back"))
        card-back (if (= card-back "") "nsg-card-back" card-back)
        maybe-image? (or (get-in card-backs/card-backs [(keyword card-back) :file]) "nsg")
        s (lower-case side)]
    (str "/img/card-backs/" s "/" maybe-image?  ".png")))

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
     [tag {:src (facedown-card-url side)
           :alt alt}])))

(defn sort-archives
  [cards] (->> cards (sort-by get-title) (sort-by #(not (faceup? %)))))

(defn sort-heap
  [cards] (sort-by get-title cards))

(defn sort-archives?
  [] (get-in @app-state [:options :archives-sorted] false))

(defn sort-heap?
  [] (get-in @app-state [:options :heap-sorted] false))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [code] :as card}]
  (when code
    [:div.card-frame
     [:div.blue-shade.card {:on-mouse-enter #(put-game-card-in-channel card zoom-channel)
                            :on-mouse-leave #(put! zoom-channel false)}
      (when-let [url (image-url card)]
        [:div
         [:span.cardname (get-title card)]
         [:img.card.bg {:src url :alt (get-title card) :onError #(-> % .-target js/$ .hide)}]])]]))

(defn card-implementation [zoom-card]
  (r/with-let [log-width (r/cursor app-state [:options :log-width])]
    (when-let [card @zoom-card]
      (let [implemented (:implementation card)]
        (case implemented
          (:full "full") nil
          [:div.panel.blue-shade.implementation {:style {:right @log-width}}
           (if implemented
             [:span.impl-msg implemented]
             [tr-element :span.unimplemented [:game_unimplemented "Unimplemented"]])])))))

(defn card-zoom-display
  [zoom-card img-side]
  (r/with-let [pin (r/cursor app-state [:options :pin-zoom])
               card-zoom-type (r/cursor app-state [:options :card-zoom])]
    (when-let [card @zoom-card]
      [:<>
       [:div.card-preview.blue-shade
        {:on-click #(reset! img-side (not @img-side))}
        (let [url (image-url card {:zoom? true})
              show-img (= "image" (or @card-zoom-type "image"))]
          (if (and url (if show-img @img-side (not @img-side)))
            [:img {:src url :alt (get-title card) :onLoad #(-> % .-target js/$ .show)}]
            [card-as-text card false]))]
       (when (or @pin false)
         [:button.win-right {:on-click #(reset! zoom-card false)
                             :type "button"}
          "✘"])])))

(defn card-zoom [zoom-card img-side]
  (if @zoom-card
    (do (-> ".card-zoom" js/$ (.addClass "fade"))
        [card-zoom-display zoom-card img-side])
    (do (-> ".card-zoom" js/$ (.removeClass "fade")) nil)))

(defn card-zoom-view [zoom-card]
  (r/with-let [zoomed-card (r/atom nil)
               img-side (r/atom true)
               pin (r/cursor app-state [:options :pin-zoom])]
    (let [pin (or @pin false)]
      (when (or @zoom-card
                (and (not @zoom-card) (not pin)))
        (reset! zoomed-card @zoom-card)
        (reset! img-side true))
      [:<>
       [:div.card-zoom
        [card-zoom zoomed-card img-side]]
       [card-implementation zoomed-card]])))

(defn- card-menu-item
  "Creates a li item for a card menu using the given id and key.
   Performs the function provided on click or pressing the Enter or Space keys"
  ([label func] (card-menu-item label func true))
  ([label func enabled]
   (if enabled
     [:li {:tab-index 0
           :on-click func
           :on-key-down #(when (= "Enter" (.-key %))
                           (func))
           :on-key-up #(when (= " " (.-key %))
                         (func))}
      label]
     [:li.disabled label])))

(defn server-menu
  "The pop-up on a card in hand when clicked"
  [card]
  (r/with-let [install-list (r/cursor game-state [:corp :install-list])]
    (let [active-menu? (= (:cid card) (:source @card-menu))]
      (when (and @install-list active-menu?)
        [:div.panel.blue-shade.servers-menu {:class "active-menu"
                                             :style {:display "inline"}}
         [:ul (doall
               (map-indexed
                (fn [_ label]
                  ^{:key label}
                  [card-menu-item (if (:cid label)
                                    (:title label)
                                    (tr-game-prompt label))
                   #(do (close-card-menu)
                        (if (= "Expend" label)
                          (send-command "expend" {:card card :server label})
                          (send-command "play" {:card card :server label})))])
                @install-list))]]))))

(defn list-abilities
  [ab-type card abilities]
  (doall
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
          ^{:key i}
          [card-menu-item (render-icons (add-cost-to-label ab))
           #(do (send-command command args)
                (if (:keep-menu-open ab)
                  (swap! card-menu assoc :keep-menu-open (keyword (:keep-menu-open ab)))
                  (close-card-menu)))
           (:playable ab)]))
      abilities)))

(defn check-keep-menu-open
  [card player keep-menu-open]
  (let [{:keys [credit click hand]} @player
        keep-menu-open (case @keep-menu-open
                        :while-credits-left
                        (pos? credit)

                        :while-clicks-left
                        (pos? click)

                        :while-2-clicks-left
                        (>= click 2)

                        :while-3-clicks-left
                        (>= click 3)

                        :while-4-clicks-left
                        (>= click 4)

                        :while-cards-in-hand
                        (not-empty hand)

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
                            (not (zero? click))) ; clicks left

                        :forever true

                         #_:else
                        false)]
    (when-not keep-menu-open (close-card-menu))
    keep-menu-open))

(defn runner-abs [card runner-abilities subroutines title]
  (when (= (:cid card) (:source @card-menu))
    [:div.panel.blue-shade.runner-abilities.active-menu {:style {:display "inline"}}
     [:button.win-right {:on-click #(close-card-menu) :type "button"} "✘"]
     (when (or (seq runner-abilities)
               (seq subroutines))
       [:span.float-center [tr-span [:game_abilities "Abilities"]] ":"])
     [:ul
      (list-abilities :runner card runner-abilities)
      (when (seq subroutines)
        [card-menu-item [tr-span [:game_let-subs-fire "Let unbroken subroutines fire"]]
         #(do (send-command "system-msg"
                            {:msg (str "indicates to fire all unbroken subroutines on " title)})
              (close-card-menu))])]
     (when (seq subroutines)
       [:span.float-center [tr-span [:game_subs "Subroutines"]] ":"])
     (doall
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
             (cond (:broken sub) [banned-span]
                   (:fired sub) "✅")]])
         subroutines))]))

(defn corp-abs [card corp-abilities]
  (when (= (:cid card) (:source @card-menu))
    [:div.panel.blue-shade.corp-abilities.active-menu {:style {:display "inline"}}
     [:button.win-right {:on-click #(close-card-menu) :type "button"} "✘"]
     (when (seq corp-abilities)
       [:span.float-center [tr-span [:game_abilities "Abilities"]] ":"])
     [:ul (list-abilities :corp card corp-abilities)]]))

(defn encounter-info-div
  "Displays encounter information including current ice strength and subroutines"
  [ice]
  (let [subtypes (sort-by #(case %
                             "Mythic" 1
                             ("Barrier" "Code Gate" "Sentry") 2
                             ("Bioroid" "Trap") 3
                             4) (:subtypes ice))
        current-strength (or (:current-strength ice)
                             (:strength ice)
                             0)
        subroutines (:subroutines ice)]
    [:div.panel.blue-shade.encounter-info {:style {:display "inline"}}
     [:span.active.float-center (get-title ice)]
     [:span.info {:style {:display "block"}} (join " - " subtypes)]
     [tr-element :span.float-center [:card-browser_strength "Strength"] {:strength current-strength}]
     [:hr]
     (when (seq subroutines)
       [:span.float-center [tr-span [:game_subs "Subroutines"]] ":"])
     (doall
       (map-indexed
         (fn [i sub]
           (let [fire-sub #(when (= :corp (:side @game-state))
                             (send-command "subroutine" {:card ice
                                                         :subroutine i})
                             (close-card-menu))]
             [:div {:key i
                    :tab-index 0
                    :on-click fire-sub
                    :on-key-down #(when (= "Enter" (.-key %))
                                    (fire-sub))
                    :on-key-up #(when (= " " (.-key %))
                                  (fire-sub))}
              [:span (cond (:broken sub)
                           {:class :disabled
                            :style {:font-style :italic}}
                           (false? (:resolve sub))
                           {:class :dont-resolve
                            :style {:text-decoration :line-through}})
               (render-icons (str " [Subroutine] " (:label sub)))]
              [:span.float-right
               (cond (:broken sub) [banned-span]
                     (:fired sub) "✅")]]))
         subroutines))]))

(defn card-abilities [card abilities subroutines]
  (r/with-let [gs-side (r/cursor game-state [:side])
               gs-player (r/cursor game-state [@gs-side])
               keep-menu-open (r/cursor card-menu [:keep-menu-open])]
    (let [actions (action-list card)]
      (when (and (= (:cid card) (:source @card-menu))
                 (= (:ghost card) (:ghost @card-menu))
                 (or (nil? (:keep-menu-open @card-menu))
                     (check-keep-menu-open card gs-player keep-menu-open))
                 (or (pos? (+ (count actions)
                              (count abilities)
                              (count subroutines)))
                     (some #{"derez" "rez" "advance" "trash"} actions)
                     (= type "ICE")))
        [:div.panel.blue-shade.abilities.active-menu {:style {:display "inline"}}
         [:button.win-right {:on-click #(close-card-menu) :type "button"} "✘"]
         (when (seq actions)
           [:span.float-center [tr-span [:game_actions "Actions"]] ":"])
         (when (seq actions)
           [:ul
            (doall
             (map-indexed
              (fn [_ action]
                (let [keep-menu-open (case action
                                       "derez" false
                                       "rez" :if-abilities-available
                                       "trash" false
                                       "advance" :for-agendas
                                       "score" false
                                       false)]
                  ^{:key action}
                  [card-menu-item (tr-game-prompt action)
                   #(do (send-command action {:card card})
                        (if keep-menu-open
                          (swap! card-menu assoc :keep-menu-open keep-menu-open)
                          (close-card-menu)))]))
              actions))])
         (when (or (seq abilities)
                   (and (active? card)
                        (seq (remove #(or (:fired %) (:broken %)) subroutines))))
           [:span.float-center [tr-span [:game_abilities "Abilities"]] ":"])
         [:ul
          (when (seq abilities)
            (list-abilities :ability card abilities))
          (when (and (active? card)
                     (seq (remove #(or (:fired %) (:broken %)) subroutines)))
            [card-menu-item [tr-span [:game_fire-unbroken "Fire unbroken subroutines"]]
             #(do (send-command "unbroken-subroutines" {:card card})
                  (close-card-menu))])]
         (when (seq subroutines)
           [:span.float-center [tr-span [:game_subs "Subroutines"]] ":"])
         (when (seq subroutines)
           [:ul
            (doall
             (map-indexed
              (fn [i sub]
                (let [fire-sub #(do (send-command "subroutine" {:card card
                                                                :subroutine i})
                                    (close-card-menu))]
                  [:li {:key i
                        :tab-index 0
                        :on-click fire-sub
                        :on-key-down #(when (= "Enter" (.-key %))
                                        (fire-sub))
                        :on-key-up #(when (= " " (.-key %))
                                      (fire-sub))}
                   [:span (cond (:broken sub)
                                {:class :disabled
                                 :style {:font-style :italic}}
                                (false? (:resolve sub))
                                {:class :dont-resolve
                                 :style {:text-decoration :line-through}})
                    (render-icons (str " [Subroutine] " (:label sub)))]
                   [:span.float-right
                    (cond (:broken sub) [banned-span]
                          (:fired sub) "✅")]]))
              subroutines))])]))))

(defn draw-facedown?
  "Returns true if the installed card should be drawn face down."
  [{:keys [host] :as card}]
  (or (facedown? card)
      (and (corp? card)
           (not (or (operation? card)
                    (condition-counter? card)
                    (faceup? card)
                    (= (:side host) "Runner"))))))

;; HERE

(defn card-view
  [{:keys [zone code type abilities counter
           subtypes strength current-strength selected hosted
           side facedown card-target icon new ghost runner-abilities subroutines seen
           subtype-target corp-abilities flashback-fake-in-hand flashback-playable]
    :as card} flipped disable-click]
  (let [title (get-title card)]
    (r/with-let [gs-prompt-state (r/cursor game-state [(keyword (lower-case side)) :prompt-state])
                 gs-encounter-ice (r/cursor game-state [:encounters :ice])
                 as-button (r/cursor app-state [:button])
                 gs-side (r/cursor game-state [:side])
                 as-stacked-cards (r/cursor app-state [:options :stacked-cards])]
      [:div.card-frame.menu-container
       [:div.blue-shade.card {:class (str (cond selected "selected"
                                                (contains? (into #{} (get-in @gs-prompt-state [:selectable])) (:cid card)) "selectable"
                                                (same-card? card @as-button) "hovered"
                                                (same-card? card @gs-encounter-ice) "encountered"
                                              (and (not (any-prompt-open? side)) (playable? card)) "playable"
                                              ghost "ghost"
                                              (and flashback-fake-in-hand flashback-playable seen) "playable flashback known"
                                              (and flashback-fake-in-hand flashback-playable) "playable flashback unknown"
                                              flashback-fake-in-hand "flashback"
                                              (graveyard-highlight-card? card) "graveyard-highlight"
                                              ;; specifically, don't show cards as 'new' during selection prompts, so they dont look like selectable cards (we're running out of colors)
                                            (and new (not (seq (get-in @gs-prompt-state [:selectable])))) "new"))
                            :tab-index (when (and (not disable-click)
                                                  (or (active? card)
                                                      (playable? card)))
                                         0)
                            :draggable (when (and (not-spectator?) (not disable-click) (not flashback-fake-in-hand)) true)
                            :on-drag-start #(handle-dragstart % card)
                            :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                            :on-mouse-enter #(when (or (not (or (not code) flipped facedown))
                                                       (spectator-view-hidden?)
                                                       (= @gs-side (keyword (lower-case side))))
                                               (put-game-card-in-channel card zoom-channel))
                            :on-mouse-leave #(put! zoom-channel false)
                            :on-click #(when (not disable-click)
                                         (handle-card-click card (.-shiftKey %)))
                            :on-key-down #(when (and (= "Enter" (.-key %))
                                                     (not disable-click))
                                            (handle-card-click card (.-shiftKey %)))
                            :on-key-up #(when (and (= " " (.-key %))
                                                   (not disable-click))
                                          (handle-card-click card (.-shiftKey %)))}
      (if (or (not code) flipped facedown)
        (let [facedown-but-known (or (not (or (not code) flipped facedown))
                                     (spectator-view-hidden?)
                                     (= @gs-side (keyword (lower-case side))))
              alt-str (when facedown-but-known (str "Facedown " title))]
          [facedown-card side ["bg"] alt-str])
        (when-let [url (image-url card)]
          [:div
           [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]]))
      [:span.cardname (tr-data :title (get @all-cards (:title card)))]
      [:div.counters
       (when counter
         (doall
          (map (fn [[type num-counters]]
                 (when (pos? num-counters)
                   (let [selector (str "div.darkbg." (lower-case (name type)) "-counter.counter")]
                     [(keyword selector) {:key type} num-counters])))
               (sort-by key counter))))
       (when (pos? (get-counters card :advancement))
         [:div.darkbg.advance-counter.counter {:key "adv"} (get-counters card :advancement)])]
      (when (and (or current-strength strength)
                 (or (ice? card)
                     (has-subtype? card "Icebreaker"))
                 (active? card))
        [:div.darkbg.strength (or current-strength strength)])
      (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
      (when card-target [:div.darkbg.card-target card-target])
      (when subtype-target [:div.darkbg.subtype-target subtype-target])
      (when (active? card)
        (let [server-card (get @all-cards title)]
          [:div.darkbg.additional-subtypes
           (join " - " (remove (into #{} (:subtypes server-card)) subtypes))]))]

     (cond
       (and (= zone ["hand"])
            (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
       [server-menu card]

       (and (= :runner @gs-side)
            (pos? (+ (count runner-abilities) (count subroutines))))
       [runner-abs card runner-abilities subroutines title]

       (and (= :corp @gs-side)
            (pos? (count corp-abilities)))
       [corp-abs card corp-abilities]

       (= (keyword (lower-case side)) @gs-side)
       [card-abilities card abilities subroutines])

     (when (pos? (count hosted))
       [:div.hosted
        (if (and (not (ice? card))
                 @as-stacked-cards)
        ; stacked mode
          (let [distinct-hosted (vals (group-by get-title hosted))]
            (show-distinct-cards distinct-hosted))

          (doall
           (for [card hosted]
             (let [flipped (draw-facedown? card)]
               ^{:key (:cid card)}
               [card-view card flipped]))))])])))

(defn show-distinct-cards
  [distinct-cards]
  (doall (apply concat (for [cards distinct-cards] ; apply concat for one-level flattening
                         (let [hosting (remove #(zero? (count (:hosted %))) cards) ; There are hosted cards on these
                               others (filter #(zero? (count (:hosted %))) cards)
                               facedowns (filter draw-facedown? others)
                               others (remove draw-facedown? others)]
                           [; Hosting
                            (for [c hosting]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c (draw-facedown? c)]])
                            ; Facedown
                            (for [c facedowns]
                              ^{:key (:cid c)} [:div.card-wrapper {:class (when (playable? c) "playable")}
                                                [card-view c true]])
                            ; Rest
                            (when (not-empty others)
                              (if (= 1 (count others))
                                (let [c (first others)
                                      flipped (draw-facedown? c)]
                                  ^{:key (:cid c)}
                                  [:div.card-wrapper {:class (when (playable? c) "playable")}
                                   [card-view c flipped]])
                                [stacked-card-view others]))])))))

(defn stacked-card-view
  [cards]
  [:div.stacked
   (doall
     (for [c cards]
       (let [flipped (draw-facedown? c)]
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
  (let [fn (or (:fn opts) count)
        classes (str (when (pos? (count cursor)) "darkbg ")
                     (:classes opts))]
    [:div.header {:class classes}
     (if (:tr-vec opts) [tr-span (:tr-vec opts) (:tr-params opts)] (:name opts))
     (when-not (:hide-cursor opts) (str " (" (fn cursor) ")"))]))

(defn- this-user?
  [user]
  (if (:replay @game-state)
    (= (get-in @game-state [@replay-side :user :_id]) (:_id user))
    (= (:_id user) (-> @app-state :user :_id))))

(defn build-hand-card-view
  [hand size wrapper-class]
  [:div
   (doall
     (map-indexed
       (fn [i card]
         [:div {:key (or (:cid card) i)
                :class (str wrapper-class)
                :style {:left (when (< 1 size) (* (/ 320 (dec size)) i))}}
          (cond
            (spectator-view-hidden?)
            [card-view (dissoc card :new :selected)]
            (:cid card)
            [card-view card]
            :else
            [facedown-card (:side card)])])
       hand))])

(defn hand-view []
  (let [s (r/atom {})
        ordering (r/atom :natural)]
    (fn [side hand hand-size hand-count popup popup-direction discard]
      (let [flashbacks (if discard (map #(assoc % :flashback-fake-in-hand true) (filter :flashback-playable @discard))
                           [])
            printed-size (if (nil? @hand-count) (count @hand) @hand-count)
            size (+ printed-size (count flashbacks))
            sorted-hand (case @ordering
                          :natural @hand
                          :title (sort-by :title @hand)
                          :type (sort-by (juxt :type :title) @hand))
            filled-hand (concat (map #(dissoc % :flashback) sorted-hand)
                                flashbacks
                                (repeat (- size (+ (count @hand) (count flashbacks))) {:side (if (= :corp side) "Corp" "Runner")}))]
        [:div.hand-container
         [:div.hand-controls
          [:div.panel.blue-shade.hand
           (drop-area (if (= :corp side) "HQ" "the Grip") {:class (when (> size 6) "squeeze")})
           [build-hand-card-view filled-hand size "card-wrapper"]
           [label filled-hand {:name (if (= :corp side)
                                       [tr-span [:game_hq "HQ"]]
                                       [tr-span [:game_grip "Grip"]])
                               :fn (fn [_] (str printed-size "/" (:total @hand-size)))}]]
          (when popup
            [:div.hand-controls
             [:div.panel.blue-shade.hand-expand
              {:on-click #(-> (:hand-popup @s) js/$ .fadeToggle)}
              "+"]
             [:div.panel.blue-shade.hand-sort
              {:on-click (fn [] (reset! ordering (case @ordering :natural :title :title :type :type :natural)))}
              (case @ordering
                :natural "#"
                :title "t"
                :type "y")]])]
         (when popup
           [:div.panel.blue-shade.popup {:ref #(swap! s assoc :hand-popup %) :class popup-direction}
            [:div
             [:a {:on-click #(close-popup % (:hand-popup @s) nil false false)} [tr-span [:game_close "Close"]]]
             [tr-element :label [:game_card-count] {:cnt size}]
             (let [{:keys [total]} @hand-size]
               (stat-controls :hand-size [tr-element :div.hand-size [:game_max-hand "Max hand size"] {:total total}]))
             [build-hand-card-view filled-hand size "card-popup-wrapper"]]])]))))

(defn show-deck [event ref]
  (-> ((keyword (str ref "-content")) @board-dom) js/$ .fadeIn)
  (-> ((keyword (str ref "-menu")) @board-dom) js/$ .fadeOut)
  (send-command "view-deck"))

(defn identity-view [render-side identity hand-count]
  (let [is-runner (= :runner render-side)
        hand-count-str (str " (" hand-count ")")
        title (if is-runner
                [:game_grip-count (str "Grip" hand-count-str)]
                [:game_hq-count (str "HQ" hand-count-str)])]
    [:div.blue-shade.identity
     [card-view @identity]
     [:div.header {:class "darkbg server-label"}
      ;; TODO - can the tr take in hand count?
      [tr-span title {:cnt hand-count}]]]))

(defn deck-view [render-side player-side identity deck deck-count]
  (fn [render-side player-side identity deck deck-count]
    (let [is-runner (= :runner render-side)
          deck-count-number (if (nil? @deck-count) (count @deck) @deck-count)
          tr-vec (if is-runner
                   [:game_stack-count (str "Stack (" deck-count-number ")")]
                   [:game_rnd-count (str "R&D (" deck-count-number ")")])
          title (tr (if is-runner [:game_stack "Stack"] [:game_rnd "R&D"]))
          ref (if is-runner "stack" "rd")
          menu-ref (keyword (str ref "-menu"))
          content-ref (keyword (str ref "-content"))]
      ;; deck-count is only sent to live games and does not exist in the replay
      [:div.deck-container (drop-area title {})
       [:div.blue-shade.deck {:on-click (when (and (= render-side player-side) (not-spectator?))
                                          #(let [popup-display (-> (content-ref @board-dom) .-style .-display)]
                                             (if (or (empty? popup-display)
                                                     (= "none" popup-display))
                                               (-> (menu-ref @board-dom) js/$ .toggle)
                                               (close-popup % (content-ref @board-dom) "stops looking at their deck" false true))))}
        (when (pos? deck-count-number)
          [facedown-card (:side @identity) ["bg"] nil])
        ;; todo - again, can we pass the server count into the tr?
        [:div.header {:class "darkbg server-label"}
         [tr-span tr-vec {:cnt deck-count-number}]]]
       (when (and (= render-side player-side) (not (is-replay?)))
         [:div.panel.blue-shade.menu {:ref #(swap! board-dom assoc menu-ref %)}
          [:div {:on-click #(do (send-command "shuffle")
                                (-> (menu-ref @board-dom) js/$ .fadeOut))}
           [tr-span [:game_shuffle "Shuffle"]]]
          [:div {:on-click #(show-deck % ref)}
           [tr-span [:game_show "Show"]]]])
       (when (and (= render-side player-side) (not (is-replay?)))
         [:div.panel.blue-shade.popup {:ref #(swap! board-dom assoc content-ref %)}
          [:div
           [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" false true)}
            [tr-span [:game_close "Close"]]]
           [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" true true)}
            [tr-span [:game_close-shuffle "Close & Shuffle"]]]]
          (doall
            (for [card @deck]
              ^{:key (:cid card)}
              [card-view card]))])])))

(defn discard-view-runner [player-side discard]
  (let [s (r/atom {})]
    (fn [player-side discard]
      [:div.discard-container (drop-area "Heap" {})
       [:div.blue-shade.discard {:on-click #(-> (:popup @s) js/$ .fadeToggle)}
        (when-not (empty? @discard)
          [card-view (last @discard) nil true])
        [:div.header {:class (str "server-label "
                                  (if (some graveyard-highlight-card? @discard)
                                    "graveyard-highlight-bg"
                                    "darkbg"))}
         [tr-span [:game_heap "Heap"] {:cnt (count @discard)}]]]
       [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                     :class (if (= player-side :runner) "me" "opponent")}
        [:div
         [:a {:on-click #(close-popup % (:popup @s) nil false false)}
          [tr-span [:game_close "Close"]]]]
        (doall
          (for [card (if (sort-heap?) (sort-heap @discard) @discard)]
            ^{:key (:cid card)}
            [card-view card]))]])))

(defn discard-view-corp [player-side discard]
  (let [s (r/atom {})]
    (fn [player-side discard]
      (let [draw-card #(if (faceup? %1)
                         [card-view %1 nil %2]
                         (if (or (= player-side :corp)
                                 (spectator-view-hidden?))
                           [:div.unseen [card-view %1 nil %2]]
                           [facedown-card "corp"]))]
        [:div.discard-container (drop-area "Archives" {})
         [:div.blue-shade.discard {:on-click #(-> (:popup @s) js/$ .fadeToggle)}
          (when-not (empty? @discard)
            [:<> {:key "discard"} (draw-card (last @discard) true)])
          [:div.header {:class (str "server-label "
                                    (if (some (if (or (= player-side :corp) (spectator-view-hidden?))
                                                graveyard-highlight-card?
                                                (every-pred graveyard-highlight-card? :seen))
                                              @discard)
                                      "graveyard-highlight-bg"
                                      "darkbg"))}
           (let [total (count @discard)
                 face-up (count (filter faceup? @discard))]
             [tr-span [:game_archives "Archives"]
              {:faceup face-up
               :facedown (- total face-up)}])]]
         [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                       :class (if (= (:side @game-state) :runner) "opponent" "me")}
          [:div
           [:a {:on-click #(close-popup % (:popup @s) nil false false)}
            [tr-span [:game_close "Close"]]]
           [:label (let [d @discard
                         total (count d)
                         face-up (count (filter faceup? d))]
                     [tr-span [:game_face-down-count]
                      {:total total
                       :facedown (- total face-up)}])]]
          (doall
            (for [[idx c] (map-indexed vector (if (sort-archives?) (sort-archives @discard) @discard))]
              ^{:key idx}
              [:div (draw-card c false)]))]]))))

(defn rfg-view
  ([cards tr-vec popup] (rfg-view cards tr-vec popup nil))
  ([cards tr-vec popup noclick]
   (let [dom (atom {})]
     (fn [cards tr-vec popup]
       (when-not (empty? @cards)
         (let [size (count @cards)]
           [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")
                                       :on-click (when popup #(-> (:rfg-popup @dom) js/$ .fadeToggle))}
            (doall
              (map-indexed (fn [i card]
                             [:div.card-wrapper {:key i
                                                 :style {:left (when (> size 1) (* (/ 128 size) i))}}
                              [:div [card-view card nil noclick]]])
                           @cards))
            [label @cards {:tr-vec tr-vec}]
            (when popup
              [:div.panel.blue-shade.popup {:ref #(swap! dom assoc :rfg-popup %)
                                            :class "opponent"}
               [:div
                [:a {:on-click #(close-popup % (:rfg-popup @dom) nil false false)} [tr-span [:game_close "Close"]]]
                [tr-element :label [:game_card-count] {:cnt size}]]
               (doall
                 (for [c @cards]
                   ^{:key (:cid c)}
                   [card-view c]))])]))))))

(defn play-area-view [user tr-vec cards]
  (fn [user tr-vec cards]
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
         [label @cards {:tr-vec tr-vec}]]))))

(defn scored-view [scored agenda-point agenda-point-req me?]
  (let [size (count @scored)
        ctrl (if me? stat-controls (fn [key content] content))
        point-req @agenda-point-req]
    [:div.panel.blue-shade.scored.squeeze
     (doall
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:key i
                                          :style {:left (when (> size 1) (* (/ 128 (dec size)) i))}}
                       [:div [card-view card]]])
                    @scored))
     [label @scored {:tr-vec [:game_scored-area "Scored Area"]}]
     [:div.stats-area
      (ctrl :agenda-point (if (= 7 point-req)
                            [tr-element :div [:game_agenda-count] {:agenda-point @agenda-point}]
                            [tr-element :div [:game_agenda-count-with-req] {:agenda-point @agenda-point
                                                                            :agenda-point-req point-req}]))]]))


(defn run-arrow [run]
  [:div.run-arrow [:div {:class (cond
                                  (= "movement" (:phase run))
                                  "movement"
                                  (= "approach-ice" (:phase run))
                                  "approach"
                                  (= "encounter-ice" (:phase run))
                                  "encounter"
                                  :else
                                  "")}]])

(defn server-view [{:keys [key server central-view run]} opts]
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
                  flipped (not (faceup? card))]
              [:div.server-card {:key (:cid card)
                                 :class (str (when central-view "central ")
                                             (when (or central-view
                                                       (and (< 1 (count content)) (not is-first)))
                                               "shift"))}
               [card-view card flipped]]))))
      (if central-view
        [label content (assoc opts :classes "server-label" :hide-cursor true)]
        [label content (assoc opts :classes "server-label" :hide-cursor true :name (str (tr [:game_server "Server"] {:num key})) :tr-vec [:game_server "Server"] :tr-params {:num key})])]]))

(defn stacked-label [cursor similar-servers opts]
  (let [similar-server-names (->> similar-servers
                                  (map first)
                                  (map remote->str-name))
        full-server-names (cons (get-in opts [:opts :name]) similar-server-names)
        numbers (map #(second (split % " ")) full-server-names)]
    [label full-server-names (assoc opts
                                        :classes "server-label"
                                        :name (str "Servers " (join ", " numbers))
                                        :tr-vec [:game_server "Server"] 
                                        :tr-params {:num (join ", " numbers)}
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
                      flipped (not (faceup? card))]
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
             {:opts {:name (remote->str-name (first server))}}]
            [stacked-view {:key num
                           :server (second server)
                           :similar-servers similar-servers
                           :run (when
                                  (some #(= server-type (str "remote" %)) (map #(remote->num (first %)) all-servers))
                                  (= server-type (str "remote" num)) @run)}
             {:opts {:name (remote->str-name (first server))}}])))
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

(defn- ghost-card
  "recursively ghosts a card and all hosted cards"
  [card]
  (let [hosted (map ghost-card (:hosted card))]
    (assoc card :ghost true :hosted hosted)))

(defn- find-hosted-programs
  "finds all programs hosted on ice, and makes them have the 'ghost' key"
  [servers]
  (let [servers (concat [(:archives @servers) (:rd @servers) (:hq @servers)] (map second (get-remotes @servers)))
        ices (mapcat :ices servers)
        hosted (mapcat :hosted ices)
        hosted-programs (filter program? hosted)]
    (map ghost-card hosted-programs)))

(defn board-view-runner [player-side identity deck deck-count hand hand-count discard rig run servers]
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
     (let [hosted-programs (when (get-in @app-state [:options :ghost-trojans])
                             (find-hosted-programs servers))]
       (doall
         (for [zone (runner-f [:program :hardware :resource :facedown])]
           ^{:key zone}
           [:div
            (if (get-in @app-state [:options :stacked-cards] false)
                                        ; stacked mode
              (let [cards (get @rig zone)
                    cards (if (= zone :program)
                            (concat cards hosted-programs)
                            cards)
                    distinct-cards (vals (group-by get-title cards))]
                (show-distinct-cards distinct-cards))
                                        ; not in stacked mode
              (doall (for [c (if (= zone :program)
                               (concat (get @rig zone) hosted-programs)
                               (get @rig zone))]
                       ^{:key (:cid c)}
                       [:div.card-wrapper {:class (when (playable? c) "playable")}
                        [card-view c]])))])))
       (when is-me centrals)]))

(defn build-win-box
  "Builds the end of game pop up game end"
  [game-state]
  (let [win-shown (r/atom false)
        corp-stats (r/cursor game-state [:stats :corp])
        runner-stats (r/cursor game-state [:stats :runner])]
    (fn [game-state]
      (when (and (:winner @game-state)
                 (not @win-shown))
        (let [winner (or (:winner @game-state) "-")
              winning-user (or (:winning-user @game-state) "-")
              turn (:turn @game-state)
              reason (capitalize (:reason @game-state))
              time (get-in @game-state [:stats :time :elapsed])
              args {:winner winning-user
                    :side winner
                    :reason reason
                    :turn turn}]
          [:div.win.centered.blue-shade
           [:div
            (case reason
              "Decked" [tr-span [:game_win-decked] args]
              "Flatline" [tr-span [:game_win-flatlined] args]
              "Concede" [tr-span [:game_win-conceded] args]
              "Claim" [tr-span [:game_win-claimed] args]
              "Agenda" [tr-span [:game_win-points] args]
              #_:else [tr-span [:game_win-other] args])]
           [tr-element :div [:game_time-taken] {:time time}]
           [:br]
           [build-game-stats @corp-stats @runner-stats]
           (when (not= :spectator (:side @game-state))
             [:<>
              [:br]
              [:div {:class "end-of-game-buttons"}
               (when (= :corp (:side @game-state))
                 [:button#rez-all
                  {:on-click #(ws/ws-send! [:game/say {:gameid (current-gameid app-state)
                                                       :msg "/rez-all"}])}
                  [tr-span [:game_rez-all "Rez All"]]])
               [:button#reveal-hand
                {:on-click #(ws/ws-send! [:game/say {:gameid (current-gameid app-state)
                                                     :msg "/show-hand"}])}
                [tr-span [:game_reveal-my-hand "Reveal My Hand"]]]]])
           [:button.win-right {:on-click #(reset! win-shown true) :type "button"} "✘"]])))))

(defn- build-in-game-decklists
  "Builds the in-game decklist display"
  [corp-list runner-list]
  (let [lists (map-longest list nil corp-list runner-list)
        is-divider? (fn [c] (= "divider" (second c)))
        card-qty (fn [c] (if (is-divider? c) "" (second c)))
        card-name (fn [c]
                    (if (is-divider? c)
                      [:div {:text-align "left"}
                       [:strong (render-message (first c))]]
                      [:div {:text-align "left"
                             :on-mouse-over #(card-preview-mouse-over % zoom-channel)
                             :on-mouse-out #(card-preview-mouse-out % zoom-channel)}
                       (render-message (first c))]))]
    [:div
     [:table.decklists.table
      [:tbody
       [:tr.win.th
        [:td.win.th [tr-side "Corp"]] [:td.win.th]
        [:td.win.th [tr-side "Runner"]] [:td.win.th]]
       (doall (map-indexed
                (fn [i [corp runner]]
                  [:tr {:key i}
                   [:td (card-qty corp)] [:td (card-name corp)]
                   [:td (card-qty runner)] [:td (card-name runner)]])
                lists))]]]))

(defn build-decks-box
  "Builds the decklist display box for open decklists"
  [game-state]
  (let [show-decklists (r/cursor app-state [:display-decklists])]
    (fn [game-state]
      (when (and @show-decklists
                 (get-in @game-state [:decklists]))
        (let [corp-list (or (get-in @game-state [:decklists :corp]) {:- 1})
              runner-list (or (get-in @game-state [:decklists :runner]) {:- 1})]
          [:div.decklists.blue-shade
           [:br]
           [build-in-game-decklists corp-list runner-list]])))))

(defn build-start-box
  "Builds the start-of-game pop up box"
  [my-ident my-user my-hand prompt-state my-keep op-ident op-user op-keep me-quote op-quote my-side]
  (let [visible-quote (r/atom true)
        mulliganed (r/atom false)
        start-shown (r/cursor app-state [:start-shown])
        card-back (get-in @app-state [:options :card-back])]
    (fn [my-ident my-user my-hand prompt-state my-keep op-ident op-user op-keep me-quote op-quote my-side]
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
                 [:img {:src     url :alt (get-title @my-ident) :onLoad #(-> % .-target js/$ .show)
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
                        :alt (get-title @op-ident)
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
                               [:img.start-card {:src (facedown-card-url (:side @my-ident))}]]
                              [:div.card-front
                               (when-let [url (image-url card)]
                                 [:div {:on-mouse-enter #(put-game-card-in-channel card zoom-channel)
                                        :on-mouse-leave #(put! zoom-channel false)}
                                  [:img.start-card {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]
                             (when-let [elem (.querySelector js/document (str "#startcard" i))]
                               (js/setTimeout #(.add (.-classList elem) "flip") (+ 1000 (* i 300))))])
                          @my-hand))]])
             [:div.mulligan
              (if (or (= :spectator @my-side)
                      (and @my-keep @op-keep))
                [cond-button [tr-span (if (= :spectator @my-side)
                                        [:game_close "Close"]
                                        [:game_start "Start Game"])]
                 true #(swap! app-state assoc :start-shown true)]
                (list ^{:key "keepbtn"} [cond-button [tr-span [:game_keep "Keep"]]
                                         (= "mulligan" (:prompt-type @prompt-state))
                                         #(send-command "choice" {:eid (prompt-eid (:side @game-state)) :choice {:uuid (->> (:choices @prompt-state)
                                                                                      (filter (fn [c] (= "Keep" (:value c))))
                                                                                      first
                                                                                      :uuid)}})]
                      ^{:key "mullbtn"} [cond-button [tr-span [:game_mulligan "Mulligan"]]
                                         (= "mulligan" (:prompt-type @prompt-state))
                                         #(do (send-command "choice" {:eid (prompt-eid (:side @game-state)) :choice {:uuid (->> (:choices @prompt-state)
                                                                                          (filter (fn [c] (= "Mulligan" (:value c))))
                                                                                          first
                                                                                          :uuid)}})
                                              (reset! mulliganed true))]))]]]
           [:br]
           [:button.win-right {:on-click #(swap! app-state assoc :start-shown true) :type "button"} "✘"]])))))

(defn get-run-ices []
  (let [server (-> (:run @game-state)
                   :server
                   first
                   keyword)]
    (get-in @game-state (concat [:corp :servers] [server] [:ices]))))

(defn get-current-ice []
  (let [run-ice (get-run-ices)
        pos (get-in @game-state [:run :position])
        phase (get-in @game-state [:run :phase])
        encounter-ice (-> @game-state :encounters :ice)
        get-ice-from-pos? (or (= "movement" phase)
                              (get-in @game-state [:run :approached-ice-in-position?]))]
    (or encounter-ice
        (when (and get-ice-from-pos?
                   pos
                   (pos? pos)
                   (<= pos (count run-ice)))
          (nth run-ice (dec pos))))))

(defn phase->title
  [phase]
  (case phase
    "initiation"      [:game_initiation "Initiation"]
    "approach-ice"    [:game_approach-ice "Approach ice"]
    "approach-server" [:game_approach-server "Approach server"]
    "encounter-ice"   [:game_encounter-ice "Encounter ice"]
    "movement"        [:game_movement "Movement"]
    "success"         [:game_success "Success"]
    [:game_unknown-phase "Unknown phase"]))

(defn phase->next-phase-title
  ([run] (phase->next-phase-title (:phase @run) (:position @run)))
  ([phase position]
   (case phase
     "initiation" (tr [:game_approach-ice "Approach ice"])
     "approach-ice" (if (rezzed? (get-current-ice))
                      (tr [:game_encounter-ice "Encounter ice"])
                      (tr [:game_movement "Movement"]))
     "encounter-ice" (tr [:game_movement "Movement"])
     "movement" (if (zero? position)
                  (tr [:game_success "Success"])
                  (tr [:game_approach-ice "Approach ice"]))
     "success" (tr [:game_run-ends "Run ends"])
     ;; Error
     (tr [:game_no-current-run "No current run"]))))

(defn corp-run-div
  [run encounters]
  (let [ice (get-current-ice)]
    [:div.panel.blue-shade
     (when (and @encounters
                ice)
       [:<>
        [:div {:style {:text-align "center"}
               :on-mouse-over #(card-highlight-mouse-over % ice button-channel)
               :on-mouse-out #(card-highlight-mouse-out % ice button-channel)}
         [tr-span [:game_encounter-ice "Encounter ice"]] ": " (render-message (get-title ice))]
        [:hr]
        (when (or (:button @app-state) (get-in @app-state [:options :display-encounter-info]))
          [encounter-info-div ice])])
     (when @run
       [:h4 [tr-span [:game_current-phase "Current phase"]] ":" [:br] [tr-span (phase->title (:phase @run))]])

     (cond
       (and (= "approach-ice" (:phase @run))
            ice)
       [cond-button
        [:span [tr-span [:game_rez "Rez"]] " " (get-title ice)]
        (not (rezzed? ice))
        #(send-command "rez" {:card ice
                              :press-continue (get-in @app-state [:options :pass-on-rez])})]

       (or (= "encounter-ice" (:phase @run))
           @encounters)
       [cond-button
        [tr-span [:game_fire-unbroken "Fire unbroken subroutines"]]
        (and (seq (:subroutines ice))
             (some #(and (not (:broken %))
                         (not (:fired %))
                         (:resolve % true))
                   (:subroutines ice)))
        #(send-command "unbroken-subroutines" {:card ice})])

     (cond
       @encounters
       ;;Encounter continue button
       (let [pass-ice? (and (= "encounter-ice" (:phase @run))
                            (= 1 (:encounter-count @encounters)))]
         [cond-button
          (if pass-ice?
            [tr-span [:game_continue-to "Continue to"] {:phase (phase->next-phase-title run)}]
            [tr-span [:game_continue "Continue"]])
          (not= "corp" (:no-action @encounters))
          #(send-command "continue")])

       ;; initiation
       (= "initiation" (:phase @run))
       ;; TODO - should these trs be refactored to not double-process?
       [cond-button
        [tr-span [:game_continue-to "Continue to"]
         {:phase (if (zero? (:position @run))
                   (tr [:game_approach-server "Approach server"])
                   (tr [:game_approach-ice "Approach ice"]))}]
        (not= "corp" (:no-action @run))
        #(send-command "continue")]

       ;;Non-encounter continue button
       :else
       [cond-button
        (if (or (:next-phase @run)
                (zero? (:position @run)))
          [tr-span [:game_no-further "No further actions"]]
          [tr-span [:game_continue-to "Continue to"] {:phase (phase->next-phase-title run)}])
        (and (not= "initiation" (:phase @run))
             (not= "success" (:phase @run))
             (not= "corp" (:no-action @run)))
        #(send-command "continue")])

     (when (and @run
                (<= (:encounter-count @encounters) 1)
                (not= "success" (:phase @run)))
       [checkbox-button
        [tr-span [:game_stop-auto-pass "Stop auto-passing priority"]]
        [tr-span [:game_auto-pass "Auto-pass priority"]]
        (:corp-auto-no-action @run)
        #(send-command "toggle-auto-no-action")])]))

(defn runner-run-div
  [run encounters]
  (let [phase (:phase @run)
        next-phase (:next-phase @run)
        ice (get-current-ice)
        pass-ice? (and (= "encounter-ice" phase)
                       (= 1 (:encounter-count @encounters)))]
    [:div.panel.blue-shade
     (when (and @encounters
                ice)
       [:<>
        [:div {:style {:text-align "center"}
               :on-mouse-over #(card-highlight-mouse-over % ice button-channel)
               :on-mouse-out #(card-highlight-mouse-out % ice button-channel)}
         [tr-span [:game_encounter-ice "Encounter ice"]] ": " (render-message (get-title ice))]
        [:hr]
        (when (or (:button @app-state)  (get-in @app-state [:options :display-encounter-info]))
          [encounter-info-div ice])])
     (when @run
       [:h4 [tr-span [:game_current-phase "Current phase"]] ":" [:br] [tr-span (phase->title phase)]])

     (cond
       (and (:next-phase @run)
            (not= "initiation" (:phase @run)))
       [cond-button
        [tr-span (phase->title next-phase)]
        (and next-phase
             (not (:no-action @run)))
        #(send-command "start-next-phase")]

       (= "initiation" (:phase @run))
       [cond-button
        [tr-span [:game_continue-to "Continue to"] {:phase (if (zero? (:position @run))
                                                             (tr [:game_approach-server "Approach server"])
                                                             (tr [:game_approach-ice "Approach ice"]))}]
        (not= "runner" (:no-action @run))
        #(send-command "continue")]

       (and (not next-phase)
            (not (zero? (:position @run)))
            (not @encounters))
       [cond-button
        [tr-span [:game_continue-to "Continue to"] {:phase (phase->next-phase-title run)}]
        (not= "runner" (:no-action @run))
        #(send-command "continue")]

       (and (zero? (:position @run))
            (not @encounters)
            (= "movement" phase))
       [cond-button [tr-span [:game_breach-server "Breach server"]]
        (not= "runner" (:no-action @run))
        #(send-command "continue")])

     (when @encounters
       [cond-button
        [tr-span [:game_let-subs-fire "Let unbroken subroutines fire"]]
        (and (seq (:subroutines ice))
             (some #(and (not (:broken %))
                         (not (:fired %))
                         (:resolve % true))
                   (:subroutines ice)))
        #(send-command "system-msg"
                       {:msg (str "indicates to fire all unbroken subroutines on " (get-title ice))})])

     (when @encounters
       [cond-button
        (if pass-ice?
          [tr-span [:game_continue-to "Continue to"] {:phase (phase->next-phase-title run)}]
          [tr-span [:game_continue "Continue"]])
        (not= "runner" (:no-action @encounters))
        #(send-command "continue")])

     (when (and @run
                (not (:forced-encounter @game-state))
                (not= "success" phase))
       [cond-button
        [tr-span [:game_jack-out "Jack Out"]]
        (and (= "movement" phase)
             (not (:cannot-jack-out @run))
             (not= "runner" (:no-action @run)))
        #(send-command "jack-out")])]))

(defn run-div
  [side run encounters]
  (if (= side :corp)
    [corp-run-div run encounters]
    [runner-run-div run encounters]))

(defn trace-div
  [{:keys [base strength player link bonus choices corp-credits runner-credits unbeatable beat-trace] :as prompt-state}]
  (r/with-let [!value (r/atom 0)]
    [:div
     (when base
       ;; This is the initial trace prompt
       (if (nil? strength)
         (if (= "corp" player)
           ;; This is a trace prompt for the corp, show runner link + credits
           [:div.info [tr-side "Runner"] ": " link [:span {:class "anr-icon link"}]
            " + " runner-credits [:span {:class "anr-icon credit"}]]
           ;; Trace in which the runner pays first, showing base trace strength and corp credits
           [:div.info [tr-span [:game_trace "Trace"]] ": " (if bonus (+ base bonus) base)
            " + " corp-credits [:span {:class "anr-icon credit"}]])
         ;; This is a trace prompt for the responder to the trace, show strength
         (if (= "corp" player)
           [:div.info "vs Trace: " strength]
           [:div.info "vs Runner: " strength [:span {:class "anr-icon link"}]])))
     [:div.credit-select
      ;; Inform user of base trace / link and any bonuses
      (when base
        (if (nil? strength)
          (if (= "corp" player)
            (let [strength (if bonus (+ base bonus) base)]
              [:span (str strength " + ")])
            [:span link " " [:span {:class "anr-icon link"}] " + " ])
          (if (= "corp" player)
            [:span link " " [:span {:class "anr-icon link"}] " + " ]
            (let [strength (if bonus (+ base bonus) base)]
              [:span strength " + "]))))
      [:select#credit {:value @!value
                       :on-change #(reset! !value (.. % -target -value))
                       :onKeyUp #(when (= "Enter" (.-key %))
                                   (-> "#trace-submit" js/$ .click)
                                   (.stopPropagation %))}
       (doall (for [i (range (inc choices))]
                [:option {:value i :key i} i]))]
      [tr-span [:game_credits "credits"]]]
     (when (or unbeatable beat-trace)
       (let [beat-vec (if unbeatable
                        [:game_unbeatable "Make Unbeatable"]
                        [:game_beat-trace "Beat Trace"])]
         [:button#trace-unbeatable
          {:on-click #(reset! !value (or unbeatable beat-trace))}
          [:div [tr-span beat-vec] " (" (or unbeatable beat-trace) [:span {:class "anr-icon credit"}] ")"]]))
     [:button#trace-submit {:on-click #(send-command "choice" {:eid (prompt-eid (:side @game-state)) :choice (-> "#credit" js/$ .val str->int)})}
      [tr-span [:game_ok "OK"]]]]))

(defn prompt-div
  [me {:keys [card msg prompt-type choices] :as prompt-state}]
  (let [id (atom 0)]
    [:div.panel.blue-shade
     (when (and card (not= "Basic Action" (:type card)))
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
             [tr-span [:game_card "Card"]] ": " (render-message (get-title card))]
            [:div.prompt-card-preview [card-view card false]]))
        [:hr]])
     [:h4 (render-message msg)]
     (cond
       ;; number prompt
       (:number choices)
       (let [n (:number choices)]
         [:div
          [:div.credit-select
           [:select#credit {:default-value (:default choices 0)
                            :onKeyUp #(when (= "Enter" (.-key %))
                                        (-> "#number-submit" js/$ .click)
                                        (.stopPropagation %))}
            (doall (for [i (range (:minimum choices 0) (inc n))]
                     [:option {:key i :value i} i]))]]
          [:button#number-submit {:on-click #(send-command "choice"
                                                           {:eid (prompt-eid (:side @game-state)) :choice (-> "#credit" js/$ .val str->int)})}
           [tr-span [:game_ok "OK"]]]])

       ;; trace prompts require their own logic
       (= prompt-type "trace")
       [trace-div prompt-state]

       ;; choice of number of credits
       (= choices "credit")
       (let [n (get-in @game-state [(:side @game-state) :credit])]
         [:div
          [:div.credit-select
           [:select#credit {:default-value (:default choices 0)
                            :onKeyUp #(when (= "Enter" (.-key %))
                                        (-> "#credit-submit" js/$ .click)
                                        (.stopPropagation %))}
            (doall (for [i (range (inc n))]
                     [:option {:key i :value i} i]))]]
          [:button#credit-submit {:on-click #(send-command "choice"
                                             {:eid (prompt-eid (:side @game-state)) :choice (-> "#credit" js/$ .val str->int)})}
           [tr-span [:game_ok "OK"]]]])

       ;; auto-complete text box
       (:card-title choices)
       [:div
        [:div.credit-select
         [:input#card-title {:placeholder "Enter a card title"
                             :onKeyUp #(when (= "Enter" (.-key %))
                                         (-> "#card-submit" js/$ .click)
                                         (.stopPropagation %))}]]
        [:button#card-submit {:on-click #(send-command "choice" {:eid (prompt-eid (:side @game-state)) :choice (-> "#card-title" js/$ .val)})}
         [tr-span [:game_ok "OK"]]]]

       ;; choice of specified counters on card
       (:counter choices)
       (let [counter-type (keyword (:counter choices))
             num-counters (get-in prompt-state [:card :counter counter-type] 0)]
         [:div
          [:div.credit-select
           [:select#credit {:onKeyUp #(when (= "Enter" (.-key %))
                                        (-> "#counter-submit" js/$ .click)
                                        (.stopPropagation %))}
            (doall (for [i (range (inc num-counters))]
                     [:option {:key i :value i :data-i18n-key :game_credits} i]))]
           [tr-span [:game_credits "credits"]]]
          [:button#counter-submit {:on-click #(send-command "choice"
                                             {:eid (prompt-eid (:side @game-state)) :choice (-> "#credit" js/$ .val str->int)})}
           [tr-span [:game_ok "OK"]]]])

       ;; otherwise choice of all present choices
       :else
       (doall (for [{:keys [idx uuid value]} choices
                    :when (not= value "Hide")]
                [:button {:key idx
                          :on-click #(do (send-command "choice" {:eid (prompt-eid (:side @game-state)) :choice {:uuid uuid}})
                                         (card-highlight-mouse-out % value button-channel))
                          :on-mouse-over
                          #(card-highlight-mouse-over % value button-channel)
                          :on-mouse-out
                          #(card-highlight-mouse-out % value button-channel)}
                 (render-message (or (not-empty (get-title value)) value))])))]))

(defn basic-actions [{:keys [side active-player end-turn runner-phase-12 corp-phase-12 me runner-post-discard corp-post-discard]}]
  (let [phase-12 (or @runner-phase-12 @corp-phase-12)
        post-discard (or @corp-post-discard @runner-post-discard)
        phase-locked (or phase-12 post-discard)]
    [:div.panel.blue-shade
     (if (= (keyword @active-player) side)
       (when (and (not phase-locked) (zero? (:click @me)) (not @end-turn))
         [:button {:on-click #(do (close-card-menu)
                                  (send-command "end-turn"))}
          [tr-span [:game_end-turn "End Turn"]]])
       (when (and @end-turn (not post-discard))
         [:button {:on-click #(do
                                (swap! app-state assoc :start-shown true)
                                (send-command "start-turn"))}
          [tr-span [:game_start-turn "Start Turn"]]]))
     ;; POST-DISCARD PHASE
     (when (and (= (keyword @active-player) side) post-discard)
       [cond-button
        [tr-span [:game_continue-end-turn "Continue End Turn"]]
        (if (:requires-consent post-discard)
          (not (side post-discard))
          true)
        #(send-command (if (:requires-consent post-discard)
                         "post-discard-pass-priority"
                         "end-post-discard"))])
     (when (and (not= (keyword @active-player) side) (:requires-consent post-discard))
       [cond-button
        [tr-span [:game_allow-turn-end "Allow Turn End"]]
        (not (side post-discard))
        #(send-command "post-discard-pass-priority")])
     ;; PHASE 1.2
     (when (and (= (keyword @active-player) side) phase-12)
       [cond-button
        (if (= side :corp) [tr-span [:game_mandatory-draw "Mandatory Draw"]] [tr-span [:game_take-clicks "Take Clicks"]])
        (if (:requires-consent phase-12)
          (not (side phase-12))
          true)
        #(send-command (if (:requires-consent phase-12)
                         "phase-12-pass-priority"
                         "end-phase-12"))])
     (when (and (not= (keyword @active-player) side) (:requires-consent phase-12))
       [cond-button
        (if (= side :runner) [tr-span [:game_allow-mandatory-draw "Allow Mandatory Draw"]] [tr-span [:game_allow-take-clicks "Allow Take Clicks"]])
        (not (side phase-12))
        #(send-command "phase-12-pass-priority")])
     ;; BASIC ACTIONS
     (when (= side :runner)
       [:div
        [cond-button [tr-span [:game_remove-tag "Remove Tag"]]
         (and (not phase-locked)
              (playable? (get-in @me [:basic-action-card :abilities 5]))
              (pos? (get-in @me [:tag :base])))
         #(send-command "remove-tag")]
        [:div.run-button.menu-container
         [cond-button [tr-span [:game_run "Run"]]
          (and (not phase-locked) (pos? (:click @me)))
          #(do (send-command "generate-runnable-zones")
               (if (= :run-button (:source @card-menu))
                 (close-card-menu)
                 (open-card-menu :run-button)))]
         [:div.panel.blue-shade.servers-menu (when (= :run-button (:source @card-menu))
                                               {:class "active-menu"
                                                :style {:display "inline"}})
          [:ul
           (let [servers (get-in @game-state [:runner :runnable-list])]
             (doall
               (map-indexed (fn [_ label]
                              ^{:key label}
                              [card-menu-item (tr-game-prompt label)
                               #(do (close-card-menu)
                                    (send-command "run" {:server label}))])
                            servers)))]]]])
     (when (= side :corp)
       [cond-button [tr-span [:game_purge "Purge"]]
        (and (not phase-locked) (playable? (get-in @me [:basic-action-card :abilities 6])))
        #(send-command "purge")])
     (when (= side :corp)
       [cond-button [tr-span [:game_trash-resource "Trash Resource"]]
        (and (not phase-locked)
             (playable? (get-in @me [:basic-action-card :abilities 5]))
             (is-tagged? game-state))
        #(send-command "trash-resource")])
     [cond-button [tr-span [:game_draw "Draw"]]
      (and (not phase-locked)
           (playable? (get-in @me [:basic-action-card :abilities 1]))
           (pos? (:deck-count @me)))
      #(send-command "draw")]
     [cond-button [tr-span [:game_gain-credit "Gain Credit"]]
      (and (not phase-locked) (playable? (get-in @me [:basic-action-card :abilities 0])))
      #(send-command "credit")]]))

(defn button-pane [{:keys [side prompt-state]}]
  (let [autocomp (r/track (fn [] (get-in @prompt-state [:choices :autocomplete])))
        show-discard? (r/track (fn [] (get-in @prompt-state [:show-discard])))
        prompt-type (r/track (fn [] (get-in @prompt-state [:prompt-type])))
        opened-by-system (r/atom false)]
    (r/create-class
      {:display-name "button-pane"

       :component-did-update
       (fn []
         (when (pos? (count @autocomp))
           (-> "#card-title" js/$ (.autocomplete (clj->js {"source" @autocomp}))))
         (cond @show-discard? (do (-> ".me .discard-container .popup" js/$ .fadeIn)
                                  (reset! opened-by-system true))
               @opened-by-system (do (-> ".me .discard-container .popup" js/$ .fadeOut)
                                     (reset! opened-by-system false)))
         (if (= "select" @prompt-type)
           (set! (.-cursor (.-style (.-body js/document))) "url('/img/gold_crosshair.png') 12 12, crosshair")
           (set! (.-cursor (.-style (.-body js/document))) "default"))
         (when (= "card-title" @prompt-type)
           (-> "#card-title" js/$ .focus)))

       :reagent-render
       (fn [{:keys [side run encounters prompt-state me] :as button-pane-args}]
         [:div.button-pane {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                            :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
          (cond
            (and @prompt-state (not= "run" (get-in @prompt-state [:prompt-type])))
            [prompt-div me @prompt-state]
            (or @run
                @encounters)
            [run-div side run encounters]
            :else
            [basic-actions button-pane-args])])})))

(defn- time-until
  "Helper method for timer. Computes how much time is left until `end`"
  [end]
  (let [now (inst/now)
        diff (duration/between now end)
        total-seconds (duration/get diff chrono/seconds)
        minutes (abs (quot total-seconds 60))
        seconds (mod (abs total-seconds) 60)
        positive (pos? total-seconds)]
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
  (let [end-time (-> start-date
                     (inst/parse)
                     (inst/plus timer chrono/minutes))
        remaining (r/atom nil)
        interval (r/atom nil)]
    (r/create-class
      {:component-did-mount
       (fn []
         (reset! interval
                 ;; Update timer at most every 1 sec
                 (js/setInterval #(reset! remaining (time-until end-time)) 1000)))
       :component-will-unmount
       (fn []
         (js/clearInterval @interval)
         (reset! interval nil))
       :reagent-render
       (fn []
         (when (and @remaining (not @hidden))
           [:span.float-center.timer
            {:class (warning-class @remaining)}
            (when-not (:pos @remaining) "-")
            (:minutes @remaining) [tr-span [:game_minutes "m:"]]
            (:seconds @remaining) [tr-span [:game_seconds-remaining "s remaining"]]]))})))

(defn- time-since
  "Helper method for match duration. Computes how much time since game start"
  [start]
  (let [start-time (-> start
                       (inst/parse))
        now (inst/now)
        diff (duration/between start-time now)
        total-seconds (duration/get diff chrono/seconds)
        minutes (abs (quot total-seconds 60))
        seconds (mod (abs total-seconds) 60)]
    {:minutes minutes :seconds seconds}))

(defn- time-left-in-round
  "Helper method for match duration. Computes how much time since game start"
  [end]
  (let [end-time (-> end
                     (inst/parse))
        now (inst/now)
        diff (duration/between end-time now)
        total-seconds (duration/get diff chrono/seconds)
        minutes (abs (quot total-seconds 60))
        seconds (mod (abs total-seconds) 60)]
    {:minutes minutes :seconds seconds}))

(defn match-duration
  "Component which displays a readout of the time since the start of the match."
  [start-date hidden]
  (let [duration (r/atom nil)
        interval (r/atom nil)]
    (r/create-class
      {:component-did-mount
       (fn []
         (reset! interval
                 ;; Update timer at most every 1 sec
                 (js/setInterval #(reset! duration (time-since start-date)) 1000)))
       :component-will-unmount
       (fn []
         (js/clearInterval @interval)
         (reset! interval nil))
       :reagent-render
       (fn []
         (when (not @hidden)
           [:span.float-center.timer
            (:minutes @duration) [tr-span [:game_minutes "m:"]]
            (:seconds @duration) [tr-span [:game_seconds "s"]]]))})))

(defn- adjusted-round-end [app-state]
  (let [{:keys [room round-end-time excluded? time-extension]} (:current-game @app-state)]
    (when (and (= room "competitive")
               (not excluded?)
               round-end-time)
      (inst/plus round-end-time (duration/of-minutes (or time-extension 0))))))

(defn starting-timestamp [start-date timer]
  ;; I don't like using js/Date, but `toLocalTimeString`
  ;; is just too convenient
  (let [hide-timer (r/atom false)]
    (fn []
      (let [round-end (adjusted-round-end app-state)
            round-end-date (when round-end (js/Date. (inst/to-epoch-milli round-end)))
            extension (get-in @app-state [:current-game :time-extension] 0)]
        (if round-end
          [:div.panel.blue-shade.timestamp
           [tr-element :span.float-center
            [:game_round-end "Round end"] {:timestamp (.toLocaleTimeString round-end-date)}]
           (when (pos? extension)
             [tr-element :span.float-center
              [:game_round-extension (str "(includes " extension "m time extension)")]
              {:extension extension}])]

          [:div.panel.blue-shade.timestamp
           [tr-element :span.float-center
            [:game_game-start "Game start"] {:timestamp (.toLocaleTimeString (js/Date. start-date))}]
           [:<>
            [:span.pm {:on-click #(swap! hide-timer not)}
             (if @hide-timer "+" "-")]
            (if timer [:span {:on-click #(swap! hide-timer not)}
                       [time-remaining start-date timer hide-timer]]
                [:span {:on-click #(swap! hide-timer not)}
                 [match-duration start-date hide-timer]])]])))))

(defn- handle-click [{:keys [render-board?]} e]
  (when render-board?
    (when (-> e .-target (.closest ".menu-container") nil?)
      (close-card-menu))))

(defn- get-element-for-num
  [num]
  (when-let [container (or (-> js/document (.getElementsByClassName "active-menu") array-seq first)
                           (-> js/document (.getElementsByClassName "button-pane") array-seq first))]
    (let [index (if (zero? num)
                  9
                  (dec num))
          elements (if (:source @card-menu)
                     (-> container (.getElementsByTagName "li") array-seq)
                     (-> container (.getElementsByTagName "button") array-seq))]
      (nth elements index nil))))

(defn- handle-num-key-down
  [num]
  (when-let [element (get-element-for-num num)]
    (.focus element)))

(defn- handle-num-key-up
  [num]
  (when-let [element (get-element-for-num num)]
    (when (= element (.-activeElement js/document))
      (.click element)
      (.blur element))))

(defn- focus-log-input [clear-input?]
  (when-let [log-input (-> js/document (.getElementById "log-input"))]
    (.focus log-input)
    (when clear-input?
      (set! (.-value log-input) ""))))

(defn- handle-key-down [{:keys [render-board?]} e]
  (when render-board?
    (let [active-element-type (.-type (.-activeElement js/document))
          not-text-input? (not= "text" active-element-type)
          can-focus? (-> js/document .-activeElement js/$ (.attr "tabindex"))]
      (case (.-key e)
        "Escape" (do (-> js/document .-activeElement .blur)
                     (close-card-menu))
        "Enter" (when-not (or active-element-type
                              can-focus?)
                  (focus-log-input false)
                  (.preventDefault e))
        "/" (when not-text-input?
              (focus-log-input true))
        ("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
        (when not-text-input?
          (handle-num-key-down (str->int (.-key e))))
        ;; else
        nil))))

(defn- handle-key-up [{:keys [side active-player render-board?
                             corp-phase-12 runner-phase-12
                             end-turn run
                             encounters active-page]} e]
  (when (and render-board?
             (not= "text" (.-type (.-activeElement js/document))))
    (let [clicks (:click (@side @game-state))
          active-player-kw (keyword @active-player)
          prompt-state (:prompt-state (@side @game-state))
          prompt-type (keyword (:prompt-type prompt-state))
          no-action (keyword (or (:no-action @run)
                                 (:no-action @encounters)))]
      (case (.-key e)
        " " (cond
              ;; keep default space behavior for focusable items
              (or (.-type (.-activeElement js/document))
                  (-> js/document .-activeElement js/$ (.attr "tabindex")))
              nil
              ;; continue run
              (or @run
                  @encounters)
              (when (and (or (not prompt-state)
                             (= :run prompt-type))
                         (not= @side no-action))
                (send-command "continue")
                (.stopPropagation e))
              ;; no action for prompt
              prompt-state
              nil
              ;; click for credits
              (pos? clicks)
              (do (send-command "credit")
                  (.stopPropagation e))
              ;; end turn
              (and (= active-player-kw @side)
                   (not (or @runner-phase-12 @corp-phase-12))
                   (zero? clicks)
                   (not @end-turn))
              (do (send-command "end-turn")
                  (close-card-menu)
                  (.stopPropagation e))
              ;; gain clicks/mandatory draw
              (and (= active-player-kw @side)
                   (or @runner-phase-12 @corp-phase-12))
              (do (send-command "end-phase-12")
                  (.stopPropagation e))
              ;; start turn
              (and (not= active-player-kw @side)
                   @end-turn)
              (do (send-command "start-turn")
                  (.stopPropagation e)))
        "Alt" (.preventDefault e)
        ("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
        (handle-num-key-up (str->int (.-key e)))
        ;; else
        nil))))

(defn gameboard []
  (let [active (r/cursor app-state [:active-page])
        start-date (r/cursor game-state [:start-date])
        timer (r/cursor game-state [:options :timer])
        run (r/cursor game-state [:run])
        encounters (r/cursor game-state [:encounters])
        side (r/cursor game-state [:side])
        turn (r/cursor game-state [:turn])
        end-turn (r/cursor game-state [:end-turn])
        corp-phase-12 (r/cursor game-state [:corp-phase-12])
        corp-post-discard (r/cursor game-state [:corp-post-discard])
        runner-phase-12 (r/cursor game-state [:runner-phase-12])
        runner-post-discard (r/cursor game-state [:runner-post-discard])
        corp (r/cursor game-state [:corp])
        runner (r/cursor game-state [:runner])
        active-player (r/cursor game-state [:active-player])
        zoom-card (r/cursor app-state [:zoom])
        background (r/cursor app-state [:options :background])
        custom-bg-url (r/cursor app-state [:options :custom-bg-url])
        labeled-unrezzed-cards (r/cursor app-state [:options :labeled-unrezzed-cards])
        labeled-cards (r/cursor app-state [:options :labeled-cards])]

    (go (while true
          (let [zoom (<! zoom-channel)]
            (swap! app-state assoc :zoom zoom))))

    (go (while true
          (let [button (<! button-channel)]
            (swap! app-state assoc :button button))))

    (r/create-class
      {:display-name "gameboard"

       :component-did-mount
       (fn [this]
         (-> js/document (.addEventListener
                           "keydown"
                           (partial handle-key-down {:render-board? (and @corp @runner @side true)})))
         (-> js/document (.addEventListener
                           "keyup"
                           (partial handle-key-up {:side side :active-player active-player :render-board? (and @corp @runner @side true)
                                                   :corp-phase-12 corp-phase-12 :runner-phase-12 runner-phase-12
                                                   :end-turn end-turn :run run
                                                   :encounters encounters})))
         (-> js/document (.addEventListener
                           "click"
                           (partial handle-click {:render-board? (and @corp @runner @side true)}))))

       :component-will-unmount
       (fn [this]
         (-> js/document (.addEventListener
                           "keydown"
                           (partial handle-key-down {:render-board? (and @corp @runner @side true)})))
         (-> js/document (.removeEventListener
                           "keyup"
                           (partial handle-key-up {:side side :active-player active-player :render-board? (and @corp @runner @side true)
                                                   :corp-phase-12 corp-phase-12 :runner-phase-12 runner-phase-12
                                                   :end-turn end-turn :run run
                                                   :encounters encounters})))
         (-> js/document (.addEventListener
                           "click"
                           (partial handle-click {:render-board? (and @corp @runner @side true)}))))

       :reagent-render
       (fn []
        (when (and @corp @runner @side true)
           (let [me-side (if (= :spectator @side)
                          (or (spectate-side) :corp)
                          @side)
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
                 prompt-state (r/cursor game-state [me-side :prompt-state])
                 ;; identity cards
                 me-ident (r/cursor game-state [me-side :identity])
                 op-ident (r/cursor game-state [op-side :identity])
                 ;; score areas
                 me-scored (r/cursor game-state [me-side :scored])
                 op-scored (r/cursor game-state [op-side :scored])
                 me-agenda-point (r/cursor game-state [me-side :agenda-point])
                 op-agenda-point (r/cursor game-state [op-side :agenda-point])
                 me-agenda-point-req (r/cursor game-state [me-side :agenda-point-req])
                 op-agenda-point-req (r/cursor game-state [op-side :agenda-point-req])
                 ;; servers
                 corp-servers (r/cursor game-state [:corp :servers])
                 runner-rig (r/cursor game-state [:runner :rig])
                 sfx (r/cursor game-state [:sfx])]
             [:div.gameview
              [:div {:class [:gameboard
                             (when @labeled-unrezzed-cards :show-unrezzed-card-labels)
                             (when @labeled-cards :show-card-labels)]}
               (let [me-keep (r/cursor game-state [me-side :keep])
                     op-keep (r/cursor game-state [op-side :keep])
                     me-quote (r/cursor game-state [me-side :quote])
                     op-quote (r/cursor game-state [op-side :quote])]
                 [build-start-box me-ident me-user me-hand prompt-state me-keep op-ident op-user op-keep me-quote op-quote side])

               [build-decks-box game-state]
               [build-win-box game-state]

               [:div {:class (if (:replay @game-state)
                               (case @replay-side
                                 :runner (get-in @game-state [:runner :user :options :background] "lobby-bg")
                                 :corp (get-in @game-state [:corp :user :options :background] "lobby-bg")
                                 :spectator @background)
                               @background)
                         :style (if (= @background "custom-bg")
                                  {:background (str "url(\"" @custom-bg-url "\")")
                                   :background-size "cover"}
                                  {})}]

               [:div.right-pane
                [card-zoom-view zoom-card]

                (if (:replay @game-state)
                  [content-pane :log :settings :notes :notes-shared]
                  [content-pane :log :settings :run-timing :turn-timing])]

               [:div.centralpane
                (if (= op-side :corp)
                  [board-view-corp me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard corp-servers run]
                  [board-view-runner me-side op-ident op-deck op-deck-count op-hand op-hand-count op-discard runner-rig run corp-servers])
                (if (= me-side :corp)
                  [board-view-corp me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard corp-servers run]
                  [board-view-runner me-side me-ident me-deck me-deck-count me-hand me-hand-count me-discard runner-rig run corp-servers])]

               [:div.leftpane
                [:div.opponent
                 [hand-view op-side op-hand op-hand-size op-hand-count (atom nil) (= @side :spectator)]]

                [:div.inner-leftpane
                 [:div.left-inner-leftpane
                  [:div
                   [stats-view opponent]
                   [scored-view op-scored op-agenda-point op-agenda-point-req false]]
                  [:div
                   [scored-view me-scored me-agenda-point me-agenda-point-req true]
                   [stats-view me]]]

                 [:div.right-inner-leftpane
                  (let [op-rfg (r/cursor game-state [op-side :rfg])
                        op-set-aside (r/cursor game-state [op-side :set-aside])
                        op-destroyed (r/cursor game-state [op-side :destroyed])
                        op-current (r/cursor game-state [op-side :current])
                        op-play-area (r/cursor game-state [op-side :play-area])
                        last-revealed (r/cursor game-state [:last-revealed])
                        me-rfg (r/cursor game-state [me-side :rfg])
                        me-set-aside (r/cursor game-state [me-side :set-aside])
                        me-destroyed (r/cursor game-state [me-side :destroyed])
                        me-current (r/cursor game-state [me-side :current])
                        me-play-area (r/cursor game-state [me-side :play-area])]
                    [:div
                     (when-not (:replay @game-state)
                       [starting-timestamp @start-date @timer])
                     [rfg-view op-rfg [:game_rfg "Removed from the game"] true]
                     [rfg-view me-rfg [:game_rfg "Removed from the game"] true]
                     [rfg-view op-set-aside [:game_set-aside "Set aside"] false]
                     [rfg-view me-set-aside [:game_set-aside "Set aside"] false]
                     [rfg-view op-destroyed [:game_destroyed "Destroyed"] false]
                     [rfg-view me-destroyed [:game_destroyed "Destroyed"] false]
                     [play-area-view op-user [:game_play-area "Play Area"] op-play-area]
                     [play-area-view me-user [:game_play-area "Play Area"] me-play-area]
                     [rfg-view op-current [:game_current "Current"] false]
                     [rfg-view me-current [:game_current "Current"] false]
                     [rfg-view last-revealed [:game_last-revealed "Last Revealed"] false true]])
                  (when (or (not= @side :spectator)
                            (and (spectator-view-hidden?) (spectate-side)))
                    [button-pane {:side me-side :active-player active-player :run run :encounters encounters
                                  :end-turn end-turn :runner-phase-12 runner-phase-12
                                  :corp-phase-12 corp-phase-12 :corp corp :runner runner
                                  :runner-post-discard runner-post-discard
                                  :corp-post-discard corp-post-discard
                                  :me me :opponent opponent :prompt-state prompt-state}])]]

                [:div.me
                 [hand-view me-side me-hand me-hand-size me-hand-count prompt-state true me-discard]]]]
              (when (:replay @game-state)
                [:div.bottompane
                 [replay-panel]])])))})))

(defonce sfx (r/track #(select-keys @game-state [:sfx :sfx-current-id])))
(defonce trigger-sfx (r/track! #(update-audio @sfx)))
