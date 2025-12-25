(ns nr.gameboard.player-stats
  (:require
   [clojure.string :as s :refer [capitalize lower-case]]
   [nr.appstate :refer [app-state]]
   [nr.avatar :refer [avatar]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.state :refer [game-state not-spectator?]]
   [nr.translations :refer [tr tr-span tr-element tr-pronouns]]
   [reagent.core :as r]))

(defn stat-controls
  "Create an overlay to increase/decrease a player attribute (e.g. credits)."
  ([key content] (stat-controls key 1 -1 content))
  ([key increment decrement content]
   (if (not-spectator?)
     [:div.stat-controls
      content
      [:div.controls
       [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
       [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]]]
     content)))

(defn- stat-controls-for-side
  [side]
  (if (= (:side @game-state) side)
    stat-controls
    (fn [key content] content)))

(defn- name-area
  [user]
  [:div.name-area [avatar user {:opts {:size 32}}]
   [:div.name-box
    [:div.username (:username user)]
    (if-let [pronouns (get-in user [:options :pronouns])]
      (let [pro-str (if (= "blank" pronouns) "" (tr-pronouns pronouns))]
        [:div.pronouns (lower-case pro-str)]))]])

(defn- display-memory
  ([memory] (display-memory memory false))
  ([memory icon?]
   (let [ctrl (stat-controls-for-side :runner)
         {:keys [available used]} memory
         unused (- available used)
         label (if icon? [:<> unused "/" available " " [:span.anr-icon.mu]]
                   [tr-span [:game_mu-count] {:unused unused :available available}])]
     (ctrl :memory [:div label (when (neg? unused) [:div.warning "!"])]))))

(defn- display-special-memory
  ([memory] (display-special-memory memory false))
  ([memory icon?]
   (when-let [only-for (->> (:only-for memory)
                            (filter #(pos? (:available (second %))))
                            (into {})
                            not-empty)]
     [:<>
      (doall
       (for [[mu-type {:keys [available used]}] only-for
             :let [unused (max 0 (- available used))
                   mu-type-name (capitalize (name mu-type))]]
         ^{:key mu-type-name}
         [:div (if icon? [:<> unused "/" available " " mu-type-name " " [:span.anr-icon.mu]]
                   [tr-span [:game_special-mu-count]
                    {:unused unused
                     :available available
                     :mu-type mu-type-name}])]))])))

(defmulti stats-area
  (fn [player] (get-in @player [:identity :side])))

(defmethod stats-area "Runner" [runner]
  (let [ctrl (stat-controls-for-side :runner)]
    (fn []
      (let [{:keys [click credit run-credit memory link tag brain-damage]} @runner
            base-credit (- credit run-credit)
            plus-run-credit (when (pos? run-credit) (str "+" run-credit))
            icons? (get-in @app-state [:options :player-stats-icons] true)]
        [:div.stats-area
         (if icons?
           [:<>
            [:div.icon-grid
             (ctrl :click [:div click " " [:span.anr-icon.click]])
             (ctrl :credit [:div base-credit plus-run-credit " " [:span.anr-icon.credit]])
             [display-memory memory true]
             (ctrl :link [:div link " " [:span.anr-icon.link]])]
            [display-special-memory memory true]]
           [:<>
            (ctrl :click [:div [tr-span [:game_click-count] {:click click}]])
            (ctrl :credit [:div (if (pos? run-credit)
                                  [tr-span [:game_credit-count-with-run-credits]
                                   {:credit credit
                                    :run-credit run-credit}]
                                  [tr-span [:game_credit-count] {:credit credit}])])
            [display-memory memory]
            [display-special-memory memory]
            (ctrl :link [:div (str link " " [tr-span [:game_link-strength "Link Strength"]])])])
         (let [{:keys [base total is-tagged]} tag
               additional (- total base)
               show-tagged (or is-tagged (pos? total))]
           (ctrl :tag [:div (if (pos? additional)
                              [tr-span [:game_tag-count-additional]
                               {:base base
                                :additional additional
                                :total total}]
                              [tr-span [:game_tag-count] {:base base}])
                       (when show-tagged [:div.warning "!"])]))
         (ctrl
          :brain-damage
          [tr-element :div [:game_brain-damage "Core Damage"] {:dmg brain-damage}])]))))

(defmethod stats-area "Corp" [corp]
  (let [ctrl (stat-controls-for-side :corp)]
    (fn []
      (let [{:keys [click credit bad-publicity active]} @corp
            icons? (get-in @app-state [:options :player-stats-icons] true)]
        [:div.stats-area
         (if icons?
           [:div.icon-grid
            (ctrl :click [:div click " " [:span.anr-icon.click]])
            (ctrl :credit [:div credit " " [:span.anr-icon.credit]])]
           [:<>
            (ctrl :click [tr-element :div [:game_click-count] {:click click}])
            (ctrl :credit [tr-element :div [:game_credit-count] {:credit credit}])])
         (let [{:keys [base additional]} bad-publicity]
           (ctrl :bad-publicity [:div (if-not (pos? additional)
                                        [tr-span [:game_bad-pub-count] {:base base}]
                                        [tr-span [:game_bad-pub-count-additional]
                                         {:base base
                                          :additional additional}])]))]))))

(defn- really-my-side?
  [side]
  (and
    (= (:side @game-state) (keyword (lower-case side)))
    (not-spectator?)))

(defn- tabs [selected-tab set-tab!]
  [:div.panel-bot.selector.options-tabs
   [:div.tab
    [:a {:key :stats
         :on-click #(set-tab! :stats)}
     [:label "Stats"]]]
   [:div.tab.right
    [:a {:key :options
         :on-click #(set-tab! :options)}
     [:label "Gameplay Options"]]]])

(defn- render-player-panel
  [player]
  (fn []
    [:<>
     (name-area (:user @player))
     ;; note: react doesn't like defmulti much, and caches the first hit
     ;; when it redoes stats - so it needs a key to re-render if sides
     ;; change (ie playing a corp game after playing a runner game) - nbk
     ^{:key (get-in @player [:identity :side])} [stats-area player]]))

(defn- gameplay-boolean
  [player target-side key tr-key tr-text]
  (fn []
    (let [val (get-in @player [:properties key] false)]
      (when (or (not target-side)
                (= target-side (:side @game-state)))
        [:div [:label [:input {:type "checkbox"
                               :key key
                               :checked val
                               :on-change #(send-command "set-property" {:key key :value (.. % -target -checked)})}]
               [tr-span [tr-key tr-text]]]]))))

(defn- in-game-options
  [player]
  (fn []
    [:div.in-game-options
     [tr-element :h4 [:gameplay-options_gameplay-options "General Gameplay Options"]]
     [gameplay-boolean player nil :trash-like-cards :game_trash-like-cards "Offer to trash like cards"]
     [gameplay-boolean player :corp :auto-purge :game_auto-purge-smart "CSV/Mavirus Auto-purge"]

     ;; Corp Side: auto-purge with mavirus/csv
     [tr-element :h4 [:gameplay-options_timing-windows "Timing Windows"]]

     ;; corp perspective window forcing
     [gameplay-boolean player :corp :force-phase-12-self :game_force-phase-12-self-corp "Corp pre-draw PAW"]
     [gameplay-boolean player :corp :force-phase-12-opponent :game_force-phase-12-opponent-corp "Runner turn-begins PAW"]
     [gameplay-boolean player :corp :force-post-discard-self :game_force-post-discard-self-corp "Corp post-discard PAW"]
     [gameplay-boolean player :corp :force-post-discard-opponent :game_force-post-discard-opponent-corp "Runner post-discard PAW"]

     ;; runner perspective window forcing
     [gameplay-boolean player :runner :force-phase-12-self :game_force-phase-12-self-runner "Runner turn-begins PAW"]
     [gameplay-boolean player :runner :force-phase-12-opponent :game_force-phase-12-opponent-runner "Corp pre-draw PAW"]
     [gameplay-boolean player :runner :force-post-discard-self :game_force-post-discard-self-runner "Runner post-discard PAW"]
     [gameplay-boolean player :runner :force-post-discard-opponent :game_force-post-discard-opponent-runner "Corp post-discard PAW"]

     ]))

(defn stats-view
  [player]
  (let [selected-tab (r/atom :stats)]
    (fn []
      (let [show-tabs? (really-my-side? (get-in @player [:identity :side]))
            set-tab! #(reset! selected-tab %)]
        [:div.panel.blue-shade.stats {:class (when (:active @player) "active-player")}
         (if-not show-tabs?
           [render-player-panel player]

           [:<> (case @selected-tab
                  :stats [render-player-panel player]
                  :options [in-game-options player])
            [tabs selected-tab set-tab!]])]))))
