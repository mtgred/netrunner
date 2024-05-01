(ns nr.gameboard.player-stats-alternate
  (:require
   [clojure.string :as s :refer [capitalize lower-case]]
   [nr.appstate :refer [app-state]]
   [nr.avatar :refer [avatar]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.state :refer [game-state not-spectator?]]
   [nr.translations :refer [tr tr-pronouns]]
   [reagent.core :as r]))

(defn stat-controls
  "Create an overlay to increase/decrease a player attribute (e.g. credits)."
  ([key content] (stat-controls key 1 -1 content))
  ([key increment decrement content]
   (if (not-spectator?)
     [:div.stat-controls 
      [:span.minus {:on-click #(send-command "change" {:key key :delta decrement})}]
      content
      [:span.plus {:on-click #(send-command "change" {:key key :delta increment})}]]
     content)))

(defn stat-controls-for-side
  [side]
  (if (= (:side @game-state) side)
    stat-controls
    (fn [key content] content)))

(defn name-area
  [side player]
  (let [user (:user @player)
        ctrl (stat-controls-for-side side)
        {:keys [agenda-point]} @player] 
    [:div.name-area 
     [avatar user {:opts {:size 32}}] 
     [:div.name-box 
      [:div.username (:username user)] 
      (when-let [pronouns (get-in user [:options :pronouns])] 
        (let [pro-str (if (= "blank" pronouns) "" (tr-pronouns pronouns))] 
          [:div.pronouns (lower-case pro-str)]))]]))

(defn- display-memory
  ([memory] (display-memory memory false))
  ([memory icon?]
   (let [ctrl (stat-controls-for-side :runner)]
     (fn [memory]
       (let [{:keys [available used only-for]} memory
             unused (- available used)
             label (if icon? [:span unused "/" available " " [:span.anr-icon.mu]]
                       (tr [:game.mu-count] unused available))]
         [:div {:class [(when (neg? unused) "warning-text") "memory"]} label])))))

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
         [:div (if icon? [:span unused "/" available " " mu-type-name " " [:span.anr-icon.mu]]
                   (tr [:game.special-mu-count] unused available mu-type-name))]))])))

(defmulti stats-area
  (fn [player] (get-in @player [:identity :side])))

(defmethod stats-area "Runner" [runner]
  (let [ctrl (stat-controls-for-side :runner)
        expanded (r/atom false)]
    (fn [runner]
      (let [{:keys [click credit run-credit link tag 
                    brain-damage memory]} @runner
            base-credit (- credit run-credit)
            {:keys [base total is-tagged]} tag
            additional (- total base)
            show-tagged (or is-tagged (pos? total))
            plus-run-credit (when (pos? run-credit) (str "+" run-credit))
            icons? (get-in @app-state [:options :player-stats-icons] true)]
        [:<>
         [:div.stats-area.minimal
          (if icons?
             [:div.icon-grid
              (ctrl :click [:div [:span.number-spin-animation {:style {"--num" click}} " " [:span.anr-icon.click]]])
              (ctrl :credit [:div [:span.number-spin-animation {:style {"--num" base-credit}} plus-run-credit " " [:span.anr-icon.credit {:style {"--num" base-credit}}]]])
              (if show-tagged 
                (ctrl :tag [:div.tags.warning-text (tr [:game.tag-count] base additional total)])
                (ctrl :memory [display-memory memory true]))]
            [:div.text-grid 
             (ctrl :click [:div (tr [:game.click-count] click)])
             (ctrl :credit [:div (tr [:game.credit-count] credit run-credit)])
             (if show-tagged [:div.tags.warning-text (tr [:game.tag-count] base additional total)] [display-memory memory true])])]
         (when @expanded [:div.stats-area
          (if icons?
            [:div.icon-grid
             (if show-tagged (ctrl :memory [display-memory memory true]) (ctrl :tag [:div {:class [(when show-tagged "warning-text")]} (tr [:game.tag-count] base additional total)]))
             [display-special-memory memory true]
             (ctrl :link [:div [:span link " " [:span.anr-icon.link]]])
             (ctrl
              :brain-damage
              [:div (str brain-damage " " (tr [:game.brain-damage "Core Damage"]))])]
            [:div.text-grid
               (if show-tagged
                 (ctrl :memory [display-memory memory])
                 (ctrl :tag [:div {:class [(when show-tagged "warning-text")]} (tr [:game.tag-count] base additional total)]))
             [display-special-memory memory]
             (ctrl :link [:div (str link " " (tr [:game.link-strength "Link Strength"]))])
             (ctrl
              :brain-damage
              [:div (str brain-damage " " (tr [:game.brain-damage "Core Damage"]))])])])
          [:div.expander.pointer {:on-click #(swap! expanded not)} (if @expanded [:div.chevron-up] [:div.chevron-down])]]))))  

(defmethod stats-area "Corp" [corp]
  (let [ctrl (stat-controls-for-side :corp)]
    (fn [corp]
      (let [{:keys [click credit bad-publicity]} @corp
            {:keys [base additional]} bad-publicity
            icons? (get-in @app-state [:options :player-stats-icons] true)]
        [:div.stats-area.minimal
         (if icons?
           [:div.icon-grid
            (ctrl :click [:div [:span.number-spin-animation {:style {"--num" click}} " " [:span.anr-icon.click]]])
            (ctrl :credit [:div [:span.number-spin-animation {:style {"--num" credit}} " " [:span.anr-icon.credit]]])
            (ctrl :bad-publicity [:div.bad-pub (tr [:game.bad-pub-count] base additional)])]
           [:div.text-grid
            (ctrl :click [:div (tr [:game.click-count] click)])
            (ctrl :credit [:div (tr [:game.credit-count] credit -1)])
            (ctrl :bad-publicity [:div.bad-pub (tr [:game.bad-pub-count] base additional)])])]))))
