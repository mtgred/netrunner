(ns nr.gameboard.player-stats
  (:require
   [clojure.string :as s :refer [capitalize lower-case]]
   [nr.appstate :refer [app-state]]
   [nr.avatar :refer [avatar]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.state :refer [game-state not-spectator?]]
   [nr.translations :refer [tr tr-pronouns]]))

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
   (let [ctrl (stat-controls-for-side :runner)]
     (fn [memory]
       (let [{:keys [available used only-for]} memory
             unused (- available used)
             label (if icon? [:<> unused "/" available " " [:span.anr-icon.mu]]
                       (tr [:game_mu-count] {:unused unused :available available}))]
         (ctrl :memory [:div label (when (neg? unused) [:div.warning "!"])]))))))

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
                   (tr [:game_special-mu-count]
                       {:unused unused
                        :available available
                        :mu-type mu-type-name}))]))])))

(defmulti stats-area
  (fn [player] (get-in @player [:identity :side])))

(defmethod stats-area "Runner" [_runner]
  (let [ctrl (stat-controls-for-side :runner)]
    (fn [runner]
      (let [{:keys [click credit run-credit memory link tag trash-like-cards brain-damage]} @runner
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
            (ctrl :click [:div (tr [:game_click-count] {:click click})])
            (ctrl :credit [:div (if (pos? run-credit)
                                  (tr [:game_credit-count-with-run-credits]
                                      {:credit credit
                                       :run-credit run-credit})
                                  (tr [:game_credit-count] {:credit credit}))])
            [display-memory memory]
            [display-special-memory memory]
            (ctrl :link [:div (str link " " (tr [:game_link-strength "Link Strength"]))])])
         (let [{:keys [base total is-tagged]} tag
               additional (- total base)
               show-tagged (or is-tagged (pos? total))]
           (ctrl :tag [:div (if (pos? additional)
                              (tr [:game_tag-count-additional]
                                  {:base base
                                   :additional additional
                                   :total total})
                              (tr [:game_tag-count] {:base base}))
                       (when show-tagged [:div.warning "!"])]))
         (ctrl
          :brain-damage
          [:div (tr [:game_brain-damage "Core Damage"] {:dmg brain-damage})])
         (when (= (:side @game-state) :runner)
           (let [toggle-offer-trash #(send-command "set-property" {:key :trash-like-cards :value (.. % -target -checked)})]
             [:div [:label [:input {:type "checkbox"
                                    :value true
                                    :checked trash-like-cards
                                    :on-click toggle-offer-trash}]
                    (tr [:game_trash-like-cards "Offer to trash like cards"])]]))]))))

(defmethod stats-area "Corp" [corp]
  (let [ctrl (stat-controls-for-side :corp)]
    (fn [corp]
      (let [{:keys [click credit bad-publicity active trash-like-cards]} @corp
            icons? (get-in @app-state [:options :player-stats-icons] true)]
        [:div.stats-area
         (if icons?
           [:div.icon-grid
            (ctrl :click [:div click " " [:span.anr-icon.click]])
            (ctrl :credit [:div credit " " [:span.anr-icon.credit]])]
           [:<>
            (ctrl :click [:div (tr [:game_click-count] {:click click})])
            (ctrl :credit [:div (tr [:game_credit-count] {:credit credit})])])
         (let [{:keys [base additional]} bad-publicity]
           (ctrl :bad-publicity [:div (if-not (pos? additional)
                                        (tr [:game_bad-pub-count] {:base base})
                                        (tr [:game_bad-pub-count-additional]
                                            {:base base
                                             :additional additional}))]))
         (when (= (:side @game-state) :corp)
           (let [toggle-offer-trash #(send-command "set-property" {:key :trash-like-cards :value (.. % -target -checked)})]
             [:div [:label [:input {:type "checkbox"
                                    :value true
                                    :checked trash-like-cards
                                    :on-click toggle-offer-trash}]
                    (tr [:game_trash-like-cards "Offer to trash like cards"])]]))]))))

(defn stats-view
  [player]
  (fn [player]
    [:div.panel.blue-shade.stats {:class (when (:active @player) "active-player")}
     (name-area (:user @player))
     ;; note: react doesn't like defmulti much, and caches the first hit
     ;; when it redoes stats - so it needs a key to re-render if sides
     ;; change (ie playing a corp game after playing a runner game) - nbk
     ^{:key (get-in @player [:identity :side])} [stats-area player]]))
