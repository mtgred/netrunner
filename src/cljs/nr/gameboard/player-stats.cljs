(ns nr.gameboard.player-stats
  (:require [clojure.string :as s :refer [capitalize join lower-case]]
            [nr.avatar :refer [avatar]]
            [nr.gameboard.actions :refer [send-command]]
            [nr.gameboard.state :refer [game-state]]
            [nr.translations :refer [tr tr-pronouns]]))

(defn stat-controls
  "Create an overlay to increase/decrease a player attribute (e.g. credits)."
  ([key content] (stat-controls key 1 -1 content))
  ([key increment decrement content]
   [:div.stat-controls
    content
    [:div.controls
     [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
     [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]]]))

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

(defmulti stats-view #(get-in @% [:identity :side]))

(defn- display-memory
  [memory]
  (let [ctrl (stat-controls-for-side :runner)]
    (fn [memory]
      (let [{:keys [available used only-for]} memory
            unused (- available used)]
        (ctrl :memory [:div (tr [:game.mu-count] unused available)
                       (when (neg? unused) [:div.warning "!"])])))))

(defn- display-special-memory
  [memory]
  (when-let [only-for (->> (:only-for memory)
                           (filter #(pos? (:available (second %))))
                           (into {})
                           not-empty)]
    [:div
     (str "("
          (join "), (" (for [[mu-type {:keys [available used]}] only-for
                             :let [unused (max 0 (- available used))]]
                         (str unused " of " available
                              " " (capitalize (name mu-type))
                              " MU unused")))
          ")")]))

(defmethod stats-view "Runner" [runner]
  (let [ctrl (stat-controls-for-side :runner)]
    (fn [runner]
      (let [{:keys [user click credit run-credit memory link tag
                    brain-damage active]} @runner]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         (name-area user)
         (ctrl :click [:div (tr [:game.click-count] click)])
         (ctrl :credit [:div (tr [:game.credit-count] credit run-credit)])
         [display-memory memory]
         [display-special-memory memory]
         (ctrl :link [:div (str link " " (tr [:game.link-strength "Link Strength"]))])
         (let [{:keys [base total is-tagged]} tag
               additional (- total base)
               show-tagged (or is-tagged (pos? total))]
           (ctrl :tag [:div (tr [:game.tag-count] base additional total)
                       (when show-tagged [:div.warning "!"])]))
         (ctrl
          :brain-damage
          [:div (str brain-damage " " (tr [:game.brain-damage "Brain Damage"]))])]))))

(defmethod stats-view "Corp" [corp]
  (let [ctrl (stat-controls-for-side :corp)]
    (fn [corp]
      (let [{:keys [user click credit bad-publicity active]} @corp]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         (name-area user)
         (ctrl :click [:div (tr [:game.click-count] click)])
         (ctrl :credit [:div (tr [:game.credit-count] credit -1)])
         (let [{:keys [base additional]} bad-publicity]
           (ctrl :bad-publicity [:div (tr [:game.bad-pub-count] base additional)]))]))))
