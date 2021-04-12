(ns nr.gameboard.player-stats
  (:require [clojure.string :as s :refer [capitalize join lower-case]]
            [nr.avatar :refer [avatar]]
            [nr.gameboard.actions :refer [send-command]]
            [nr.gameboard.state :refer [game-state]]
            [nr.translations :refer [tr tr-pronouns]]))

(defn controls
  "Create the control buttons for the side displays."
  ([key] (controls key 1 -1))
  ([key increment decrement]
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
    [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]]))

(defn- name-area
  [user]
  [:div.namearea [avatar user {:opts {:size 32}}]
   [:div.namebox
    [:div.username (:username user)]
    (if-let [pronouns (get-in user [:options :pronouns])]
      (let [pro-str (if (= "blank" pronouns) "" (tr-pronouns pronouns))]
        [:div.pronouns (lower-case pro-str)]))]])

(defmulti stats-view #(get-in @% [:identity :side]))

(defn- display-memory
  [memory]
  (let [me? (= (:side @game-state) :runner)]
    (fn [memory]
      (let [{:keys [available used only-for]} memory
            unused (- available used)]
        [:div (tr [:game.mu-count] unused available)
         (when (neg? unused) [:div.warning "!"])
         (when me? (controls :memory))]))))

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
  (let [me? (= (:side @game-state) :runner)]
    (fn [runner]
      (let [{:keys [user click credit run-credit memory link tag
                    brain-damage active]} @runner]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         (name-area user)
         [:div (tr [:game.click-count] click)
          (when me? (controls :click))]
         [:div (tr [:game.credit-count] credit run-credit)
          (when me? (controls :credit))]
         [display-memory memory]
         [display-special-memory memory]
         [:div (str link " " (tr [:game.link-strength "Link Strength"]))
          (when me? (controls :link))]
         (let [{:keys [base total is-tagged]} tag
               additional (- total base)
               show-tagged (or is-tagged (pos? total))]
           [:div (tr [:game.tag-count] base additional total)
            (when show-tagged [:div.warning "!"])
            (when me? (controls :tag))])
         [:div (str brain-damage " " (tr [:game.brain-damage "Brain Damage"]))
          (when me? (controls :brain-damage))]]))))

(defmethod stats-view "Corp" [corp]
  (let [me? (= (:side @game-state) :corp)]
    (fn [corp]
      (let [{:keys [user click credit bad-publicity active]} @corp]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         (name-area user)
         [:div (tr [:game.click-count] click)
          (when me? (controls :click))]
         [:div (tr [:game.credit-count] credit -1)
          (when me? (controls :credit))]
         (let [{:keys [base additional]} bad-publicity]
           [:div (tr [:game.bad-pub-count] base additional)
            (when me? (controls :bad-publicity))])]))))
