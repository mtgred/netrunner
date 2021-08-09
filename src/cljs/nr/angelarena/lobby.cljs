(ns nr.angelarena
  (:require [clojure.string :refer [join]]
            [nr.appstate :refer [app-state]]
            [nr.cardbrowser :refer [image-url]]
            [nr.deckbuilder :refer [deck-entry deck-name deck-date]]
            [nr.translations :refer [tr tr-side tr-format]]
            [nr.utils :refer [num->percent cond-button]]
            [reagent.core :as r]
            [reagent-modals.modals :as reagent-modals]))

(defn lobby-buttons []
  [:div.lobby-buttons
   "hi"])

(defn- deck-view [deck user]
  [:div.deck
   [:img {:src (image-url (:identity deck))
          :alt (get-in deck [:identity :title] "")}]
   [:h4 (deck-name deck)]
   [:div (get-in deck [:identity :title])]
   [:div.result.float-right (if (= (str (:_id deck)) "6083c9b9c25a4115d2d10f8a") "Current result: 4-1" "Current result: 2-2")]
   [:div.time (if (= (str (:_id deck)) "6083c9b9c25a4115d2d10f8a") "Time left: 2 days 4 hours" "Time left: 1 day 14 hours")]])

(defn- deck-button-bar [gameid games user]
  (r/with-let [abandon (r/atom false)]
    [:div.button-bar
     [cond-button (tr [:lobby.new-game "Play game"])
      (and (not @gameid)
           (->> @games
                (mapcat :players)
                (filter #(= (-> % :user :_id) (:_id @user)))
                empty?))
      #(do (println "Starting game"))]
     (if @abandon
       [:span "Are you sure? "
        [:button.small {:on-click #(println "Abandoning run")} "yes"]
        [:button.small {:on-click #(reset! abandon false)} "no"]]
       [:button {:on-click #(reset! abandon true)} "Abandon run"])]))

(defn deckselect-modal [user {:keys [side gameid games decks format]}]
  [:div
    [:h3 (tr [:lobby.select-title "Select your deck"])]
    [:div.deck-collection.lobby-deck-selector
     (let [same-side? (fn [deck] (= side (get-in deck [:identity :side])))]
       [:div
        (doall
          (for [deck (->> @decks
                          (filter same-side?)
                          (sort-by :date >))]
            ^{:key (:_id deck)}
            [:div.deckline {:on-click #(do (println "Selecting...")
                                           (reagent-modals/close-modal!))}
             [:img {:src (image-url (:identity deck))
                    :alt (get-in deck [:identity :title] "")}]
             [:h4 (:name deck)]
             [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
             [:p (get-in deck [:identity :title])]]))])]])

(defn- new-run-button-bar [side decks s games gameid sets user]
  [:div.button-bar
   [cond-button (tr [:lobby.start-new-run "Start new run"])
    (and (not @gameid)
         (->> @games
              (mapcat :players)
              (filter #(= (-> % :user :_id) (:_id @user)))
              empty?))
    #(reagent-modals/modal!
       [deckselect-modal user {:games games :gameid gameid
                               :sets sets :decks decks
                               :side side :format nil}])]])

(defn game-panel [decks s games gameid sets user]
  (let [run (or (:angelarena-run @user) {:corp nil :runner nil})]
    [:div.game-panel.angelarena
     [:h3 "Current corp run"]
     (if true;(:corp run)
       [:div
        [deck-view (first (filter #(= (str (:_id %)) "6083c9b9c25a4115d2d10f8a") @decks)) user]
        [deck-button-bar gameid games user]]
       [new-run-button-bar "Corp" decks s games gameid sets user])

     [:h3 "Current runner run"]
     (if (:runner run)
       [:div
        [deck-view (first (filter #(= (str (:_id %)) "608bef4bc25a4150d84167dc") @decks)) user]
        [deck-button-bar gameid games user]]
       [new-run-button-bar "Runner" decks s games gameid sets user])

     [:h3 "Latest runs"]
     ]))
