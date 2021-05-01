(ns nr.angelarena
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :refer [lower-case]]
            [nr.ajax :refer [GET]]
            [nr.appstate :refer [app-state]]
            [nr.cardbrowser :refer [image-url]]
            [nr.deckbuilder :refer [deck-entry deck-name deck-date]]
            [nr.translations :refer [tr tr-side tr-format]]
            [nr.utils :refer [num->percent cond-button]]
            [nr.ws :as ws]
            [reagent.core :as r]
            [reagent-modals.modals :as reagent-modals]))

(defn- fetch-current-run [current-run]
  (go (let [{:keys [status json]} (<! (GET "/profile/angelarena/run"))]
        (when (= 200 status)
          (reset! current-run (js->clj json))))))

(defn lobby-buttons []
  [:div.lobby-buttons
   "hi"])

(defn- time-delta-string [delta]
  (let [days (Math/floor (/ delta (* 1000 60 60 24)))
        delta (mod delta (* 1000 60 60 24))
        hours (Math/floor (/ delta (* 1000 60 60)))
        delta (mod delta (* 1000 60 60))
        minutes (Math/floor (/ delta (* 1000 60)))
        delta (mod delta (* 1000 60))
        seconds (Math/floor (/ delta 1000))]
    (cond
      (pos? days) (str days " days, " hours " hours")
      (pos? hours) (str hours " hours, " minutes " minutes")
      :else (str minutes " minutes, " seconds " seconds"))))

(defn- deck-view [side decks current-run]
  (r/with-let [side (keyword (lower-case side))
               deck (first (filter #(= (str (:_id %))
                                       (get-in @current-run [side :deckid]))
                                   @decks))
               run-info (side @current-run)
               time-since-start (- (js/Date.now) (js/Date.parse (:run-started run-info)))
               allowed-days (+ 3 (:wins run-info) (:losses run-info))]
    [:div.deck
     [:img {:src (image-url (:identity deck))
            :alt (get-in deck [:identity :title] "")}]
     [:h4 (deck-name deck)]
     [:div.result.float-right (str (:wins run-info) " wins")]
     [:div (get-in deck [:identity :title])]
     [:div.result.float-right (str (:losses run-info) " losses")]
     [:div.time (str "Time left: " (time-delta-string (- (* 1000 60 60 24 allowed-days)
                                                         time-since-start)))]]))

(defn- deck-button-bar [side gameid games user current-run]
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
        [:button.small {:on-click #(do (ws/ws-send! [:angelarena/abandon-run {:side side}])
                                       (fetch-current-run current-run))} "yes"]
        [:button.small {:on-click #(reset! abandon false)} "no"]]
       [:button {:on-click #(reset! abandon true)} "Abandon run"])]))

(defn deckselect-modal [user {:keys [side gameid games decks format]} current-run]
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
            [:div.deckline {:on-click #(do (ws/ws-send! [:angelarena/start-run
                                                         {:side side :deckid (:_id deck)}])
                                           (reagent-modals/close-modal!)
                                           (fetch-current-run current-run))}
             [:img {:src (image-url (:identity deck))
                    :alt (get-in deck [:identity :title] "")}]
             [:h4 (:name deck)]
             [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
             [:p (get-in deck [:identity :title])]]))])]])

(defn- new-run-button-bar [side decks s games gameid sets user current-run]
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
                               :side side :format nil}
        current-run])]])

(defn game-panel [decks s games gameid sets user]
  (r/with-let [current-run (r/atom nil)]
    (if-not @current-run
      (do
        (fetch-current-run current-run)
        [:div.game-panel.angelarena
         [:h3 "Requesting run data..."]])
      [:div.game-panel.angelarena
       [:h3 "Current corp run"]
       (if (:corp @current-run)
         [:div
          [deck-view "Corp" decks current-run]
          [deck-button-bar "Corp" gameid games user current-run]]
         [new-run-button-bar "Corp" decks s games gameid sets user current-run])

       [:h3 "Current runner run"]
       (if (:runner @current-run)
         [:div
          [deck-view "Runner" decks current-run]
          [deck-button-bar "Runner" gameid games user current-run]]
         [new-run-button-bar "Runner" decks s games gameid sets user current-run])

       [:h3 "Latest runs"]])))
