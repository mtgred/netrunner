(ns nr.tournament
  (:require [nr.appstate :refer [app-state]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defn change-cobra-link
  [state value]
  (swap! state dissoc :cobra-link)
  (swap! state assoc :url value))

(defn cobra-link
  [state]
  [:div
   [:h3 "Public Cobr.ai tournament"]
   [:input {:placeholder "cobr.ai tournament link"
            :type "text"
            :value (:url @state)
            :on-change #(change-cobra-link state (-> % .-target .-value))}]])

(defn parse-id
  [{value :url}]
  (let [link (first (next (re-find #"(?:tournaments/)(\d+)" value)))
        number (re-find #"\d+" value)]
    (or link number)))

(defn process-link
  [state]
  (swap! state assoc :cobra-link (parse-id @state))
  (ws/ws-send! [:tournament/fetch {:cobra-link (:cobra-link @state)}]))

(defn load-tournament
  [state]
  [:button {:on-click #(process-link state)} "Load tournament"])

(defn missing-players
  [state]
  (when (:cobra-link @state)
    (let [players (:missing-players @state)]
      [:div
       [:h3 "Players in Cobra with no registered jnet accounts"]
       [:ul
        (doall
          (map-indexed
            (fn [idx player]
              ^{:key idx} [:li player])
            players))]])))

(defn round-selector
  [state]
  (when (:cobra-link @state)
    [:div
     [:h3 "Select round"]
     [:select {:value (or (:selected-round @state) (count (:rounds @state)))
               :on-change #(swap! state assoc :selected-round (.. % -target -value))}
      (doall
        (for [round (range 1 (inc (count (:rounds @state))))]
          ^{:key round}
          [:option {:value (dec round)}
           (str "Round " round)]))]]))

(defn create-tables
  [state]
  (when (:cobra-link @state)
    (ws/ws-send! [:tournament/create {:cobra-link (:cobra-link @state)
                                      :selected-round (:selected-round @state)}])))

(defn select-round-button
  [state]
  (when (:cobra-link @state)
    [:button {:on-click #(create-tables state)}
     "Create tables for this round"]))

(defn success
  [state]
  (when-let [results (:results @state)]
    [:h2 "Success!"]
    [:p (str results " tables have been created")]))

(defn tournament-container
  [state]
  [:div.panel
   [:h1 "Tournament stuff"]
   [cobra-link state]
   [load-tournament state]
   [missing-players state]
   [round-selector state]
   [select-round-button state]
   [success state]])

(defn load-players
  [state json]
  (swap! state merge (:data json)))

(defn show-results
  [state json]
  (swap! state assoc :results (get-in json [:data :created-rounds])))

(defn tournament []
  (r/with-let [user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])
               state (r/atom {})]

    (ws/register-ws-handler! :tournament/loaded #(load-players state %))
    (ws/register-ws-handler! :tournament/created #(show-results state %))

    (when (and (= "/tournament" (first @active))
               (:tournament-organizer @user))
      [:div.container
       [:div.lobby.panel.blue-shade
        [tournament-container state]]])))
