(ns nr.tournament
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize]]
            [jinteki.cards :refer [all-cards]]
            [nr.ajax :refer [GET DELETE]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [authenticated] :as auth]
            [nr.avatar :refer [avatar]]
            [nr.deckbuilder :refer [num->percent]]
            [nr.end-of-game-stats :refer [build-game-stats]]
            [nr.player-view :refer [player-view]]
            [nr.utils :refer [faction-icon render-message notnum->zero]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defn change-cobra-link
  [state value]
  (swap! state dissoc :ready :response)
  (swap! state assoc :cobra-link value))

(defn parse-id
  [{value :cobra-link}]
  (let [link (first (next (re-find #"(?:tournaments/)(\d+)" value)))
        number (re-find #"\d+" value)]
    (or link number)))

(defn process-link
  [state]
  (go (let [{:keys [status json]} (<! (GET (str "/tournament-load/" (parse-id @state))))]
        (swap! state assoc :loaded true)
        (cond
          (= 200 status)
          (swap! state assoc :response "Tournament loaded")
          (= 401 status)
          (swap! state assoc :response (str (:message json) " " (:player-names json)))
          (= 403 status)
          (swap! state assoc :response (:message json))))))

(defn create-tables
  [state]
  (go (let [{:keys [status json]} (<! (GET (str "/tournament-create/" (parse-id @state))))]
        (println status)
        (println json)
        (when (= 200 status)
          (swap! state assoc :success true)))))

(defn tournament-container
  [state]
  [:div.panel
   [:div
    [:h3 "Tournament stuff"]
    [:input {:placeholder "cobr.ai tournament link"
             :type "text"
             :value (:cobra-link @state)
             :on-change #(change-cobra-link state (-> % .-target .-value))}]]
   [:div
    [:button {:on-click #(process-link state)} "Load tournament"]
    [:div (:response @state)]
    (when (:loaded @state)
      [:button {:on-click #(create-tables state)} "Create tables"])
    (when (:success @state)
      [:div (:response @state)]
      )
    ]])

(defn tournament []
  (r/with-let [user (r/cursor app-state [:user])
               active (r/cursor app-state [:active-page])
               state (r/atom {})]

    (go (let [{:keys [status json]} (<! (GET (str "/tournament-auth/" (:username @user))))]
          (when (= 200 status)
            (swap! state assoc :logged-in true))))

    (when (and (= "/tournament" (first @active))
               (:logged-in @state))
      [:div.container
       [:div.lobby.panel.blue-shade
        [tournament-container state]]])))
