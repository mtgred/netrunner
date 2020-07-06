(ns nr.admin
  (:require [reagent.core :as r]
            [nr.appstate :refer [app-state]]))

(defn parse-id
  [value]
  (let [link (first (next (re-find #"(?:tournaments/)(\d+)" value)))
        number (re-find #"\d+" value)]
    (or link number)))

(defn tournament []
  (let [state (r/atom {:value ""
                       :id nil
                       :link ""
                       :printing ""})
        value (r/cursor state [:value])
        id (r/cursor state [:id])
        link (r/cursor state [:link])
        printing (r/cursor state [:printing])
        ]
    (fn []
      [:div
       [:div.reset-bg]
       [:div.panel.blue-shade.reset-form
        [:h3 "Tournament"]
        [:label
         "Cobr.ai tournament link"
         [:input {:type "text"
                  :name "tournament-link"
                  :value @value
                  :on-change #(swap! state assoc :value (-> % .-target .-value))}]
         [:button {:on-click #(when-let [v (parse-id @value)]
                                (swap! state assoc :id v :value v))}
          "Parse link"]
         [:p#link (when @id (str "http://cobr.ai/tournaments/" @id ".json"))]
         [:button {:on-click #(swap! state assoc :printing "Loading...")}
          "Load tournament"]
         [:h3#printing @printing]
         ]]])))

(defn admin []
  (let [user (r/cursor app-state [:user])
        active (r/cursor app-state [:active-page])]
    (when (and @user (= "/admin" (first @active)))
      [tournament])))
