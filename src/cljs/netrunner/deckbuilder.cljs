(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.auth :as auth]))

(def app-state (atom {:decks []}))

(defn deck [deck owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:h4 (:identity deck)]
        [:p (:name deck)]]))))

(defn deck-builder [{:keys [decks]} owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (sab/html
       [:div.deckbuilder
        [:h1 "Deck Builder"]
        [:div.blue-shade.panel
         [:p.button-bar
          [:button {:on-click #()} "New Corp deck"]
          [:button {:on-click #()} "New Runner deck"]]
         [:div.decks
          (if (empty? decks)
            [:h4 "You have no deck"]
            (om/build-all deck))]]]))))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})

(let [user (:user @auth/app-state)]
  (when-not (empty? user)
    (go (swap! app-state assoc :decks
               (:json (<! (GET (str "/data/decks/user/username/" (:username user)))))))))
