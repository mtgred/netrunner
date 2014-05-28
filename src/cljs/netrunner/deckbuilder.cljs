(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.auth :as auth]
            [clojure.string :refer [split split-lines]]))

(def app-state (atom {:decks []}))

(defn deck [deck owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:h4 (:identity deck)]
        [:p (:name deck)]]))))

(defn new-deck [side owner]
  (om/set-state! owner :edit true)
  (-> owner (om/get-node "viewport") js/$ (.css "left" -477)))

(defn save-deck [deck owner]
  (om/set-state! owner :edit false)
  (-> owner (om/get-node "viewport") js/$ (.css "left" 0)))

(defn deck-builder [{:keys [decks]} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
       :card-search ""
       :quantity 1})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:h1 "Your decks"]
        [:div.deckbuilder.blue-shade.panel
         [:div.viewport {:ref "viewport"}
          [:div.decks
           [:p.button-bar
            [:button {:on-click #(new-deck :corp owner)} "New Corp deck"]
            [:button {:on-click #(new-deck :runner owner)} "New Runner deck"]]
           (if (empty? decks)
             [:h4 "You have no deck"]
             (om/build-all deck))]
          [:div.decklist
           [:h2.deck-name "Astrobiotic"]
           (if (:edit state)
             [:button {:on-click #(save-deck "" owner)} "Save"]
             [:button {:on-click #(new-deck :corp owner)} "Edit"])]
          [:div.deckedit
           [:p [:input {:type "text" :placeholder "Deck name" :value ""}]]
           [:p
            [:input {:type "text" :placeholder "Card" :value (:card-search state)}] " x "
            [:input.qty {:type "text" :value (:quantity state)}]
            [:button {:on-click #()} "Add"]]
           [:textarea]]]]]))))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})

(let [user (:user @auth/app-state)]
  (when-not (empty? user)
    (go (swap! app-state assoc :decks
               (:json (<! (GET (str "/data/decks/user/username/" (:username user)))))))))
