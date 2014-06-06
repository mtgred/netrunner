(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.auth :as auth]
            [netrunner.cardbrowser :as cb]
            [netrunner.ajax :refer [POST GET]]
            [netrunner.deck :refer [parse-deck]]))

(def app-state (atom {:decks []}))

(defn edit-deck [owner]
  (om/set-state! owner :edit true)
  (-> owner (om/get-node "viewport") js/$ (.css "left" -477)))

(defn new-deck [side owner]
  (om/set-state! owner :side side)
  (om/set-state! owner :name "New deck")
  (om/set-state! owner :cards [])
  (om/set-state! owner :identity "")
  (edit-deck owner))

(defn save-deck [owner]
  (om/set-state! owner :edit false)
  (-> owner (om/get-node "viewport") js/$ (.css "left" 0))
  ;; (let [params (-> e .-target js/$ )]
  ;;   (go (let [response (<! (POST "/data/deck/new" params))])))
  )

(defn handle-edit [event owner]
  (let [deck (-> owner (om/get-node "deck-edit") .-value)]
    (om/set-state! owner :cards (parse-deck deck))))

(defn deck-view [{:keys [name]} owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:h4 (:identity deck)]
        [:p (:name deck)]]))))

(defn deck-builder [{:keys [decks]} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
       :card-search ""
       :quantity 1
       :name ""
       :identity ""
       :side ""
       :cards []})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:h1 "Your decks"]
        [:div.deckbuilder.blue-shade.panel
         [:div.viewport {:ref "viewport"}
          [:div.decks
           [:p.button-bar
            [:button {:on-click #(new-deck "Corp" owner)} "New Corp deck"]
            [:button {:on-click #(new-deck "Runner" owner)} "New Runner deck"]]
           (if (empty? decks)
             [:h4 "You have no deck"]
             (om/build-all deck-view decks))]

          [:div.decklist
           [:h2.deck-name (:name state)]
           [:h2.deck-name (:identity state)]
           (if (:edit state)
             [:button {:on-click #(save-deck owner)} "Save"]
             [:button {:on-click #(edit-deck owner)} "Edit"])
           [:div.cards
            (for [card (:cards state)]
              [:p (:qty card) " "
               (if-let [name (get-in card [:card :title])]
                 [:a {:href ""} name]
                 (:card card))])]]

          [:div.deckedit
           [:p [:input.name {:type "text" :placeholder "Deck name" :value (:name state)
                             :on-change #(om/set-state! owner :name (.. % -target -value))}]]
           [:p
            [:select {:value (:identity state)}
             (for [card (filter #(and (= (:side %) (:side state)) (= (:type %) "Identity")) (:cards @cb/app-state))]
               [:option (:title card)])]]
           [:p
            [:input.lookup {:type "text" :placeholder "Card" :value (:card-search state)}] " x "
            [:input.qty {:type "text" :value (:quantity state)}]
            [:button {:on-click #()} "Add"]]
           [:textarea {:ref "deck-edit" :on-change #(handle-edit % owner)}]]]]]))))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})

;; (let [user (:user @auth/app-state)]
;;   (when-not (empty? user)
;;     (go (swap! app-state assoc :decks
;;                (:json (<! (GET (str "/data/decks/user/username/" (:username user)))))))))
