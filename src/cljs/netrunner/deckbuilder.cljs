(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [join]]
            [netrunner.auth :as auth]
            [netrunner.cardbrowser :as cb]
            [netrunner.ajax :refer [POST GET]]
            [netrunner.deck :refer [parse-deck]]))

(def app-state (atom {:decks []}))

(defn side-identities [side]
  (filter #(and (= (:side %) side)
                (not (#{"Special" "Alternates"} (:setname %)))
                (= (:type %) "Identity")) (:cards @cb/app-state)))

(defn edit-deck [owner]
  (om/set-state! owner :edit true)
  (-> owner (om/get-node "viewport") js/$ (.css "left" -500)))

(defn new-deck [side owner]
  (om/set-state! owner :deck {:side side :name "New deck" :cards []
                              :identity (-> side side-identities first :title)})
  (edit-deck owner))

(defn end-edit [owner]
  (om/set-state! owner :edit false)
  (-> owner (om/get-node "viewport") js/$ (.css "left" 0)))

(defn save-deck [owner]
  (end-edit owner)
  (let [deck (om/get-state owner :deck)
        cards (for [card (:cards deck)]
                {:qty (:qty card) :card (get-in card [:card :title])})
        data (assoc deck :cards cards)]
    (swap! app-state assoc :decks (conj (:decks @app-state) deck))
    (go (let [response (<! (POST "/data/decks/" data :json))]))))

(defn handle-edit [event owner]
  (let [deck (-> owner (om/get-node "deck-edit") .-value)]
    (om/set-state! owner [:deck :cards] (parse-deck deck))))

(defn decklist-view [{:keys [name identity cards]} owner]
  (om/component
   (sab/html
    [:div
     [:h3.deckname name]
     [:h4 identity]
     [:div.cards
      (for [card cards]
        [:p (:qty card) " "
         (if-let [name (get-in card [:card :title])]
           [:a {:href ""} name]
           (:card card))])]])))

(defn deck-builder [{:keys [decks]} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
       :card-search ""
       :quantity 1
       :deck {}})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:div.deckbuilder.blue-shade.panel
         [:div.viewport {:ref "viewport"}
          [:div.decks
           [:div.button-bar
            [:button {:on-click #(new-deck "Corp" owner)} "New Corp deck"]
            [:button {:on-click #(new-deck "Runner" owner)} "New Runner deck"]]
           [:div.deck-collection
            (if (empty? decks)
              [:h4 "You have no deck"]
              (for [deck decks]
                [:div.block-link {:class (when (= (:deck state) deck) "active")
                                  :on-click #(om/set-state! owner :deck deck)}
                 [:h4 (:name deck)]
                 [:p (:identity deck)]]))]]

          [:div.decklist
           (when-not (empty? (:deck state))
             (if (:edit state)
               [:span
                [:button {:on-click #(end-edit owner)} "Cancel"]
                [:button {:on-click #(save-deck owner)} "Save"]]
               [:button {:on-click #(edit-deck owner)} "Edit"]))
           (om/build decklist-view (:deck state))]

          [:div.deckedit
           [:div
            [:p
             [:input.name {:type "text" :placeholder "Deck name" :value (get-in state [:deck :name])
                           :on-change #(om/set-state! owner [:deck :name] (.. % -target -value))}]]
            [:p
             [:select {:value (get-in state [:deck :identity])
                       :on-change #(om/set-state! owner [:deck :identity] (.. % -target -value))}
              (for [card (side-identities (get-in state [:deck :side]))]
                [:option (:title card)])]]
            [:p
             [:input.lookup {:type "text" :placeholder "Card" :value (:card-search state)}] " x "
             [:input.qty {:type "text" :value (:quantity state)}]
             [:button {:on-click #()} "Add"]]
            [:textarea {:ref "deck-edit" :on-change #(handle-edit % owner)}]]]]]]))))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})

(when-not (empty? (:user @auth/app-state))
  (go (let [data (:json (<! (GET (str "/data/decks" (:username user)))))
            decks (for [deck data]
                    (let [cards (map #(str (:qty %) " " (:card %)) (:cards deck))]
                      (assoc deck :cards (parse-deck (join "\n" cards)))))]
        (swap! app-state assoc :decks decks))))


(println (:decks @app-state))
