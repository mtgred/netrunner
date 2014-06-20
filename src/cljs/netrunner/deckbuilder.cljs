(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <! timeout] :as async]
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

(defn get-card [title]
  (some #(when (= (:title %) title) %) (:cards @cb/app-state)))

(defn deck->str [cards]
  (reduce #(str %1 (:qty %2) " " (get-in %2 [:card :title]) "\n") "" cards))

(defn compute-influence [deck]
  (let [faction (get-in deck [:identity :faction] deck)
        cards (:cards deck)]
    (reduce #(let [card (:card %2)]
               (if (= (:faction card) faction)
                 %1
                 (+ %1 (* (:qty %2) (:factioncost card)))))
            0 (:cards deck))))

(defn edit-deck [owner]
  (om/set-state! owner :edit true)
  (om/set-state! owner :deck-edit (deck->str (om/get-state owner [:deck :cards])))
  (-> owner (om/get-node "viewport") js/$ (.css "left" -500))
  (go (<! (timeout 500))
      (-> owner (om/get-node "deckname") js/$ .focus)))

(defn new-deck [side owner]
  (om/set-state! owner :deck {:side side :name "New deck" :cards []
                              :identity (-> side side-identities first)})
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

(defn handle-edit [owner]
  (let [text (-> owner (om/get-node "deck-edit") .-value)]
    (om/set-state! owner :deck-edit text)
    (om/set-state! owner [:deck :cards] (parse-deck text))))

(defn handle-delete [cursor owner]
  (let [deck (om/get-state owner :deck)]
    (go (let [response (<! (POST "/data/decks/delete" deck :json))]))
    (om/transact! cursor :decks (fn [ds] (remove #(= deck %) ds)))
    (om/set-state! owner :deck (first (:decks @cursor)))))

(defn deck-builder [{:keys [decks] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
       :card-search ""
       :quantity 1
       :deck-edit ""
       :deck nil})

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (if (and (not (empty? decks)) (not (:deck prev-state)))
        (om/set-state! owner :deck (first decks))))

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
              (for [deck (:decks cursor)]
                [:div.deckline {:class (when (= (:deck state) deck) "active")
                                  :on-click #(om/set-state! owner :deck deck)}
                 [:h4 (:name deck)]
                 [:p (get-in deck [:identity :title])]]))]]

          [:div.decklist
           (when-let [deck (:deck state)]
             [:div
              (if (:edit state)
                [:span
                 [:button {:on-click #(end-edit owner)} "Cancel"]
                 [:button {:on-click #(save-deck owner)} "Save"]]
                [:span
                 [:button {:on-click #(handle-delete cursor owner)} "Delete"]
                 [:button {:on-click #(edit-deck owner)} "Edit"]])
              [:h3 (:name deck)]
              (let [identity (:identity deck)]
                [:h4 (:title identity)]
                [:div
                 (str "Influence: " (compute-influence deck)"/" (:influencelimit identity))])
              [:div.cards
               (for [group (group-by #(get-in % [:card :type]) (:cards deck))]
                 [:div.group
                  [:h4 (str (first group) " (" (count (last group)) ")") ]
                  (for [line (last group)]
                    [:div.line (:qty line) " "
                     (if-let [name (get-in line [:card :title])]
                       [:a {:href ""} name]
                       (:card line))])])]])]

          [:div.deckedit
           [:div
            [:p
             [:h4 "Deck name"]
             [:input.deckname {:type "text" :placeholder "Deck name"
                               :ref "deckname" :value (get-in state [:deck :name])
                               :on-change #(om/set-state! owner [:deck :name] (.. % -target -value))}]]
            [:p
             [:h4 "Identity"]
             [:select.identity {:value (get-in state [:deck :identity])
                                :on-change #(om/set-state! owner [:deck :identity] (get-card (.. % -target -value)))}
              (for [card (side-identities (get-in state [:deck :side]))]
                [:option (:title card)])]]
            [:p
             [:h4 "Card lookup"]
             [:form
              [:input.lookup {:type "text" :placeholder "Card" :value (:card-search state)}] " x "
              [:input.qty {:type "text" :value (:quantity state)}]
              [:button {:on-click #()} "Add to deck"]]]
            [:h4 "Decklist"]
            [:textarea {:ref "deck-edit" :value (:deck-edit state)
                        :on-change #(handle-edit owner)}]]]]]]))))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})

(when-not (empty? (:user @auth/app-state))
  (go (let [data (:json (<! (GET (str "/data/decks" (:username user)))))
            decks (for [deck data]
                    (let [cards (map #(str (:qty %) " " (:card %)) (:cards deck))]
                      (assoc deck :cards (parse-deck (join "\n" cards)))))]
        (swap! app-state assoc :decks decks))))
