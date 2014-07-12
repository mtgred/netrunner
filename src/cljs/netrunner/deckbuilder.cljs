(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :refer [join]]
            [netrunner.auth :refer [auth-channel] :as auth]
            [netrunner.cardbrowser :refer [cards-channel image-url card-view] :as cb]
            [netrunner.ajax :refer [POST GET]]
            [netrunner.deck :refer [parse-deck]]))

(def app-state (atom {:decks []}))

(defn fetch-decks []
  (go (let [data (:json (<! (GET (str "/data/decks"))))
            decks (for [deck data]
                    (let [cards (map #(str (:qty %) " " (:card %)) (:cards deck))]
                      (assoc deck :cards (parse-deck (get-in deck [:identity :side]) (join "\n" cards)))))]
        (swap! app-state assoc :decks decks))))

(go (let [cards (<! cards-channel)]
      (if (:user @auth/app-state)
        (fetch-decks)
        (go (<! auth-channel)
            (fetch-decks)))
      (>! cards-channel cards)))

(defn side-identities [side]
  (filter #(and (= (:side %) side)
                (not (#{"Special" "Alternates"} (:setname %)))
                (= (:type %) "Identity")) (:cards @cb/app-state)))

(defn get-card [title]
  (some #(when (= (:title %) title) %) (:cards @cb/app-state)))

(defn deck->str [owner]
  (let [cards (om/get-state owner [:deck :cards])
        str (reduce #(str %1 (:qty %2) " " (get-in %2 [:card :title]) "\n") "" cards)]
    (om/set-state! owner :deck-edit str)))

(defn influence [deck]
  (let [faction (get-in deck [:identity :faction])
        cards (:cards deck)]
    (reduce #(let [card (:card %2)]
               (if (= (:faction card) faction)
                 %1
                 (+ %1 (* (:qty %2) (:factioncost card)))))
            0 (:cards deck))))

(defn card-count [cards]
  (reduce #(+ %1 (:qty %2)) 0 cards))

(defn min-agenda-points [deck]
  (let [size (max (card-count (:cards deck)) (get-in deck [:identity :minimumdecksize]))]
    (+ 2 (* 2 (quot size 5)))))

(defn agenda-points [cards]
  (reduce #(if-let [point (get-in %2 [:card :agendapoints])]
             (+ (* point (:qty %2)) %1) %1) 0 cards))

(defn edit-deck [owner]
  (om/set-state! owner :edit true)
  (deck->str owner)
  (-> owner (om/get-node "viewport") js/$ (.addClass "edit"))
  (go (<! (timeout 500))
      (-> owner (om/get-node "deckname") js/$ .focus)))

(defn new-deck [side owner]
  (om/set-state! owner :deck {:side side :name "New deck" :cards []
                              :identity (-> side side-identities first)})
  (edit-deck owner))

(defn end-edit [owner]
  (om/set-state! owner :edit false)
  (-> owner (om/get-node "viewport") js/$ (.removeClass "edit")))

(defn save-deck [cursor owner]
  (end-edit owner)
  (let [deck (assoc (om/get-state owner :deck) :date (.toJSON (js/Date.)))
        decks (:decks @app-state)
        cards (for [card (:cards deck)]
                {:qty (:qty card) :card (get-in card [:card :title])})
        data (assoc deck :cards cards)]
    (if-let [id (:_id deck)]
      (om/update! cursor :decks (map #(if (= id (:_id %)) deck %) decks))
      (om/update! cursor :decks (conj decks deck)))
    (go (let [response (<! (POST "/data/decks/" data :json))]))))

(defn match [{:keys [side faction]} query]
  (if (empty? query)
    []
    (let [cards (filter #(and (= (:side %) side)
                              (not (#{"Special" "Alternates"} (:setname %)))
                              (not= (:type %) "Identity")
                              (or (not= (:type %) "Agenda")
                                  (= (:faction %) "Neutral")
                                  (= (:faction %) faction)))
                        (:cards @cb/app-state))]
      (take 10 (filter #(if (= (.indexOf (.toLowerCase (:title %)) (.toLowerCase query)) -1) false true) cards)))))

(defn handle-edit [owner]
  (let [text (-> owner (om/get-node "deck-edit") .-value)]
    (om/set-state! owner :deck-edit text)
    (om/set-state! owner [:deck :cards] (parse-deck (om/get-state owner [:deck :identity :side]) text))))

(defn handle-delete [cursor owner]
  (let [deck (om/get-state owner :deck)]
    (go (let [response (<! (POST "/data/decks/delete" deck :json))]))
    (om/transact! cursor :decks (fn [ds] (remove #(= deck %) ds)))
    (om/set-state! owner :deck (first (:decks @cursor)))))

(defn handle-keydown [owner event]
  (let [selected (om/get-state owner :selected)
        matches (om/get-state owner :matches)]
    (case (.-keyCode event)
      38 (when (> selected 0)
           (om/update-state! owner :selected dec))
      40 (when (< selected (dec (count matches)))
           (om/update-state! owner :selected inc))
      (9 13) (when-not (= (om/get-state owner :query) (:title (first matches)))
               (.preventDefault event)
               (-> ".deckedit .qty" js/$ .focus)
               (om/set-state! owner :query (:title (nth matches selected))))
      (om/set-state! owner :selected 0))))

(defn handle-add [owner event]
  (.preventDefault event)
  (put! (om/get-state owner :edit-channel)
        {:qty (js/parseInt (om/get-state owner :quantity))
         :card (first (om/get-state owner :matches))})
  (om/set-state! owner :quantity 3)
  (om/set-state! owner :query "")
  (-> ".deckedit .lookup" js/$ .focus))

(defn card-lookup [{:keys [cards]} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:query ""
       :matches []
       :quantity 3
       :selected 0})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:p
        [:h4 "Card lookup"]
        [:form.card-search {:on-submit #(handle-add owner %)}
         [:input.lookup {:type "text" :placeholder "Card" :value (:query state)
                         :on-change #(om/set-state! owner :query (.. % -target -value))
                         :on-key-down #(handle-keydown owner %)}]
         " x "
         [:input.qty {:type "text" :value (:quantity state)
                      :on-change #(om/set-state! owner :quantity (.. % -target -value))}]
         [:button "Add to deck"]
         (let [query (:query state)
               matches (match (get-in state [:deck :identity]) query)]
           (when-not (or (empty? query)
                         (= (:title (first matches)) query))
             (om/set-state! owner :matches matches)
             [:div.typeahead
              (for [i (range (count matches))]
                [:div {:class (if (= i (:selected state)) "selected" "")
                       :on-click (fn [e] (-> ".deckedit .qty" js/$ .focus)
                                         (om/set-state! owner :query (.. e -target -innerHTML)))}
                 (:title (nth matches i))])]))]]))))

(defn deck-builder [{:keys [decks] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
       :edit-channel (chan)
       :deck nil})

    om/IWillMount
    (will-mount [this]
      (let [edit-channel (om/get-state owner :edit-channel)]
        (go (while true
              (let [edit (<! edit-channel)
                    card (:card edit)
                    cards (om/get-state owner [:deck :cards])
                    match? #(when (= (get-in % [:card :title]) (:title card)) %)
                    existing-line (some match? cards)]
                (if existing-line
                  (let [new-qty (+ (:qty existing-line) (:qty edit))
                        other-cards (remove match? cards)
                        new-cards (cond (> new-qty 3) (conj other-cards {:qty 3 :card card})
                                        (<= new-qty 0) other-cards
                                        :else (conj other-cards {:qty new-qty :card card}))]
                    (om/set-state! owner [:deck :cards] new-cards))
                  (om/set-state! owner [:deck :cards] (conj cards edit)))
                (deck->str owner))))))

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (if (and (not (empty? decks)) (not (:deck prev-state)))
        (om/set-state! owner :deck (first (sort-by :date > decks)))))

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
              (for [deck (sort-by :date > decks)]
                [:div.deckline {:class (when (= (get-in state [:deck :_id]) (:_id deck)) "active")
                                :on-click #(om/set-state! owner :deck deck)}
                 [:img {:src (image-url (:identity deck))}]
                 [:h4 (:name deck)]
                 [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY - HH:mm"))]
                 [:p (get-in deck [:identity :title])]]))]]

          [:div.decklist
           (when-let [deck (:deck state)]
             (let [identity (:identity deck)
                   cards (:cards deck)]
               [:div
                (if (:edit state)
                  [:span
                   [:button.big {:on-click #(end-edit owner)} "Cancel"]
                   [:button.big {:on-click #(save-deck cursor owner)} "Save"]]
                  [:span
                   [:button.big {:on-click #(handle-delete cursor owner)} "Delete"]
                   [:button.big {:on-click #(edit-deck owner)} "Edit"]])
                [:h3.deckname (:name deck)]
                [:div.header
                 [:img {:src (image-url identity)}]
                 [:h4 (:title identity)]
                 (let [count (card-count cards)
                       min-count (:minimumdecksize identity)]
                   [:div count " cards"
                    (when (< count min-count)
                      [:span.invalid (str "(minimum " min-count ")")])])
                 (let [inf (influence deck)
                       limit (:influencelimit identity)]
                   [:div "Influence: "
                    [:span {:class (when (> inf limit) "invalid")} inf]
                    "/" (:influencelimit identity)])
                 (when (= (:side identity) "Corp")
                   (let [min-point (min-agenda-points deck)
                         points (agenda-points cards)]
                     [:div "Agenda points: " points
                      (when (< points min-point)
                        [:span.invalid "(minimum " min-point ")"])
                      (when (> points (inc min-point))
                        [:span.invalid "(maximum" (inc min-point) ")"])]))]
                [:div.cards
                 (for [group (group-by #(get-in % [:card :type]) cards)]
                   [:div.group
                    [:h4 (str (or (first group) "Unknown") " (" (card-count (last group)) ")") ]
                    (for [line (last group)]
                      [:div.line
                       (when (:edit state)
                         [:span
                          [:button.small {:type "button" :on-click #()} "+"]
                          [:button.small {:type "button" :on-click #()} "-"]])
                       (:qty line) " "
                       (if-let [name (get-in line [:card :title])]
                         (let [card (:card line)]
                           [:span
                            [:a {:href ""} name]
                            (om/build card-view card)
                            (when-not (or (= (:faction card) (:faction identity))
                                          (zero? (:factioncost card)))
                              (let [influence (* (:factioncost card) (:qty line))]
                                [:span.influence
                                 {:class (-> card :faction .toLowerCase (.replace " " "-"))
                                  :dangerouslySetInnerHTML
                                  #js {:__html (apply str (for [i (range influence)] "&#8226;"))}}]))])
                         (:card line))])])]]))]

          [:div.deckedit
           [:div
            [:p
             [:h4 "Deck name"]
             [:input.deckname {:type "text" :placeholder "Deck name"
                               :ref "deckname" :value (get-in state [:deck :name])
                               :on-change #(om/set-state! owner [:deck :name] (.. % -target -value))}]]
            [:p
             [:h4 "Identity"]
             [:select.identity {:value (get-in state [:deck :identity :title])
                                :on-change #(om/set-state! owner [:deck :identity] (get-card (.. % -target -value)))}
              (for [card (side-identities (get-in state [:deck :side]))]
                [:option (:title card)])]]
            (om/build card-lookup cursor {:state state})
            [:h4 "Decklist"]
            [:textarea {:ref "deck-edit" :value (:deck-edit state)
                        :placeholder "Copy & paste a decklist. Or start typing."
                        :on-change #(handle-edit owner)}]]]]]]))))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})
