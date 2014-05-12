(ns netrunner.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [goog.net.XhrIo :as xhr]))

(defn fetch [url]
  (let [ch (chan)]
    (xhr/send url (fn [event]
                    (let [response (-> event .-target .getResponseText JSON/parse
                                   (js->clj :keywordize-keys true))]
                      (put! ch response))))
    ch))

(def app-state
  (atom
   {:cards []
    :sets []}))

(defn card-view [card owner]
  (om/component
   (let [base-url "http://netrunnerdb.com/web/bundles/netrunnerdbcards/images/cards/en/"]
     (sab/html [:img.card-img {:src (str base-url (:code card) ".png")}]))))

(defn set-view [{:keys [set set-filter]} owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (let [name (:name set)]
        (sab/html
         [:div {:class (if (= set-filter name) "active" "")
                :on-click #(put! (:ch state) {:filter :set-filter :value name})}
          name])))))

(defn card-browser [cursor owner]
  (reify
    om/IInitState
    (init-state [this]
      {:set-filter ""
       :filter-ch (chan)})

    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [f (<! (om/get-state owner :filter-ch))]
              (om/set-state! owner (:filter f) (:value f))))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.cardbrowser
        [:div.blue-shade.panel.set-list {}
         [:div {:class (if (= (:set-filter state) "") "active" "")
                :on-click #(om/set-state! owner :set-filter "")} "All"]
         (for [set (sort-by :available (:sets cursor))]
           (om/build set-view
                     {:set set :set-filter (:set-filter state)}
                     {:init-state {:ch (:filter-ch state)}}))]
        [:div.card-list
         (om/build-all card-view
                       (let [set-filter (:set-filter state)
                             cards (:cards cursor)]
                         (if (empty? set-filter)
                           cards
                           (filter #(= (:setname %) set-filter) cards))))]]))))

(om/root card-browser app-state {:target (. js/document (getElementById "cardbrowser"))})

(go (swap! app-state assoc :sets (<! (fetch "data/sets"))))
(go (swap! app-state assoc :cards (<! (fetch "data/cards"))))
