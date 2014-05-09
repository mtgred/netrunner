(ns netrunner.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! close!] :as async]
            [goog.net.XhrIo :as xhr]))

(defn fetch [url]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (let [response (-> event .-target .getResponseText JSON/parse
                                   (js->clj :keywordize-keys true))]
                  (put! ch response)
                  (close! ch))))
    ch))

(def app-state
  (atom
   {:cards []
    :sets []
    :search-query ""
    :cycle-filter ""
    :set-filter []
    :side-filter ""
    :faction-filter []
    :type-filter []}))

(defn set-view [{:keys [set set-filter]} owner]
  (om/component
   (sab/html
    [:div {:class (if (= (first set-filter) set) "active" "")
           :on-click #(om/update! set-filter [set])}
     (:name set)])))

(defn set-list-view [{:keys [sets set-filter]} owner]
  (om/component
   (sab/html
    [:div.blue-shade.panel.set-list {}
     (for [set sets]
       (om/build set-view {:set set :set-filter set-filter}))])))

(defn card-view [card owner]
  (om/component
   (let [base-url "http://netrunnerdb.com/web/bundles/netrunnerdbcards/images/cards/en/"]
     (sab/html [:img.card-img {:src (str base-url (:code card) ".png")}]
               ;; [:div {} (:title card)]
               ))))

(defn card-list-view [cards owner]
  (om/component
   (sab/html
    [:div.card-list
     (om/build-all card-view cards)])))

(defn card-browser-app [cursor owner]
  (om/component
   (sab/html
    [:div.cardbrowser
     (om/build set-list-view cursor)
     [:div.main
     ;;  (om/build filter-view cursor)
      (om/build card-list-view (:cards cursor))]])))

(om/root card-browser-app app-state {:target (. js/document (getElementById "cardbrowser"))})

(go (swap! app-state assoc :sets (<! (fetch "data/sets"))))
(go (swap! app-state assoc :cards (<! (fetch "data/cards"))))
