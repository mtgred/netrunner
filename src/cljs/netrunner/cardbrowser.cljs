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
    :sets []}))

(defn set-view [{:keys [name]} owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [set-filter]}]
      (sab/html
       [:div {:class (if (= set-filter name) "active" "")
              :on-click #(om/set-state! owner :set-filter name)} name]))))

(defn set-list-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.blue-shade.panel.set-list {}
        (om/build-all set-view (:sets cursor) {:init-state state})]))))

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
  (reify
    om/IInitState
    (init-state [this]
      {:set-filter "Core Set"})

    om/IRenderState
    (render-state [this {:keys [set-filter]}]
      (sab/html
       [:div.cardbrowser
        (om/build set-list-view cursor {:init-state {:set-filter set-filter}})
        [:div.main
         ;;  (om/build filter-view cursor)
         (om/build card-list-view
                   (let [cards (:cards cursor)]
                     (if (zero? (alength set-filter))
                       cards
                       (filter #(= (:setname %) set-filter) cards))))]]))))

(om/root card-browser-app app-state {:target (. js/document (getElementById "cardbrowser"))})

(go (swap! app-state assoc :sets (<! (fetch "data/sets"))))
(go (swap! app-state assoc :cards (<! (fetch "data/cards"))))
