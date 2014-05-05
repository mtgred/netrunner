(ns netrunner.game
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.socket :refer [out-channel chat-channel]]))

(def game-state
  (atom
   {:log []
    :corp
      {:r&d []
       :hq []
       :archive []
       :hidden-archive []
       :credit 5
       :click 3
       :bad-publicity 0
       :agenda-point 0
       :max-hand-size 5}
    :runner
      {:stack []
       :grip []
       :heap []
       :credit 5
       :memory 4
       :link 0
       :click 4
       :tag 0
       :agenda-point 0
       :max-hand-size 5
       :brain-damage 0}}))

(defn log-pane [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.log {} ""]))))

(defn hand-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.hand {} ""]))))

(defn stack-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.stack {} ""]))))

(defn discard-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.discard {} ""]))))

(defn corp-board [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.corp-board {}
        (om/build hand-view (:hq app))
        (om/build stack-view (:r&d app))
        (om/build discard-view (:archive app))
        (om/build stack-view (:hidden-archive app))]))))

(defn runner-board [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.runner-board {}
        (om/build hand-view (:grip app))
        (om/build stack-view (:stack app))
        (om/build discard-view (:heap app))]))))

(defn game-app [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.gameboard {}
                 (om/build log-pane (:log app))
                 (om/build corp-board (:corp app))
                 (om/build runner-board (:runner app))]))))

(om/root game-app game-state {:target (. js/document (getElementById "gameboard"))})
