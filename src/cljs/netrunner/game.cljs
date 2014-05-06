(ns netrunner.game
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.socket :refer [out-channel chat-channel]]))

(def game-state
  (atom
   {:log []
    :side :runner
    :corp
      {:r&d []
       :hq []
       :archive []
       :remote-servers []
       :credit 5
       :click 3
       :bad-publicity 0
       :agenda-point 0
       :max-hand-size 5}
    :runner
      {:stack []
       :grip []
       :heap []
       :rig []
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

(defn deck-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.deck {} ""]))))

(defn discard-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.discard {} ""]))))

(defn stats-view [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.stats {} ""]))))

;; (defn corp-board [app owner]
;;   (reify
;;     om/IRender
;;     (render [this]
;;       (sab/html
;;        [:div.corp-board {}
;;         (om/build stack-view (:hq app))
;;         (om/build stack-view (:r&d app))
;;         (om/build stack-view (:archive app))]))))

;; (defn runner-board [app owner]
;;   (reify
;;     om/IRender
;;     (render [this]
;;       (sab/html
;;        [:div.runner-board {}]))))

(defn deck []
  (if (= (:side game-state) :runner) :stack :r&d))

(defn hand []
  (if (= (:side game-state) :runner) :grip :hq))

(defn discard []
  (if (= (:side game-state) :runner) :heap :archive))

(defn game-app [app owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.dashboard {}
                 (om/build log-pane (:log app))
                 (om/build hand-view ((hand) app))
                 (om/build deck-view ((deck) app))
                 (om/build discard-view ((discard) app))
                 (om/build stats-view app)
                 ;; (om/build corp-board (:corp app))
                 ;; (om/build runner-board (:runner app))
                 ]))))

(om/root game-app game-state {:target (. js/document (getElementById "gameboard"))})
