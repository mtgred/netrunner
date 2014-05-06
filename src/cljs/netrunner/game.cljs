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

(defn log-pane [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.log {} "Log"]))))

(defn hand-view [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.hand {} "Hand"]))))

(defn deck-view [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.deck {} "Deck"]))))

(defn discard-view [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.discard {} "Discard"]))))

(defn scored-view [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.scored {} (str "Agenda Points")]))))

(defn stats-view [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.stats {} "Stat"]))))

(defn corp-board [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.board {}
        ;; (om/build stack-view (:hq app))
        ;; (om/build stack-view (:r&d app))
        ;; (om/build stack-view (:archive app))
        ]))))

(defn runner-board [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.board {}]))))

(defn deck []
  (if (= (:side game-state) :runner) :stack :r&d))

(defn hand []
  (if (= (:side game-state) :runner) :grip :hq))

(defn discard []
  (if (= (:side game-state) :runner) :heap :archive))

(defn game-app [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:div.gameboard {}
        [:div.leftpane {}
         [:div {}
          (om/build stats-view (:runner app))
          (om/build scored-view app)]
         [:div {}
          (om/build scored-view app)
          (om/build stats-view (:corp app))]]
        [:div.centralpane
         (om/build corp-board (:corp app))
         (om/build runner-board (:runner app))
         [:div.dashboard {}
          (om/build hand-view ((hand) app))
          (om/build deck-view ((deck) app))
          (om/build discard-view ((discard) app))]]
        [:div.rightpane {}
         [:div.card-zoom {}]
         (om/build log-pane (:log app))
         [:input {:type "text" :placeholder "Say something..."}]]]))))

(om/root game-app game-state {:target (. js/document (getElementById "gameboard"))})
