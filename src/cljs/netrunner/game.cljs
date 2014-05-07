(ns netrunner.game
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [netrunner.socket :refer [out-channel chat-channel]]))

(def game-state
  (atom
   {:game-id 0
    :log []
    :side :runner
    :corp
      {:username "Richard Gardfield"
       :r&d []
       :hq []
       :archive []
       :remote-servers []
       :click 3
       :credit 5
       :bad-publicity 0
       :agenda-point 0
       :max-hand-size 5}
    :runner
      {:username "Lukas Litzsinger"
       :stack []
       :grip []
       :heap []
       :rig []
       :click 4
       :credit 5
       :memory 4
       :link 0
       :tag 0
       :agenda-point 0
       :max-hand-size 5
       :brain-damage 0}}))

(swap! game-state assoc-in [:corp :bad-publicity] 0)

(defn log-pane [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:div.panel.blue-shade.log {} "Log"]))))

(defn send-msg [cursor owner]
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (when-not (zero? (alength text))
      (aset input "value" "")
      (put! out-channel #js {:type "game"
                             :game-id 0
                             :action "say"
                             :msg text}))))

(defn msg-input-view [cursor owner]
  (reify
    om/IRender
    (render [this]
      (sab/html
       [:input {:type "text" :ref "msg-input" :placeholder "Say something..."
                :onKeyPress #(when (== (.-keyCode %) 13) (send-msg cursor owner))}]))))

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

(defn runner-stats-view [{:keys [username click credit memory link tag
                                 brain-damage max-hand-size]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis username]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     [:div (str memory " Memory Unit" (if (> memory 1) "s" ""))]
     [:div (str link " Link" (if (> link 1) "s" ""))]
     [:div (str tag " Tag" (if (> tag 1) "s" ""))]
     [:div (str brain-damage " Brain Damage" (if (> brain-damage 1) "s" ""))]
     [:div (str max-hand-size " Max Hand Size")]])))

(defn corp-stats-view [{:keys [username click credit bad-publicity max-hand-size]} owner]
  (om/component
   (sab/html
    [:div.panel.blue-shade {}
     [:h4.ellipsis username]
     [:div (str click " Click" (if (> click 1) "s" ""))]
     [:div (str credit " Credit" (if (> credit 1) "s" ""))]
     [:div (str bad-publicity " Bad Publicit" (if (> bad-publicity 1) "ies" "y"))]
     [:div (str max-hand-size " Max Hand Size")]])))

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
          (om/build corp-stats-view (:corp cursor))
          (om/build scored-view cursor)]
         [:div {}
          (om/build scored-view cursor)
          (om/build runner-stats-view (:runner cursor))
          ]]
        [:div.centralpane
         (om/build corp-board (:corp cursor))
         (om/build runner-board (:runner cursor))
         [:div.dashboard {}
          (om/build hand-view ((hand) cursor))
          (om/build deck-view ((deck) cursor))
          (om/build discard-view ((discard) cursor))]]
        [:div.rightpane {}
         [:div.card-zoom {}]
         (om/build log-pane (:log cursor))
         (om/build msg-input-view cursor)]]))))

(om/root game-app game-state {:target (. js/document (getElementById "gameboard"))})
