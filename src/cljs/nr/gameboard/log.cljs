(ns nr.gameboard.log
  (:require
   [clojure.string :as string]
   [nr.angel-arena.log :as angel-arena-log]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.avatar :refer [avatar]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.card-preview :refer [card-preview-mouse-out
                                      card-preview-mouse-over zoom-channel]]
   [nr.gameboard.state :refer [game-state not-spectator?]]
   [nr.help :refer [command-info]]
   [nr.translations :refer [tr]]
   [nr.utils :refer [influence-dot player-highlight-option-class
                     render-message render-player-highlight]]
   [nr.ws :as ws]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(def commands (distinct (map :name command-info)))
(def command-info-map (->> command-info
                           (map (fn [item] [(:name item) (select-keys item [:has-args :usage :help])]))
                           (into {})))

(defn scrolled-to-end?
  [el tolerance]
  (> tolerance (- (.-scrollHeight el) (.-scrollTop el) (.-clientHeight el))))

(def should-scroll (r/atom {:update true :send-msg false}))

(defn log-typing []
  (r/with-let [typing (r/cursor game-state [:typing])]
    (when @typing
      [:div [:p.typing
             (doall
               (for [i (range 10)]
                 ^{:key i}
                 [:span " " influence-dot " "]))]])))

(defn send-msg [s]
  (let [text (:msg @s)]
    (when (and (not (:replay @game-state))
               (seq text))
      (reset! should-scroll {:update false :send-msg true})
      (ws/ws-send! [:game/say {:gameid (current-gameid app-state)
                               :msg text}])
      (swap! s assoc :msg ""))))

(defn send-typing
  "Send a typing event to server for this user if it is not already set in game state AND user is not a spectator"
  [s]
  (let [text (:msg @s)]
    (when (and (not (:replay @game-state))
               (not-spectator?))
      (ws/ws-send! [:game/typing {:gameid (current-gameid app-state)
                                  :typing (boolean (seq text))}]))))

(defn indicate-action []
  (when (not-spectator?)
    [:button.indicate-action {:on-click #(do (.preventDefault %)
                                             (send-command "indicate-action"))
                              :key "Indicate action"}
     (tr [:game.indicate-action "Indicate action"])]))

(defn fuzzy-match-score
  "Matches if all characters in input appear in target in order.
  Score is sum of matched indices, lower is a better match"
  [input target]
  (loop [curr-input (first input)
         rest-input (rest input)
         target-index (string/index-of target curr-input 0)
         score target-index]
    (when target-index
      (if (not (seq rest-input))
        score
        (let [next-index (string/index-of target (first rest-input) (inc target-index))]
          (recur
            (first rest-input)
            (rest rest-input)
            next-index
            (+ score (or next-index 0))))))))

(defn find-command-matches
  ([input commands]
   (when (= "/" (first input))
     (->> commands
       (map (fn [target] {:match target :score (fuzzy-match-score input target)}))
       (filter :score)
       (sort-by :score)
       (map :match)))))

(defn show-command-menu? [s]
  (seq (:command-matches s)))

(defn reset-command-menu
  "Resets the command menu state."
  [state]
  (swap! state assoc :command-matches '())
  (swap! state assoc :command-highlight nil))

(defn command-menu-key-down-handler
  [state e]
  (when (show-command-menu? @state)
    (let [key (-> e .-key)
          matches (:command-matches @state)
          match-count (count matches)]
      (case key
        ;; ArrowDown
        "ArrowDown" (do (.preventDefault e)
                        (swap! state update :command-highlight #(if % (mod (inc %) match-count) 0)))
        ;; ArrowUp
        "ArrowUp" (when (:command-highlight @state)
                    (.preventDefault e)
                    (swap! state update :command-highlight #(if % (mod (dec %) match-count) 0)))
        ;; Return, Space, ArrowRight, Tab
        ("Enter" "Space" "ArrowRight" "Tab")
        (when (or (= 1 match-count) (:command-highlight @state))
          (let [use-index (if (= 1 match-count) 0 (:command-highlight @state))
                command (nth matches use-index)]
            (.preventDefault e)
            (swap! state assoc :msg (str command " "))
            (reset-command-menu state)
            ;; auto send when no args needed
            (when (and (= key "Enter")
                       (not (get-in command-info-map [command :has-args])))
              (send-msg state))))
        ;; else
        nil))))

(defn log-input-change-handler
  [state e]
  (reset-command-menu state)
  (swap! state assoc :command-matches (-> e .-target .-value (find-command-matches commands)))
  (swap! state assoc :msg (-> e .-target .-value))
  (send-typing state))

(defn command-menu [!input-ref state]
  (when (show-command-menu? @state)
    [:div.command-matches-container.panel.blue-shade
     {:on-mouse-leave #(swap! state dissoc :command-highlight)}
     [:ul.command-matches
      (doall (map-indexed
               (fn [i match]
                 [:li.command-match
                  {:key match
                   :class (when (= i (:command-highlight @state)) "highlight")}
                  [:span {:on-mouse-over #(swap! state assoc :command-highlight i)
                          :on-click #(do
                                       (swap! state assoc :msg (str match " "))
                                       (reset-command-menu state)
                                       (.focus @!input-ref))}

                          (get-in command-info-map [match :usage])]])
                      (:command-matches @state)))]]))

(defn log-input []
  (let [current-game (r/cursor app-state [:current-game])
        !input-ref (r/atom nil)
        state (r/atom {})]
    (fn []
      (when (or (not-spectator?)
                (not (:mutespectators @current-game)))
        [:div.log-input
         [:div.form-container
          [:form {:on-submit #(do (.preventDefault %)
                                  (reset-command-menu state)
                                  (send-msg state))}
           [:input#log-input
            {:placeholder (tr [:chat.placeholder "Say something"])
             :type "text"
             :autoComplete "off"
             :ref #(reset! !input-ref %)
             :value (:msg @state)
             :on-blur #(send-typing (atom nil))
             :on-key-down #(command-menu-key-down-handler state %)
             :on-change #(log-input-change-handler state %)}]]]
         [indicate-action]
         [command-menu !input-ref state]]))))

(defn log-messages []
  (let [log (r/cursor game-state [:log])
        corp (r/cursor game-state [:corp :user :username])
        runner (r/cursor game-state [:runner :user :username])]
    (r/create-class
      {:display-name "log-messages"

       :component-did-mount
       (fn [this]
         (when (:update @should-scroll)
           (let [n (rdom/dom-node this)]
             (set! (.-scrollTop n) (.-scrollHeight n)))))

       :component-will-update
       (fn [this]
         (let [n (rdom/dom-node this)]
           (reset! should-scroll {:update (or (:send-msg @should-scroll)
                                              (scrolled-to-end? n 15))
                                  :send-msg false})))

       :component-did-update
       (fn [this]
         (when (:update @should-scroll)
           (let [n (rdom/dom-node this)]
             (set! (.-scrollTop n) (.-scrollHeight n)))))

       :reagent-render
       (fn []
         (into [:div.messages {:class [(when (:replay @game-state)
                                         "panel-bottom")
                                       (player-highlight-option-class)]
                               :on-mouse-over #(card-preview-mouse-over % zoom-channel)
                               :on-mouse-out #(card-preview-mouse-out % zoom-channel)
                               :aria-live "polite"}]
               (map
                 (fn [{:keys [user text timestamp]}]
                   ^{:key timestamp}
                   (if (= user "__system__")
                     [:div.system (render-message (render-player-highlight text @corp @runner))]
                     [:div.message
                      [avatar user {:opts {:size 38}}]
                      [:div.content
                       [:div.username (:username user)]
                       [:div (render-message text)]]]))
                 @log)))})))

(defn log-pane []
  (fn []
    [:div.log
     [angel-arena-log/inactivity-pane]
     [log-messages]
     [log-typing]
     [log-input]]))
