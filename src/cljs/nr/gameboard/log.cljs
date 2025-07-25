(ns nr.gameboard.log
  (:require
   [clojure.string :as string]
   [jinteki.utils :refer [command-info]]
   [jinteki.cards :refer [all-cards]]
   [nr.angel-arena.log :as angel-arena-log]
   [nr.appstate :refer [app-state current-gameid]]
   [nr.avatar :refer [avatar]]
   [nr.gameboard.actions :refer [send-command]]
   [nr.gameboard.card-preview :refer [card-preview-mouse-out
                                      card-preview-mouse-over zoom-channel]]
   [nr.gameboard.state :refer [game-state not-spectator?]]
   [nr.translations :refer [tr]]
   [nr.utils :refer [influence-dot player-highlight-option-class
                     render-message render-player-highlight]]
   [nr.ws :as ws]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(def commands (distinct (map :name command-info)))
(def command-info-map (->> command-info
                           (map (fn [info] [(:name info) (select-keys info [:has-args :usage :help])]))
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
  (r/with-let [typing (r/cursor game-state [:typing])]
    (let [typing? (boolean (seq (:msg @s)))]
      (when (and (not (:replay @game-state))
                 ;; only send if the typing state is different
                 (or (and (not @typing) typing?)
                     (and (not typing?) @typing))
                 (not-spectator?))
        (ws/ws-send! [:game/typing {:gameid (current-gameid app-state)
                                    :typing typing?}])))))

(defn indicate-action []
  (when (not-spectator?)
    [:button.indicate-action {:on-click #(do (.preventDefault %)
                                             (send-command "indicate-action"))
                              :key "Indicate action"}
     (tr [:game_indicate-action "Indicate paid ability"])]))

(defn show-decklists []
  (when (get-in @app-state [:current-game :open-decklists])
    [:button.show-decklists {:on-click #(do (.preventDefault %)
                                            (swap! app-state update-in [:display-decklists] not))
                             :key "Show Decklists"}
     (tr [:game_show-decklists "Show/Hide decklists"])]))

(defn fuzzy-match-score
  "Matches if all characters in input appear in target in order.
  Score is sum of matched indices, lower is a better match. Scoring is case insensitive.
  Unicode NFKD normalization (see https://www.unicode.org/reports/tr15/) is also used to allow fuzzy matching against composite unicode glyphs.
    e.g. Poétrï decomposes to [ P o e <accent> t r i <umlaut> ]
  TODO: some cards use 1337, which we could account for too (e.g. D4v1d)"
  [input target]
  (let [input  (-> input  string/lower-case (.normalize "NFKD"))
        target (-> target string/lower-case (.normalize "NFKD"))]
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
              (+ score (or next-index 0)))))))))

(defn find-matches
  ([potential-matches pattern]
     (->> potential-matches
       (map (fn [target] {:match target :score (fuzzy-match-score pattern target)}))
       (filter :score)
       (sort-by :score)
       (take 10)
       (map :match))))

(defn show-completions? [s]
  (seq (:completions s)))

(defn reset-completions
  "Resets the command menu state."
  [state]
  (swap! state assoc :completions nil)
  (swap! state assoc :completion-highlight nil))

(defn fill-completion [state completion-text]
  (swap! state assoc :msg (str completion-text " "))
  (reset-completions state))

(defn is-command? [completion]
  (contains? command-info-map completion))

(defn has-args? [completion]
  (some? (get-in command-info-map [completion :has-args])))

(defn autosend? [completion]
  ;; Commands with arguments do not autosend
  (if (and (is-command? completion) (has-args? completion))
    false
    ;; Other completion types (commands with no args, card completions) do autosend
    true))

(defn completions-key-down-handler
  [state e]
  (when (show-completions? @state)
    (let [key (-> e .-key)
          completions (:completions @state)
          completions-count (count completions)]
      (case key
        ;; ArrowDown
        "ArrowDown" (do (.preventDefault e)
                        (swap! state update :completion-highlight #(if % (mod (inc %) completions-count) 0)))
        ;; ArrowUp
        "ArrowUp" (when (:completion-highlight @state)
                    (.preventDefault e)
                    (swap! state update :completion-highlight #(if % (mod (dec %) completions-count) 0)))
        ("Enter" " " "ArrowRight" "Tab")
        (when (or (= 1 completions-count) (:completion-highlight @state))
          (let [use-index (if (= 1 completions-count) 0 (:completion-highlight @state))
                completion (:completion-text (nth completions use-index))]
            (.preventDefault e)
            (fill-completion state completion)
            ;; auto send when no args needed
            (when (and (= key "Enter") (autosend? completion))
              (send-msg state))))
        ;; else
        nil))))

(defn complete-command [state input]
  (swap! state assoc :completions
         (->> (find-matches commands input)
              (mapv (fn [match] {:completion-text match :display-text (get-in command-info-map [match :usage])})))))

(defn filter-side [[card-name card-info]]
  (case (:side @game-state)
    :corp   (= (:side card-info) "Corp")
    :runner (= (:side card-info) "Runner")))

(defn complete-cardname [state full-input card-input]
  (let [cardnames (->> @all-cards
                       (filter filter-side)
                       keys)
        matches (find-matches cardnames card-input)
        complete #(string/replace full-input card-input %)]
    (swap! state assoc :completions
           (->> matches
                (mapv (fn [match] {:completion-text (complete match) :display-text match}))))))

(defn complete-identity [state full-input card-input]
  (let [cardnames (->> @all-cards
                       (filter filter-side)
                       (filter (fn [[_ {type :type}]] (= type "Identity")))
                       keys)
        matches (find-matches cardnames card-input)
        complete #(string/replace full-input card-input %)]
    (swap! state assoc :completions
           (->> matches
                (mapv (fn [match] {:completion-text (complete match) :display-text match}))))))

(defn log-input-change-handler
  [state e]
  (reset-completions state)
  (let [input (-> e .-target .-value)
        starts-with? #(string/starts-with? input %)]
    (cond
      (starts-with? "/summon ") (let [card (string/replace input #"/summon " "")]
                                     (complete-cardname state input card))
      (starts-with? "/replace-id ") (let [card (string/replace input #"/replace-id " "")]
                                         (complete-identity state input card))
      (= "/" (first input)) (complete-command state input))
     
    (swap! state assoc :msg input)))
  ;;(send-typing state)

(defn completions [!input-ref state]
  (when (show-completions? @state)
    [:div.command-matches-container.panel.blue-shade
     {:on-mouse-leave #(swap! state dissoc :completion-highlight)}
     [:ul.command-matches
      (doall (map-indexed
               (fn [i {:keys [completion-text display-text]} completion]
                 [:li.command-match
                  {:key completion-text
                   :class (when (= i (:completion-highlight @state)) "highlight")}
                  [:span {:on-mouse-over #(swap! state assoc :completion-highlight i)
                          :on-click #(do
                            (fill-completion state completion-text)
                            (.focus @!input-ref))}
                         display-text]])
               (:completions @state)))]]))

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
                                  (reset-completions state)
                                  (send-msg state))}
           [:input#log-input
            {:placeholder (tr [:chat_placeholder "Say something..."])
             :type "text"
             :autoComplete "off"
             :ref #(reset! !input-ref %)
             :value (:msg @state)
             ;;:on-blur #(send-typing (atom nil))
             :on-key-down #(completions-key-down-handler state %)
             :on-change #(log-input-change-handler state %)}]]]
         [indicate-action]
         [show-decklists]
         [completions !input-ref state]]))))

(defn format-system-timestamp [timestamp text corp runner]
  (if (get-in @app-state [:options :log-timestamps])
    (render-message (render-player-highlight text corp runner (str "[" (string/replace (.toLocaleTimeString (js/Date. timestamp)) #"\s\w*" "") "]")))
    (render-message (render-player-highlight text corp runner))
    )
  )

(defn format-user-timestamp [timestamp user]
  (if (get-in @app-state [:options :log-timestamps])
    [:div.timestamp-wrapper
     [:div.username (:username user)]
     [:div.timestamp "[" (string/replace (.toLocaleTimeString (js/Date. timestamp)) #"\s\w*" "") "]"]
     ]
    [:div.username (:username user)]
    )
  )

(defn log-messages []
  (let [log (r/cursor game-state [:log])
        corp (r/cursor game-state [:corp :user :username])
        runner (r/cursor game-state [:runner :user :username])
        !node-ref (r/atom nil)]
    (r/create-class
      {:display-name "log-messages"

       :component-did-mount
       (fn [_]
         (when (:update @should-scroll)
           (when-let [n @!node-ref]
             (set! (.-scrollTop n) (.-scrollHeight n)))))

       :component-will-update
       (fn [_]
         (when-let [n @!node-ref]
           (reset! should-scroll {:update (or (:send-msg @should-scroll)
                                              (scrolled-to-end? n 15))
                                  :send-msg false})))

       :component-did-update
       (fn [_]
         (when (:update @should-scroll)
           (when-let [n @!node-ref]
             (set! (.-scrollTop n) (.-scrollHeight n)))))

       :reagent-render
       (fn []
         (into [:div.messages {:class [(when (:replay @game-state)
                                         "panel-bottom")
                                       (player-highlight-option-class)]
                               :ref #(reset! !node-ref %)
                               :on-mouse-over #(card-preview-mouse-over % zoom-channel)
                               :on-mouse-out #(card-preview-mouse-out % zoom-channel)
                               :aria-live "polite"}]
               (map
                 (fn [{:keys [user text timestamp]}]
                   ^{:key timestamp}
                   (if (= user "__system__")
                      [:div.system
                        [format-system-timestamp timestamp text @corp @runner]]
                      [:div.message
                       [avatar user {:opts {:size 38}}]
                       [:div.content
                        [format-user-timestamp timestamp user]
                        [:div (render-message text)]]]))
                 @log)))})))

(defn log-pane []
  (fn []
    [:div.log
     ;; [angel-arena-log/inactivity-pane]
     [log-messages]
     [log-typing]
     [log-input]]))
