(ns nr.new-game
  (:require
   [jinteki.utils :refer [str->int]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [authenticated] :as auth]
   [nr.translations :refer [tr tr-format tr-side]]
   [nr.utils :refer [slug->format]]
   [nr.ws :as ws]))

(defn start-replay [s]
  (let [reader (js/FileReader.)
        file (:replay-file s)
        onload (fn [onload-ev] (let [replay (-> onload-ev .-target .-result)
                                     replay (js->clj (.parse js/JSON replay) :keywordize-keys true)
                                     history (:history replay)
                                     init-state (first history)
                                     init-state (assoc-in init-state [:options :spectatorhands] true)
                                     diffs (rest history)
                                     init-state (assoc init-state :replay-diffs diffs :gameid "local-replay")]
                                 (ws/event-msg-handler
                                   {:id :netrunner/start
                                    :?data (.stringify js/JSON (clj->js init-state))})))]
    (aset reader "onload" onload)
    (.readAsText reader file)))

(defn new-game [s]
  (authenticated
    (fn [user]
      (let [fmt (:format (:create-game-deck @app-state) "standard")
            side (:side (:identity (:create-game-deck @app-state)) "Any Side")]
        (swap! s assoc
               :title (str (:username user) "'s game")
               :side side
               :format fmt
               :editing true
               :replay false
               :save-replay (if (= "casual" (:room @s)) false true)
               :api-access false
               :flash-message ""
               :protected false
               :password ""
               :timed false
               :timer nil
               :allow-spectator true
               :spectatorhands false
               :create-game-deck (:create-game-deck @app-state))
        (swap! app-state assoc :editing-game true)
        (swap! app-state dissoc :create-game-deck)
        (-> ".game-title" js/$ .select)))))

(def new-game-keys
  [:allow-spectator
   :api-access
   :format
   :password
   :room
   :save-replay
   :side
   :spectatorhands
   :timer
   :title])

(defn create-game [s]
  (authenticated
    (fn [_]
      (if (:replay @s)
        (cond
          (not (:replay-file @s))
          (swap! s assoc :flash-message (tr [:lobby.replay-invalid-file "Select a valid replay file."]))
          :else
          (do (swap! s assoc :editing false)
              (start-replay @s)))
        (cond
          (empty? (:title @s))
          (swap! s assoc :flash-message (tr [:lobby.title-error "Please fill a game title."]))
          (and (:protected @s)
               (empty? (:password @s)))
          (swap! s assoc :flash-message (tr [:lobby.password-error "Please fill a password."]))
          :else
          (do (swap! s assoc :editing false)
              (swap! app-state dissoc :editing-game)
              (ws/ws-send! [:lobby/create (select-keys @s new-game-keys)])))))))

(defn create-new-game [s user]
  (if (:replay @s)
    [:div
     [:div.button-bar
      [:button {:type "button"
                :on-click #(create-game s)} (tr [:lobby.start-replay "Start replay"])]
      [:button {:type "button"
                :on-click #(do
                             (swap! s assoc :editing false)
                             (swap! app-state dissoc :editing-game))}
       (tr [:lobby.cancel "Cancel"])]]
     (when-let [flash-message (:flash-message @s)]
       [:p.flash-message flash-message])
     [:div [:input {:field :file
                    :type :file
                    :on-change #(swap! s assoc :replay-file (aget (.. % -target -files) 0))}]]]
    [:div
     [:div.button-bar
      [:button {:type "button"
                :on-click #(create-game s)} (tr [:lobby.create "Create"])]
      [:button {:type "button"
                :on-click #(swap! s assoc :editing false)} (tr [:lobby.cancel "Cancel"])]]
     (when-let [flash-message (:flash-message @s)]
       [:p.flash-message flash-message])
     [:div.content
      [:section
       [:h3 (tr [:lobby.title "Title"])]
       [:input.game-title {:on-change #(swap! s assoc :title (.. % -target -value))
                           :value (:title @s)
                           :placeholder (tr [:lobby.title "Title"])
                           :maxLength "100"}]]
      [:section
       [:h3 (tr [:lobby.side "Side"])]
       (doall
         (for [option ["Any Side" "Corp" "Runner"]]
           ^{:key option}
           [:p
            [:label [:input {:type "radio"
                             :name "side"
                             :value option
                             :on-change #(swap! s assoc :side (.. % -target -value))
                             :checked (= (:side @s) option)}]
             (tr-side option)]]))]

      [:section
       [:h3 (tr [:lobby.format "Format"])]
       [:select.format {:value (:format @s "standard")
                        :on-change #(swap! s assoc :format (.. % -target -value))}
        (doall (for [[k v] slug->format]
                 ^{:key k}
                 [:option {:value k} (tr-format v)]))]]

      [:section
       [:h3 (tr [:lobby.options "Options"])]
       [:p
        [:label
         [:input {:type "checkbox" :checked (:allow-spectator @s)
                  :on-change #(swap! s assoc :allow-spectator (.. % -target -checked))}]
         (tr [:lobby.spectators "Allow spectators"])]]
       [:p
        [:label
         [:input {:type "checkbox" :checked (:spectatorhands @s)
                  :on-change #(swap! s assoc :spectatorhands (.. % -target -checked))
                  :disabled (not (:allow-spectator @s))}]
         (tr [:lobby.hidden "Make players' hidden information visible to spectators"])]]
       [:div.infobox.blue-shade {:style {:display (if (:spectatorhands @s) "block" "none")}}
        [:p "This will reveal both players' hidden information to ALL spectators of your game, "
         "including hand and face-down cards."]
        [:p "We recommend using a password to prevent strangers from spoiling the game."]]
       [:p
        [:label
         [:input {:type "checkbox" :checked (:private @s)
                  :on-change #(let [checked (.. % -target -checked)]
                                (swap! s assoc :protected checked)
                                (when (not checked) (swap! s assoc :password "")))}]
         (tr [:lobby.password-protected "Password protected"])]]
       (when (:protected @s)
         [:p
          [:input.game-title {:on-change #(swap! s assoc :password (.. % -target -value))
                              :type "password"
                              :value (:password @s)
                              :placeholder (tr [:lobby.password "Password"])
                              :maxLength "30"}]])

       (when-not (= "casual" (:room @s))
         [:p
          [:label
           [:input {:type "checkbox" :checked (:timed @s)
                    :on-change #(let [checked (.. % -target -checked)]
                                  (swap! s assoc :timed checked)
                                  (swap! s assoc :timer (if checked 35 nil)))}]
           (tr [:lobby.timed-game "Start with timer"])]])
       (when (:timed @s)
         [:p
          [:input.game-title {:on-change #(swap! s assoc :timer (-> % (.. -target -value) str->int))
                              :type "number"
                              :value (:timer @s)
                              :placeholder (tr [:lobby.timer-length "Timer length (minutes)"])}]])
       [:div.infobox.blue-shade {:style {:display (if (:timed @s) "block" "none")}}
        [:p "Timer is only for convenience: the game will not stop when timer runs out."]]

       [:p
        [:label
         [:input {:type "checkbox" :checked (:save-replay @s)
                  :on-change #(swap! s assoc :save-replay (.. % -target -checked))}]
         (str "🟢 " (tr [:lobby.save-replay "Save replay"]))]]
       [:div.infobox.blue-shade {:style {:display (if (:save-replay @s) "block" "none")}}
        [:p "This will save a replay file of this match with open information (e.g. open cards in hand)."
         " The file is available only after the game is finished."]
        [:p "Only your latest 15 unshared games will be kept, so make sure to either download or share the match afterwards."]
        [:p [:b "BETA Functionality:"] " Be aware that we might need to reset the saved replays, so " [:b "make sure to download games you want to keep."]
         " Also, please keep in mind that we might need to do future changes to the site that might make replays incompatible."]]

       (let [has-keys (:has-api-keys @user false)]
         [:p
          [:label
           [:input {:disabled (not has-keys)
                    :type "checkbox" :checked (:api-access @s)
                    :on-change #(swap! s assoc :api-access (.. % -target -checked))}]
           (tr [:lobby.api-access "Allow API access to game information"])
           (when (not has-keys)
             (str " " (tr [:lobby.api-requires-key "(Requires an API Key in Settings)"])))]])
       [:div.infobox.blue-shade {:style {:display (if (:api-access @s) "block" "none")}}
        [:p "This allows access to information about your game to 3rd party extensions. Requires an API Key to be created in Settings"]]]]]))
