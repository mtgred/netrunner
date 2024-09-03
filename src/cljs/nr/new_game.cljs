(ns nr.new-game
  (:require
   [jinteki.utils :refer [str->int]]
   [jinteki.preconstructed :refer [all-matchups matchup-by-key]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [authenticated] :as auth]
   [nr.translations :refer [tr tr-string tr-format tr-side]]
   [nr.utils :refer [slug->format]]
   [nr.ws :as ws]
   [reagent.core :as r]))

(def new-game-keys
  [:allow-spectator
   :api-access
   :format
   :password
   :room
   :save-replay
   :side
   :singleton
   :spectatorhands
   :precon
   :gateway-type
   :timer
   :title])

(defn create-game [state lobby-state options]
  (authenticated
    (fn [_]
      (cond
        (empty? (:title @state))
        (swap! state assoc :flash-message (tr [:lobby.title-error "Please fill a game title."]))
        (and (:protected @options)
             (empty? (:password @options)))
        (swap! state assoc :flash-message (tr [:lobby.password-error "Please fill a password."]))
        :else
        (let [new-game (select-keys (merge @state @options) new-game-keys)]
          (swap! lobby-state assoc :editing false)
          (ws/ws-send! [:lobby/create new-game]))))))

(defn button-bar [state lobby-state options]
  [:div.button-bar
   [:button {:type "button"
             :on-click #(create-game state lobby-state options)}
    (tr [:lobby.create "Create"])]
   [:button {:type "button"
             :on-click #(do (.preventDefault %)
                            (swap! lobby-state assoc :editing false))}
    (tr [:lobby.cancel "Cancel"])]])

(defn title-section [title-state]
  [:section
   [:h3 (tr [:lobby.title "Title"])]
   [:input.game-title
    {:on-change #(reset! title-state (.. % -target -value))
     :value @title-state
     :placeholder (tr [:lobby.title "Title"])
     :maxLength "100"}]])

(defn side-section [side-state]
  [:section
   [:h3 (tr [:lobby.side "Side"])]
   (doall
     (for [option ["Any Side" "Corp" "Runner"]]
       ^{:key option}
       [:p
        [:label [:input
                 {:type "radio"
                  :name "side"
                  :value option
                  :on-change #(reset! side-state (.. % -target -value))
                  :checked (= @side-state option)}]
         (tr-side option)]]))])

(defn singleton-only [options fmt-state]
  [:label
   [:input {:type "checkbox" :checked (:singleton @options)
            :on-change #(swap! options assoc :singleton (.. % -target -checked))}]
   (tr [:lobby.singleton "Singleton"])])

(defn gateway-constructed-choice [fmt-state gateway-type]
  [:div
   {:style {:display (if (= @fmt-state "system-gateway") "block" "none")}}
   (doall
     (for [option ["Beginner" "Intermediate" "Constructed"]]
       ^{:key option}
       [:span [:label [:input
                       {:type "radio"
                        :name "gateway-type"
                        :value option
                        :on-change #(reset! gateway-type (.. % -target -value))
                        :checked (= @gateway-type option)}]
              (str (tr-string "lobby.gateway-format" option) "    ")]]))])

(defn precon-choice [fmt-state precon]
  [:div
   {:style {:display (if (= @fmt-state "preconstructed") "block" "none")}}
   [:div
    [:label "Matchup:    "]
    [:select.precon
     {:value (or @precon "worlds-2012-a")
      :on-change #(reset! precon (.. % -target -value))}
     (doall
       (for [matchup (sort all-matchups)]
         ^{:key (name matchup)}
         [:option {:value (name matchup)} (tr (:tr-inner (matchup-by-key matchup)))]))]]])

(defn format-section [fmt-state options gateway-type precon]
  [:section
   [:h3 (tr [:lobby.default-game-format "Default game format"])]
   [:select.format
    {:value (or @fmt-state "standard")
     :on-change #(reset! fmt-state (.. % -target -value))}
    (doall
      (for [[k v] slug->format]
        ^{:key k}
        [:option {:value k} (tr-format v)]))]
   [singleton-only options fmt-state]
   [gateway-constructed-choice fmt-state gateway-type]
   [precon-choice fmt-state precon]
   [:div.infobox.blue-shade
    {:style {:display (if (:singleton @options) "block" "none")}}
    [:p (tr [:lobby.singleton-details "This will restrict decklists to only those which do not contain any duplicate cards. It is recommended you use the listed singleton-based identities."])]
    [:p (tr [:lobby.singleton-example "1) Nova Initiumia: Catalyst & Impetus 2) Ampere: Cybernetics For Anyone"])]]])

(defn allow-spectators [options]
  [:p
    [:label
     [:input {:type "checkbox" :checked (:allow-spectator @options)
              :on-change #(swap! options assoc :allow-spectator (.. % -target -checked))}]
     (tr [:lobby.spectators "Allow spectators"])]])

(defn toggle-hidden-info [options]
  [:<>
   [:p
    [:label
     [:input {:type "checkbox" :checked (:spectatorhands @options)
              :on-change #(swap! options assoc :spectatorhands (.. % -target -checked))
              :disabled (not (:allow-spectator @options))}]
     (tr [:lobby.hidden "Make players' hidden information visible to spectators"])]]
   [:div.infobox.blue-shade
    {:style {:display (if (:spectatorhands @options) "block" "none")}}
    [:p (tr [:lobby.hidden-details "This will reveal both players' hidden information to ALL spectators of your game, including hand and face-down cards."])]
    [:p (tr [:lobby.hidden-password "We recommend using a password to prevent strangers from spoiling the game."])]]])

(defn password-input [options]
  [:<>
   [:p
    [:label
     [:input {:type "checkbox" :checked (:private @options)
              :on-change #(let [checked (.. % -target -checked)]
                            (swap! options assoc :protected checked)
                            (when (not checked)
                              (swap! options assoc :password "")))}]
     (tr [:lobby.password-protected "Password protected"])]]
   (when (:protected @options)
     [:p
      [:input.game-title {:on-change #(swap! options assoc :password (.. % -target -value))
                          :value (:password @options)
                          :placeholder (tr [:lobby.password "Password"])
                          :maxLength "30"}]])])

(defn add-timer [options]
  [:<>
   (when-not (= "casual" (:room @options))
     [:p
      [:label
       [:input {:type "checkbox"
                :checked (:timed @options)
                :on-change #(let [checked (.. % -target -checked)]
                              (swap! options assoc :timed checked)
                              (swap! options assoc :timer (if checked 35 nil)))}]
       (tr [:lobby.timed-game "Start with timer"])]])
   (when (:timed @options)
     [:p
      [:input.game-title {:on-change #(let [value (str->int (.. % -target -value))]
                                        (when-not (js/isNaN value)
                                          (swap! options assoc :timer value)))
                          :type "number"
                          :value (:timer @options)
                          :placeholder (tr [:lobby.timer-length "Timer length (minutes)"])}]])
   [:div.infobox.blue-shade
    {:style {:display (if (:timed @options) "block" "none")}}
    [:p (tr [:lobby.timed-game-details "Timer is only for convenience: the game will not stop when timer runs out."])]]])

(defn save-replay [options]
  [:<>
   [:p
    [:label
     [:input {:type "checkbox"
              :checked (:save-replay @options)
              :on-change #(swap! options assoc :save-replay (.. % -target -checked))}]
     (str "🟢 " (tr [:lobby.save-replay "Save replay"]))]]
   [:div.infobox.blue-shade
    {:style {:display (if (:save-replay @options) "block" "none")}}
    [:p (tr [:lobby.save-replay-details "This will save a replay file of this match with open information (e.g. open cards in hand). The file is available only after the game is finished."])]
    [:p (tr [:lobby.save-replay-unshared "Only your latest 15 unshared games will be kept, so make sure to either download or share the match afterwards."])]
    [:p (tr [:lobby.save-replay-beta "BETA Functionality: Be aware that we might need to reset the saved replays, so make sure to download games you want to keep. Also, please keep in mind that we might need to do future changes to the site that might make replays incompatible."])]]])

(defn api-access [options user]
  [:<>
   (let [has-keys (:has-api-keys @user false)]
     [:p
      [:label
       [:input {:disabled (not has-keys)
                :type "checkbox"
                :checked (:api-access @options)
                :on-change #(swap! options assoc :api-access (.. % -target -checked))}]
       (tr [:lobby.api-access "Allow API access to game information"])
       (when (not has-keys)
         (str " " (tr [:lobby.api-requires-key "(Requires an API Key in Settings)"])))]])
   [:div.infobox.blue-shade
    {:style {:display (if (:api-access @options) "block" "none")}}
    [:p (tr [:lobby.api-access-details "This allows access to information about your game to 3rd party extensions. Requires an API Key to be created in Settings."])]]])

(defn options-section [options user]
  [:section
   [:h3 (tr [:lobby.options "Options"])]
   [allow-spectators options]
   [toggle-hidden-info options]
   [password-input options]
   [add-timer options]
   [save-replay options]
   [api-access options user]])

(defn create-new-game [lobby-state user]
  (r/with-let [state (r/atom {:flash-message ""
                              :format (or (get-in @app-state [:options :default-format]) "standard")
                              :room (:room @lobby-state)
                              :side "Any Side"
                              :gateway-type "Beginner"
                              :precon "worlds-2012-a"
                              :title (str (:username @user) "'s game")})
               options (r/atom {:allow-spectator true
                                :api-access false
                                :password ""
                                :protected false
                                :save-replay (not= "casual" (:room @lobby-state))
                                :singleton false
                                :spectatorhands false
                                :timed false
                                :timer nil})
               title (r/cursor state [:title])
               side (r/cursor state [:side])
               precon (r/cursor state [:precon])
               gateway-type (r/cursor state [:gateway-type])
               fmt (r/cursor state [:format])
               flash-message (r/cursor state [:flash-message])]
    (fn [lobby-state user]
      [:div
       [button-bar state lobby-state options]
       (when-let [message @flash-message]
         [:p.flash-message message])
       [:div.content
        [title-section title]
        [side-section side]
        [format-section fmt options gateway-type precon]
        [options-section options user]]])))
