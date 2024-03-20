(ns nr.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!] :as async]
   [clojure.string :as s]
   [goog.dom :as gdom]
   [jinteki.cards :refer [all-cards]]
   [nr.ajax :refer [DELETE GET POST PUT]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [valid-email?]]
   [nr.avatar :refer [avatar]]
   [nr.translations :refer [tr tr-format]]
   [nr.utils :refer [format-date-time ISO-ish-formatter non-game-toast
                     set-scroll-top slug->format store-scroll-top]]
   [reagent-modals.modals :as reagent-modals]
   [reagent.core :as r]))

(defn post-response [s response]
  (case (:status response)
    401 (non-game-toast  (tr [:settings.invalid-password "Invalid login or password"]) "error" nil)
    404 (non-game-toast  (tr [:settings.invalid-email "No account with that email address exists"]) "error" nil)
    ;; else
    (non-game-toast (tr [:settings.updated "Profile updated - Please refresh your browser"]) "success" nil))
  (swap! s assoc :flash-message ""))

(defn post-options [url callback]
  (let [params (:options @app-state)]
    (go (let [response (<! (PUT url params :json))]
          (callback response)))))

(defn handle-post [event url s]
  (.preventDefault event)
  (swap! s assoc :flash-message (tr [:settings.updating "Updating profile..."]))
  (swap! app-state assoc-in [:options :pronouns] (:pronouns @s))
  (swap! app-state assoc-in [:options :language] (:language @s))
  (swap! app-state assoc-in [:options :sounds] (:sounds @s))
  (swap! app-state assoc-in [:options :default-format] (:default-format @s))
  (swap! app-state assoc-in [:options :lobby-sounds] (:lobby-sounds @s))
  (swap! app-state assoc-in [:options :sounds-volume] (:volume @s))
  (swap! app-state assoc-in [:options :background] (:background @s))
  (swap! app-state assoc-in [:options :custom-bg-url] (:custom-bg-url @s))
  (swap! app-state assoc-in [:options :card-back] (:card-back @s))
  (swap! app-state assoc-in [:options :card-zoom] (:card-zoom @s))
  (swap! app-state assoc-in [:options :pin-zoom] (:pin-zoom @s))
  (swap! app-state assoc-in [:options :show-alt-art] (:show-alt-art @s))
  (swap! app-state assoc-in [:options :card-resolution] (:card-resolution @s))
  (swap! app-state assoc-in [:options :player-stats-icons] (:player-stats-icons @s))
  (swap! app-state assoc-in [:options :stacked-cards] (:stacked-cards @s))
  (swap! app-state assoc-in [:options :sides-overlap] (:sides-overlap @s))
  (swap! app-state assoc-in [:options :runner-board-order] (:runner-board-order @s))
  (swap! app-state assoc-in [:options :log-width] (:log-width @s))
  (swap! app-state assoc-in [:options :log-top] (:log-top @s))
  (swap! app-state assoc-in [:options :log-player-highlight] (:log-player-highlight @s))
  (swap! app-state assoc-in [:options :blocked-users] (:blocked-users @s))
  (swap! app-state assoc-in [:options :alt-arts] (:alt-arts @s))
  (swap! app-state assoc-in [:options :gamestats] (:gamestats @s))
  (swap! app-state assoc-in [:options :deckstats] (:deckstats @s))
  (.setItem js/localStorage "sounds" (:sounds @s))
  (.setItem js/localStorage "default-format" (:default-format @s))
  (.setItem js/localStorage "lobby_sounds" (:lobby-sounds @s))
  (.setItem js/localStorage "custom_bg_url" (:custom-bg-url @s))
  (.setItem js/localStorage "sounds_volume" (:volume @s))
  (.setItem js/localStorage "log-width" (:log-width @s))
  (.setItem js/localStorage "log-top" (:log-top @s))
  (.setItem js/localStorage "log-player-highlight" (:log-player-highlight @s))
  (.setItem js/localStorage "player-stats-icons" (:player-stats-icons @s))
  (.setItem js/localStorage "stacked-cards" (:stacked-cards @s))
  (.setItem js/localStorage "sides-overlap" (:sides-overlap @s))
  (.setItem js/localStorage "runner-board-order" (:runner-board-order @s))
  (.setItem js/localStorage "card-back" (:card-back @s))
  (.setItem js/localStorage "card-zoom" (:card-zoom @s))
  (.setItem js/localStorage "pin-zoom" (:pin-zoom @s))
  (post-options url (partial post-response s)))

(defn add-user-to-block-list
  [user s]
  (let [blocked-user (:block-user-input @s)
        my-user-name (:username user)
        current-blocked-list (:blocked-users @s)]
    (swap! s assoc-in [:block-user-input] "")
    (when (and (not (s/blank? blocked-user))
               (not= my-user-name blocked-user)
               (= -1 (.indexOf current-blocked-list blocked-user)))
      (swap! s assoc-in [:blocked-users] (conj current-blocked-list blocked-user)))))

(defn remove-user-from-block-list
  [evt s]
  (let [currElt (.-currentTarget evt)
        next-sib (gdom/getNextElementSibling currElt)
        user-name (gdom/getTextContent next-sib)
        current-blocked-list (:blocked-users @s)]
    (when user-name
      (swap! s assoc-in [:blocked-users] (vec (remove #(= % user-name) current-blocked-list))))))

(defn- all-alt-art-types
  []
  (map :version (:alt-info @app-state)))

(defn alt-art-name
  [version]
  (let [alt (first (filter #(= (name version) (:version %)) (:alt-info @app-state)))]
    (get alt :name "Official")))

(defn- remove-card-art
  [card s]
  (swap! s update :alt-arts dissoc (keyword (:code card))))

(defn- add-card-art
  [card art s]
  (swap! s update :alt-arts assoc (keyword (:code card)) art))

(defn- art-available
  "Is the given art type available for the player's language and resolution settings?"
  [card art lang res]
  (get-in card [:images lang res (keyword art)]))

(defn- update-card-art
  "Set the alt art for a card"
  [card art lang res s]
  (when (and card (string? art))
    (if (= "default" art)
      (remove-card-art card s)
      (when (art-available card art lang res)
        (add-card-art card art s)))))

(defn- clear-card-art [s]
  (swap! s assoc-in [:alt-arts] {}))

(defn- reset-card-art [s]
  (let [art (:all-art-select @s)
        lang (keyword (get-in @app-state [:options :language] "en"))
        res (keyword (get-in @app-state [:options :card-resolution] "default"))]
    (doseq [card (vals @all-cards)]
      (update-card-art card art lang res s))))

(defn log-width-option [s]
  (let [log-width (r/atom (:log-width @s))]
    (fn []
      [:div
       [:input {:type "number"
                :min 100 :max 2000
                :on-change #(do (swap! s assoc-in [:log-width] (.. % -target -value))
                                (reset! log-width (.. % -target -value)))
                :value @log-width}]
       [:button.update-log-width {:type "button"
                                  :on-click #(do (swap! s assoc-in [:log-width] (get-in @app-state [:options :log-width]))
                                                 (reset! log-width (get-in @app-state [:options :log-width])))}
        (tr [:settings.get-log-width "Get current log width"])]])))

(defn log-top-option [s]
  (let [log-top (r/atom (:log-top @s))]
    (fn []
      [:div
       [:input {:type "number"
                :min 100 :max 2000
                :on-change #(do (swap! s assoc-in [:log-top] (.. % -target -value))
                                (reset! log-top (.. % -target -value)))
                :value @log-top}]
       [:button.update-log-width {:type "button"
                                  :on-click #(do (swap! s assoc-in [:log-top] (get-in @app-state [:options :log-top]))
                                                 (reset! log-top (get-in @app-state [:options :log-top])))}
        (tr [:settings.get-log-top "Get current log top"])]])))

(defn change-email [_]
  (let [email-state (r/atom {:flash-message ""
                             :email ""})]
    (fn [s]
      [:div
       [:h3 (tr [:settings.email-title "Change email address"])]
       [:p.flash-message (:flash-message @email-state)]
       [:form {:on-submit (fn [event]
                            (.preventDefault event)
                            (let [email (:email @email-state)]
                              (when (valid-email? email)
                                (go (let [{status             :status
                                           {message :message} :json} (<! (PUT "/profile/email" {:email email} :json))]
                                      (if (= 200 status)
                                        (-> js/document .-location (.reload true))
                                        (swap! email-state assoc :flash-message message)))))))}
        (when-let [email (:email @s)]
          [:p [:label.email (tr [:settings.current-email "Current email"]) ": "]
           [:input.email {:type "text"
                          :value email
                          :name "current-email"
                          :read-only true}]])
        [:p [:label.email (tr [:settings.desired-email "Desired email"]) ": "]
         [:input.email {:type "text"
                        :placeholder (tr [:settings.email-placeholder "Email address"])
                        :name "email"
                        :on-change #(let [value (-> % .-target .-value)]
                                      (swap! email-state assoc :email value))
                        :on-blur #(if (valid-email? (-> % .-target .-value))
                                    (swap! email-state assoc :flash-message "")
                                    (swap! email-state assoc
                                           :flash-message (tr [:settings.enter-valid "Please enter a valid email address"])))}]]
        [:p.float-right
         (let [disabled (not (valid-email? (:email @email-state)))]
           [:button
            {:disabled disabled
             :class (when disabled "disabled")}
            (tr [:settings.update "Update"])])
         [:button {:on-click #(do (.preventDefault %)
                                  (reagent-modals/close-modal!))}
          (tr [:settings.cancel "Cancel"])]]]])))

(defn- update-api-keys-response [response s]
  (let [status (:status response)]
    (if (or (= 200 status)
            (= 201 status))
      (do
        (go (swap! s assoc :api-keys (:json (<! (GET "/data/api-keys")))))
        (non-game-toast "Updated API Keys" "success" nil))
      (non-game-toast "Failed to update API Keys" "error" nil))))

(defn- delete-api-key [id s]
  (go (let [response (<! (DELETE (str "/data/api-keys/" id)))]
        (update-api-keys-response response s))))

(defn- create-api-key [s]
  (go (let [response (<! (POST "/data/api-keys" {} :json))]
        (update-api-keys-response response s))))

(defn- api-keys [s]
  (r/with-let [keys-cursor (r/cursor s [:api-keys])]
    [:section
     [:h3 (tr [:settings.api-keys "API Keys"])]
     [:div.news-box.panel.blue-shade
      [:ul.list
       (doall
         (for [d @keys-cursor]
           [:li.news-item
            {:key (:_id d)}
            [:span
             [:button.delete
              {:on-click #(do (.preventDefault %)
                              (delete-api-key (:_id d) s))}
              (tr [:settings.delete-api-key "Delete"])]]
            [:span.date
             (format-date-time ISO-ish-formatter (:date d))]
            [:span.title (:api-key d "")]]))]]
     [:button {:on-click #(do (.preventDefault %)
                              (create-api-key s))}
      (tr [:settings.create-api-key "Create API Key"])]]))

(defn account-content [_ _ scroll-top]
  (r/create-class
    {:display-name "account-content"
     :component-did-mount #(set-scroll-top % @scroll-top)
     :component-will-unmount #(store-scroll-top % scroll-top)
     :reagent-render
     (fn [user s _]
       [:div#profile-form.panel.blue-shade.content-page {:ref "profile-form"}
         [:h2 (tr [:nav.settings "Settings"])]
         [:form {:on-submit #(handle-post % "/profile" s)}
          [:section
           [:h3 (tr [:settings.email "Email"])]
           [:a {:href "" :on-click #(do
                                      (.preventDefault %)
                                      (reagent-modals/modal! [change-email s]))} (tr [:settings.change-email "Change email"])]]
          [:section
           [:h3 (tr [:settings.avatar "Avatar"])]
           [avatar @user {:opts {:size 38}}]
           [:a {:href "http://gravatar.com" :target "_blank"} (tr [:settings.change-avatar "Change on gravatar.com"])]]
          [:section
           [:h3 (tr [:settings.pronouns "Pronouns"])]
           [:select {:value (:pronouns @s "none")
                     :on-change #(swap! s assoc :pronouns (.. % -target -value))}
            (doall
              (for [option [{:name (tr [:pronouns.none "Unspecified"]) :ref "none"}
                            {:name (tr [:pronouns.any "Any"]) :ref "any"}
                            {:name (tr [:pronouns.myodb "Prefer not to say"]) :ref "myodb"}
                            {:name (tr [:pronouns.blank "[blank]"]) :ref "blank"}
                            {:name (tr [:pronouns.they "They/them"]) :ref "they"}
                            {:name (tr [:pronouns.she "She/her"]) :ref "she"}
                            {:name (tr [:pronouns.shethey "She/they"]) :ref "shethey"}
                            {:name (tr [:pronouns.he "He/him"]) :ref "he"}
                            {:name (tr [:pronouns.hethey "He/they"]) :ref "hethey"}
                            {:name (tr [:pronouns.it "It"]) :ref "it"}
                            {:name (tr [:pronouns.ne "Ne/nem"]) :ref "ne"}
                            {:name (tr [:pronouns.ve "Ve/ver"]) :ref "ve"}
                            {:name (tr [:pronouns.ey "Ey/em"]) :ref "ey"}
                            {:name (tr [:pronouns.zehir "Ze/hir"]) :ref "zehir"}
                            {:name (tr [:pronouns.zezir "Ze/zir"]) :ref "zezir"}
                            {:name (tr [:pronouns.xe "Xe/xem"]) :ref "xe"}]]
                [:option {:value (:ref option) :key (:ref option)} (:name option)]))]]
          [:section
           [:h3 (tr [:settings.language "Language"])]
           [:select {:value (:language @s "en")
                     :on-change #(swap! s assoc :language (.. % -target -value))}
            (doall
              (for [option [{:name "English" :ref "en"}
                            {:name "中文 (Simplified)" :ref "zh-simp"}
                            {:name "中文 (Traditional)" :ref "zh-trad"}
                            {:name "Français" :ref "fr"}
                            {:name "Deutsch" :ref "de"}
                            {:name "Italiano" :ref "it"}
                            {:name "日本語" :ref "ja"}
                            {:name "한국어" :ref "ko"}
                            {:name "Polski" :ref "pl"}
                            {:name "Igpay Atinlay" :ref "la-pig"}
                            {:name "Português" :ref "pt"}
                            {:name "Русский" :ref "ru"}]]
                [:option {:value (:ref option) :key (:ref option)} (:name option)]))]]
          [:section
           [:h3 (tr [:settings.sounds "Sounds"])]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:lobby-sounds @s)
                             :on-change #(swap! s assoc-in [:lobby-sounds] (.. % -target -checked))}]
             (tr [:settings.enable-lobby-sounds "Enable lobby sounds"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:sounds @s)
                             :on-change #(swap! s assoc-in [:sounds] (.. % -target -checked))}]
             (tr [:settings.enable-game-sounds "Enable game sounds"])]]
           [:div (tr [:settings.volume "Volume"])
            [:input {:type "range"
                     :min 1 :max 100 :step 1
                     :on-change #(swap! s assoc-in [:volume] (.. % -target -value))
                     :value (or (:volume @s) 50)
                     :disabled (not (or (:sounds @s) (:lobby-sounds @s)))}]]]

        [:section
         [:h3 (tr [:lobby.format "Default game format"])]
         [:select.format
          {:value (or (:default-format @s) "standard")
           :on-change #(swap! s assoc-in [:default-format] (.. % -target -value))}
          (doall
           (for [[k v] slug->format]
             ^{:key k}
             [:option {:value k} (tr-format v)]))]]

          [:section
           [:h3 (tr [:settings.layout-options "Layout options"])]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:player-stats-icons @s)
                             :on-change #(swap! s assoc-in [:player-stats-icons] (.. % -target -checked))}]
             (tr [:settings.player-stats-icons "Use icons for player stats"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:stacked-cards @s)
                             :on-change #(swap! s assoc-in [:stacked-cards] (.. % -target -checked))}]
             (tr [:settings.stacked-cards "Card stacking (on by default)"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:sides-overlap @s)
                             :on-change #(swap! s assoc-in [:sides-overlap] (.. % -target -checked))}]
             (tr [:settings.sides-overlap "Runner and Corp board may overlap"])]]

           [:br]
           [:h4 (tr [:settings.runner-layout "Runner layout from Corp perspective"])]
           [:div
            [:div.radio
             [:label [:input {:name "runner-board-order"
                              :type "radio"
                              :value "jnet"
                              :checked (= "jnet" (:runner-board-order @s))
                              :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
              (tr [:settings.runner-classic "Runner rig layout is classic jnet (Top to bottom: Programs, Hardware, Resources)"])]]

            [:div.radio
             [:label [:input {:name "runner-board-order"
                              :type "radio"
                              :value "irl"
                              :checked (= "irl" (:runner-board-order @s))
                              :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
              (tr [:settings.runner-reverse "Runner rig layout is reversed (Top to bottom: Resources, Hardware, Programs)"])]]]

           [:br]
           [:h4 (tr [:settings.log-size "Log size"])]
           [:div
            [log-width-option s]
            [log-top-option s]]
           [:br]
           [:h4 (tr [:settings.log-player-highlight "Log player highlight"])]
           [:div
            [:div.radio
             [:label [:input {:name "log-player-highlight"
                              :type "radio"
                              :value "blue-red"
                              :checked (= "blue-red" (:log-player-highlight @s))
                              :on-change #(swap! s assoc :log-player-highlight (.. % -target -value))}]
              (tr [:settings.log-player-highlight-red-blue "Corp: Blue / Runner: Red"])]]

            [:div.radio
             [:label [:input {:name "log-player-highlight"
                              :type "radio"
                              :value "none"
                              :checked (= "none" (:log-player-highlight @s))
                              :on-change #(swap! s assoc :log-player-highlight (.. % -target -value))}]
              (tr [:settings.log-player-highlight-none "None"])]]]]

          (let [custom-bg-selected (= (:background @s) "custom-bg" )
                custom-bg-url (r/atom (:custom-bg-url @s))]
            [:section
             [:h3  (tr [:settings.background "Game board background"])]
             (doall (for [option [{:name "The Root"        :ref "lobby-bg"}
                                  {:name "Freelancer"      :ref "freelancer-bg"}
                                  {:name "Mushin No Shin"  :ref "mushin-no-shin-bg"}
                                  {:name "Traffic Jam"     :ref "traffic-jam-bg"}
                                  {:name "Rumor Mill"      :ref "rumor-mill-bg"}
                                  {:name "Find The Truth"  :ref "find-the-truth-bg"}
                                  {:name "Push Your Luck"  :ref "push-your-luck-bg"}
                                  {:name "Apex"            :ref "apex-bg"}
                                  {:name "Worlds 2020"     :ref "worlds2020"}
                                  {:name "Monochrome"      :ref "monochrome-bg"}
                                  {:name (str "Custom BG" (when custom-bg-selected " (input URL below)"))
                                   :ref "custom-bg"}]]
                      [:div.radio {:key (:name option)}
                       [:label [:input {:type "radio"
                                        :name "background"
                                        :value (:ref option)
                                        :on-change #(swap! s assoc-in [:background] (.. % -target -value))
                                        :checked (= (:background @s) (:ref option))}]
                        (:name option)]]))

             [:div [:input {:type "text"
                            :hidden (not custom-bg-selected)
                            :on-change #(do (swap! s assoc-in [:custom-bg-url] (.. % -target -value))
                                            (reset! custom-bg-url (.. % -target -value)))
                            :value @custom-bg-url}]]])

          [:section
           [:h3  (tr [:settings.card-backs "Card backs"])]
           (doall (for [option [{:name (tr [:settings.nsg "NSG"]) :ref "nsg"}
                                {:name (tr [:settings.ffg "FFG"]) :ref "ffg"}]]
                    [:div.radio {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "card-back"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc :card-back (.. % -target -value))
                                      :checked (= (:card-back @s) (:ref option))}]
                      (:name option)]]))]

          [:section
           [:h3  "Card preview zoom"]
           (doall (for [option [{:name "Card Image" :ref "image"}
                                {:name "Card Text" :ref "text"}]]
                    [:div.radio {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "card-zoom"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc :card-zoom (.. % -target -value))
                                      :checked (= (:card-zoom @s) (:ref option))}]
                      (:name option)]]))
           [:br]
           [:div
            [:label [:input {:type "checkbox"
                             :name "pin-zoom"
                             :checked (:pin-zoom @s)
                             :on-change #(swap! s assoc-in [:pin-zoom] (.. % -target -checked))}]
             (tr [:settings.pin-zoom "Keep zoomed cards on screen"])]]]

          [:section
           [:h3 (tr [:settings.game-stats " Game Win/Lose statistics "])]
           (doall (for [option [{:name (tr [:settings.always "Always"])                      :ref "always"}
                                {:name (tr [:settings.comp-only "Competitive Lobby Only"])   :ref "competitive"}
                                {:name (tr [:settings.none "None"])                          :ref "none"}]]
                    [:div {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "gamestats"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc-in [:gamestats] (.. % -target -value))
                                      :checked (= (:gamestats @s) (:ref option))}]
                      (:name option)]]))]

          [:section
           [:h3 (tr [:settings.deck-stats " Deck statistics "])]
           (doall (for [option [{:name (tr [:settings.always "Always"])                      :ref "always"}
                                {:name (tr [:settings.comp-only "Competitive Lobby Only"])   :ref "competitive"}
                                {:name (tr [:settings.none "None"])                          :ref "none"}]]
                    [:div {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "deckstats"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc-in [:deckstats] (.. % -target -value))
                                      :checked (= (:deckstats @s) (:ref option))}]
                      (:name option)]]))]

          [:section {:id "high-res"}
           [:h3 (tr [:settings.card-images "Card images"])]
           [:div
            [:label [:input {:type "checkbox"
                             :name "use-high-res"
                             :checked (= "high" (:card-resolution @s))
                             :on-change #(swap! s assoc-in [:card-resolution] (if (.. % -target -checked) "high" "default"))}]
             (tr [:settings.high-res "Enable high resolution card images"])]]]

          [:section {:id "alt-art"}
           [:h3 (tr [:settings.alt-art "Alt arts"])]
           [:div
            [:label [:input {:type "checkbox"
                             :name "show-alt-art"
                             :checked (:show-alt-art @s)
                             :on-change #(swap! s assoc-in [:show-alt-art] (.. % -target -checked))}]
             (tr [:settings.show-alt "Show alternate card arts"])]]
           [:br]

           (when (and (:special @user) (:show-alt-art @s) (:alt-info @app-state))
             [:div {:id "my-alt-art"}
              [:div {:id "set-all"}
               (tr [:settings.set-all "Set all cards to"]) ": "
               [:select {:ref "all-art-select"
                         :value (:all-art-select @s)
                         :on-change #(swap! s assoc-in [:all-art-select] (-> % .-target .-value))}
                (doall (for [t (all-alt-art-types)]
                         (when (not= "prev" t)
                           [:option {:value t :key t} (alt-art-name t)])))]
               [:button
                {:type "button"
                 :on-click #(reset-card-art s)}
                (tr [:settings.set "Set"])]]
              [:div.reset-all
               (let [disabled (empty? (:alt-arts @s))]
                 [:button
                  {:type "button"
                   :disabled disabled
                   :class (if disabled "disabled" "")
                   :on-click #(clear-card-art s)}
                  (tr [:settings.reset "Reset All to Official Art"])])]])]

         [:section
          [:h3 (tr [:settings.blocked "Blocked users"])]
          [:div
           [:input {:on-change #(swap! s assoc-in [:block-user-input] (-> % .-target .-value))
                    :on-key-down (fn [e]
                                   (when (= e.keyCode 13)
                                     (.preventDefault e)
                                     (add-user-to-block-list user s)))
                    :ref "block-user-input"
                    :value (:block-user-input @s)
                    :type "text" :placeholder (tr [:settings.user-name "User name"])}]
           [:button.block-user-btn {:type "button"
                                    :name "block-user-button"
                                    :on-click #(add-user-to-block-list user s)}
            (tr [:settings.block "Block user"])]]
          (doall (for [bu (:blocked-users @s)]
                   [:div.line {:key bu}
                    [:button.small.unblock-user {:type "button"
                                                 :on-click #(remove-user-from-block-list % s)} "X" ]
                    [:span.blocked-user-name (str "  " bu)]]))]

     [api-keys s]

     [:section
      [:button.float-right (tr [:settings.update-profile "Update Profile"])]
      [:span.flash-message (:flash-message @s)]]]])}))

(defn account []
  (let [user (r/cursor app-state [:user])
        scroll-top (atom 0)
        state (r/atom {:flash-message ""
                       :background (get-in @app-state [:options :background])
                       :custom-bg-url (get-in @app-state [:options :custom-bg-url])
                       :card-back (get-in @app-state [:options :card-back])
                       :card-zoom (get-in @app-state [:options :card-zoom])
                       :pin-zoom (get-in @app-state [:options :pin-zoom])
                       :pronouns (get-in @app-state [:options :pronouns])
                       :language (get-in @app-state [:options :language])
                       :sounds (get-in @app-state [:options :sounds])
                       :default-format (get-in @app-state [:options :default-format])
                       :lobby-sounds (get-in @app-state [:options :lobby-sounds])
                       :volume (get-in @app-state [:options :sounds-volume])
                       :show-alt-art (get-in @app-state [:options :show-alt-art])
                       :alt-arts (get-in @app-state [:options :alt-arts])
                       :all-art-select "wc2015"
                       :card-resolution (get-in @app-state [:options :card-resolution])
                       :stacked-cards (get-in @app-state [:options :stacked-cards])
                       :sides-overlap (get-in @app-state [:options :sides-overlap])
                       :player-stats-icons (get-in @app-state [:options :player-stats-icons])
                       :runner-board-order (get-in @app-state [:options :runner-board-order])
                       :log-width (get-in @app-state [:options :log-width])
                       :log-top (get-in @app-state [:options :log-top])
                       :log-player-highlight (get-in @app-state [:options :log-player-highlight])
                       :gamestats (get-in @app-state [:options :gamestats])
                       :deckstats (get-in @app-state [:options :deckstats])
                       :blocked-users (sort (get-in @app-state [:options :blocked-users]))})]

    (go (let [response (<! (GET "/profile/email"))]
          (when (= 200 (:status response))
            (swap! state assoc :email (:email (:json response))))))

    (go (swap! state assoc :api-keys (:json (<! (GET "/data/api-keys")))))

    (fn []
      [:div.page-container
       [:div.account-bg]
       [account-content user state scroll-top]])))
