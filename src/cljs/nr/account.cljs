(ns nr.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!] :as async]
   [clojure.string :as s]
   [goog.dom :as gdom]
   [jinteki.cards :refer [all-cards]]
   [medley.core :as m]
   [nr.ajax :refer [DELETE GET POST PUT]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [valid-email?]]
   [nr.avatar :refer [avatar]]
   [nr.local-storage :as ls]
   [nr.sounds :refer [bespoke-sounds play-sfx random-sound select-random-from-grouping]]
   [nr.translations :refer [tr tr-format]]
   [nr.utils :refer [format-date-time ISO-ish-formatter non-game-toast
                     set-scroll-top slug->format store-scroll-top]]
   [jinteki.i18n :as i18n]
   [jinteki.card-backs :as card-backs]
   [reagent-modals.modals :as reagent-modals]
   [reagent.core :as r]))

(defn post-response [s response]
  (case (:status response)
    401 (non-game-toast (tr [:settings_invalid-password "Invalid login or password"]) "error" nil)
    404 (non-game-toast (tr [:settings_invalid-email "No account with that email address exists"]) "error" nil)
    ;; else
    (do (when-let [json (:json response)]
          (when (and (:lang json) (:content json))
            (i18n/insert-lang! (:lang json) (:content json))))
        (non-game-toast (tr [:settings_updated "Profile updated - Please refresh your browser"]) "success" nil)))
  (swap! s assoc :flash-message ""))

(defn post-options [callback]
  (let [params (:options @app-state)
        params (if (i18n/get-bundle (:language params))
                 params
                 (assoc params :lang (:language params)))]
    (go (let [response (<! (PUT "/profile" params :json))]
          (callback response)))))

(defn handle-post [event s]
  (.preventDefault event)
  (swap! s assoc :flash-message (tr [:settings_updating "Updating profile..."]))
  (let [{:keys [alt-arts
                background
                bespoke-sounds
                blocked-users
                card-back-display
                card-resolution
                card-zoom
                corp-card-sleeve
                custom-bg-url
                deckstats
                default-format
                disable-websockets
                display-encounter-info
                gamestats
                ghost-trojans
                language
                lobby-sounds
                log-player-highlight
                log-timestamps
                log-top
                log-width
                pass-on-rez
                pin-zoom
                player-stats-icons
                prizes
                pronouns
                runner-board-order
                runner-card-sleeve
                show-alt-art
                sides-overlap
                sounds
                stacked-cards
                volume]} @s]
    (swap! app-state update :options
           (fn [options]
             (m/assoc-some options
                           :alt-arts alt-arts
                           :background background
                           :bespoke-sounds bespoke-sounds
                           :blocked-users blocked-users
                           :card-back-display card-back-display
                           :card-resolution card-resolution
                           :card-zoom card-zoom
                           :corp-card-sleeve corp-card-sleeve
                           :custom-bg-url custom-bg-url
                           :deckstats deckstats
                           :default-format default-format
                           :disable-websockets disable-websockets
                           :display-encounter-info display-encounter-info
                           :gamestats gamestats
                           :ghost-trojans ghost-trojans
                           :language language
                           :lobby-sounds lobby-sounds
                           :log-player-highlight log-player-highlight
                           :log-timestamps log-timestamps
                           :log-top log-top
                           :log-width log-width
                           :pass-on-rez pass-on-rez
                           :pin-zoom pin-zoom
                           :player-stats-icons player-stats-icons
                           :pronouns pronouns
                           :runner-board-order runner-board-order
                           :runner-card-sleeve runner-card-sleeve
                           :show-alt-art show-alt-art
                           :sides-overlap sides-overlap
                           :sounds sounds
                           :stacked-cards stacked-cards
                           :volume volume)))
    ;; Save ALL settings to localStorage with consistent kebab-case naming
    (ls/save! "alt-arts" alt-arts)
    (ls/save! "background" background)
    (ls/save! "bespoke-sounds" bespoke-sounds)
    (ls/save! "blocked-users" blocked-users)
    (ls/save! "card-back-display" card-back-display)
    (ls/save! "card-resolution" card-resolution)
    (ls/save! "card-zoom" card-zoom)
    (ls/save! "corp-card-sleeve" corp-card-sleeve)
    (ls/save! "custom-bg-url" custom-bg-url)
    (ls/save! "deckstats" deckstats)
    (ls/save! "default-format" default-format)
    (ls/save! "disable-websockets" disable-websockets)
    (ls/save! "display-encounter-info" display-encounter-info)
    (ls/save! "gamestats" gamestats)
    (ls/save! "ghost-trojans" ghost-trojans)
    (ls/save! "language" language)
    (ls/save! "lobby-sounds" lobby-sounds)
    (ls/save! "log-player-highlight" log-player-highlight)
    (ls/save! "log-timestamps" log-timestamps)
    (ls/save! "log-top" log-top)
    (ls/save! "log-width" log-width)
    (ls/save! "pass-on-rez" pass-on-rez)
    (ls/save! "pin-zoom" pin-zoom)
    (ls/save! "player-stats-icons" player-stats-icons)
    (ls/save! "pronouns" pronouns)
    (ls/save! "runner-board-order" runner-board-order)
    (ls/save! "runner-card-sleeve" runner-card-sleeve)
    (ls/save! "show-alt-art" show-alt-art)
    (ls/save! "sides-overlap" sides-overlap)
    (ls/save! "sounds" sounds)
    (ls/save! "sounds-volume" volume)
    (ls/save! "stacked-cards" stacked-cards)
    ;; Note: visible-formats is handled separately
    ;; Note: prizes is handled as part of user data, not a setting
    ;; TODO: Add archives-sorted, heap-sorted, labeled-cards, labeled-unrezzed-cards when UI is added)
  (post-options #(post-response s %)))

(defn add-user-to-block-list
  [user s]
  (let [blocked-user (:block-user-input @s)
        my-user-name (:username user)
        current-blocked-list (:blocked-users @s)]
    (swap! s assoc :block-user-input "")
    (when (and (not (s/blank? blocked-user))
               (not= my-user-name blocked-user)
               (= -1 (.indexOf current-blocked-list blocked-user)))
      (swap! s assoc :blocked-users (conj current-blocked-list blocked-user)))))

(defn remove-user-from-block-list
  [evt s]
  (let [currElt (.-currentTarget evt)
        next-sib (gdom/getNextElementSibling currElt)
        user-name (gdom/getTextContent next-sib)
        current-blocked-list (:blocked-users @s)]
    (when user-name
      (swap! s assoc :blocked-users (vec (remove #(= % user-name) current-blocked-list))))))

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
  (swap! s assoc :alt-arts {}))

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
                :on-change #(do (swap! s assoc :log-width (.. % -target -value))
                                (reset! log-width (.. % -target -value)))
                :value @log-width}]
       [:button.update-log-width {:type "button"
                                  :on-click #(do (swap! s assoc :log-width (get-in @app-state [:options :log-width]))
                                                 (reset! log-width (get-in @app-state [:options :log-width])))}
        (tr [:settings_get-log-width "Get current log width"])]])))

(defn log-top-option [s]
  (let [log-top (r/atom (:log-top @s))]
    (fn []
      [:div
       [:input {:type "number"
                :min 100 :max 2000
                :on-change #(do (swap! s assoc :log-top (.. % -target -value))
                                (reset! log-top (.. % -target -value)))
                :value @log-top}]
       [:button.update-log-width {:type "button"
                                  :on-click #(do (swap! s assoc :log-top (get-in @app-state [:options :log-top]))
                                                 (reset! log-top (get-in @app-state [:options :log-top])))}
        (tr [:settings_get-log-top "Get current log top"])]])))

(defn change-email [_]
  (let [email-state (r/atom {:flash-message ""
                             :email ""})]
    (fn [s]
      [:div
       [:h3 (tr [:settings_email-title "Change email address"])]
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
          [:p [:label.email (tr [:settings_current-email "Current email"]) ": "]
           [:input.email {:type "text"
                          :value email
                          :name "current-email"
                          :read-only true}]])
        [:p [:label.email (tr [:settings_desired-email "Desired email"]) ": "]
         [:input.email {:type "text"
                        :placeholder (tr [:settings_email-placeholder "Email address"])
                        :name "email"
                        :on-change #(let [value (-> % .-target .-value)]
                                      (swap! email-state assoc :email value))
                        :on-blur #(if (valid-email? (-> % .-target .-value))
                                    (swap! email-state assoc :flash-message "")
                                    (swap! email-state assoc
                                           :flash-message (tr [:settings_enter-valid "Please enter a valid email address"])))}]]
        [:p.float-right
         (let [disabled (not (valid-email? (:email @email-state)))]
           [:button
            {:disabled disabled
             :class (when disabled "disabled")}
            (tr [:settings_update "Update"])])
         [:button {:on-click #(do (.preventDefault %)
                                  (reagent-modals/close-modal!))}
          (tr [:settings_cancel "Cancel"])]]]])))

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
     [:h3 (tr [:settings_api-keys "API Keys"])]
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
              (tr [:settings_delete-api-key "Delete"])]]
            [:span.date
             (format-date-time ISO-ish-formatter (:date d))]
            [:span.title (:api-key d "")]]))]]
     [:button {:on-click #(do (.preventDefault %)
                              (create-api-key s))}
      (tr [:settings_create-api-key "Create API Key"])]]))

(def pronoun-list
  [["Unspecified" "none"]
   ["Any" "any"]
   ["Prefer not to say" "myodb"]
   ["[blank]" "blank"]
   ["They/them" "they"]
   ["She/her" "she"]
   ["She/it" "sheit"]
   ["She/they" "shethey"]
   ["He/him" "he"]
   ["He/it" "heit"]
   ["He/they" "hethey"]
   ["He/She/they" "heshe"]
   ["It" "it"]
   ["Fae/faer" "faefaer"]
   ["Ne/nem" "ne"]
   ["Ve/ver" "ve"]
   ["Ey/em" "ey"]
   ["Ze/hir" "zehir"]
   ["Ze/zir" "zezir"]
   ["Xe/xem" "xe"]
   ["Xi/xir" "xi"]])

(def background-list
  [["Apex" "apex-bg"]
   ["Custom BG (input URL below)" "custom-bg"]
   ["Find The Truth" "find-the-truth-bg"]
   ["Freelancer" "freelancer-bg"]
   ["Monochrome" "monochrome-bg"]
   ["Mushin No Shin" "mushin-no-shin-bg"]
   ["Push Your Luck" "push-your-luck-bg"]
   ["Rumor Mill" "rumor-mill-bg"]
   ["The Root" "the-root-bg"]
   ["Traffic Jam" "traffic-jam-bg"]
   ["Worlds 2020" "worlds2020"]])

(defn account-content [_ _ scroll-top]
  (r/create-class
    {:display-name "account-content"
     :component-did-mount #(set-scroll-top % @scroll-top)
     :component-will-unmount #(store-scroll-top % scroll-top)
     :reagent-render
     (fn [user s _]
       [:div#profile-form.panel.blue-shade.content-page {:ref "profile-form"}
         [:h2 (tr [:nav_settings "Settings"])]
         [:form {:on-submit #(handle-post % s)}
          [:button.float-right (tr [:settings_update-profile "Update Profile"])]
          [:section
           [:h3 (tr [:settings_email "Email"])]
           [:a {:href ""
                :on-click #(do
                             (.preventDefault %)
                             (reagent-modals/modal! [change-email s]))}
            (tr [:settings_change-email "Change email"])]]
          [:section
           [:h3 (tr [:settings_avatar "Avatar"])]
           [avatar @user {:opts {:size 38}}]
           [:a {:href "http://gravatar.com" :target "_blank"}
            (tr [:settings_change-avatar "Change on gravatar.com"])]]
          [:section
           [:h3 (tr [:settings_pronouns "Pronouns"])]
           [:select {:value (:pronouns @s "none")
                     :on-change #(swap! s assoc :pronouns (.. % -target -value))}
            (doall
             (for [[title ref] pronoun-list]
               [:option {:value ref :key ref}
                (tr [:pronouns title] {:pronoun ref})]))]
           [:div "If your personal pronouns are not represented, you can request them "
            [:a {:href "https://github.com/mtgred/netrunner/issues"} "here"]]]
          [:section
           [:h3 (tr [:settings_language "Language"])]
           [:select {:value (:language @s "en")
                     :on-change #(swap! s assoc :language (.. % -target -value))}
            (doall
              (for [option [{:name "English" :ref "en"}
                            {:name "中文 (Simplified)" :ref "zh-simp"}
                            {:name "中文 (Traditional) (Cards only)" :ref "zh-trad"}
                            {:name "Français" :ref "fr"}
                            {:name "Deutsch (Cards only)" :ref "de"}
                            {:name "Italiano (Cards only)" :ref "it"}
                            {:name "日本語" :ref "ja"}
                            {:name "한국어" :ref "ko"}
                            {:name "Polski" :ref "pl"}
                            {:name "Português" :ref "pt"}
                            {:name "Русский" :ref "ru"}
                            {:name "Igpay Atinlay" :ref "la-pig"}]]
                [:option {:value (:ref option) :key (:ref option)} (:name option)]))]
           [:div "Some languages are not fully translated yet. If you would like to help with translations, please contact us."]]
          [:section
           [:h3 (tr [:settings_sounds "Sounds"])]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:lobby-sounds @s)
                             :on-change #(swap! s assoc :lobby-sounds (.. % -target -checked))}]
             (tr [:settings_enable-lobby-sounds "Enable lobby sounds"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:sounds @s)
                             :on-change #(swap! s assoc :sounds (.. % -target -checked))}]
             (tr [:settings_enable-game-sounds "Enable game sounds"])]]
           [:div (tr [:settings_volume "Volume"])
            [:input {:type "range"
                     :min 1 :max 100 :step 1
                     :on-mouse-up #(play-sfx [(random-sound)] {:volume (int (.. % -target -value))})
                     :on-change #(swap! s assoc :volume (.. % -target -value))
                     :value (or (:volume @s) 50)
                     :disabled (not (or (:sounds @s) (:lobby-sounds @s)))}]]]

          [:section
           [:h3 (tr [:settings_bespoke-sounds "Card-Specific Sounds"] {:sound "header"})]
           (doall
             (for [grouping (distinct (map :grouping (vals bespoke-sounds)))
                   :let [group-name (name grouping)]]
               ^{:key grouping}
               [:div
                [:label [:input {:type "checkbox"
                                 :value true
                                 :checked (get-in @s [:bespoke-sounds grouping])
                                 :on-change #(let [checked (.. % -target -checked)]
                                               (when checked
                                                 (play-sfx [(select-random-from-grouping grouping)]
                                                           {:volume (or (:volume @s) 50)
                                                            :force true}))
                                               (swap! s assoc-in [:bespoke-sounds grouping] checked))}]
                 (tr [:settings_bespoke-sounds group-name] {:sound group-name})]]))]

          [:section
           [:h3 (tr [:lobby_default-game-format "Default game format"])]
           [:select.format
            {:value (or (:default-format @s) "standard")
             :on-change #(swap! s assoc :default-format (.. % -target -value))}
            (doall
             (for [[k v] slug->format]
               ^{:key k}
               [:option {:value k} (tr-format v)]))]]

          [:section
           [:h3 (tr [:settings_layout-options "Layout options"])]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:player-stats-icons @s)
                             :on-change #(swap! s assoc :player-stats-icons (.. % -target -checked))}]
             (tr [:settings_player-stats-icons "Use icons for player stats"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:stacked-cards @s)
                             :on-change #(swap! s assoc :stacked-cards (.. % -target -checked))}]
             (tr [:settings_stacked-cards "Card stacking (on by default)"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:ghost-trojans @s)
                             :on-change #(swap! s assoc :ghost-trojans (.. % -target -checked))}]
             (tr [:settings_ghost-trojans "Display ghosts for hosted programs"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:display-encounter-info @s)
                             :on-change #(swap! s assoc :display-encounter-info (.. % -target -checked))}]
             (tr [:settings_display-encounter-info "Always display encounter info"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:sides-overlap @s)
                             :on-change #(swap! s assoc :sides-overlap (.. % -target -checked))}]
             (tr [:settings_sides-overlap "Runner and Corp board may overlap"])]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:log-timestamps @s)
                             :on-change #(swap! s assoc :log-timestamps (.. % -target -checked))}]
             (tr [:settings_toggle-log-timestamps "Show log timestamps"])]]

           [:br]
           [:h4 (tr [:settings_runner-layout "Runner layout from Corp perspective"])]
           [:div
            [:div.radio
             [:label [:input {:name "runner-board-order"
                              :type "radio"
                              :value "jnet"
                              :checked (= "jnet" (:runner-board-order @s))
                              :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
              (tr [:settings_runner-classic "Runner rig layout is classic jnet (Top to bottom: Programs, Hardware, Resources)"])]]

            [:div.radio
             [:label [:input {:name "runner-board-order"
                              :type "radio"
                              :value "irl"
                              :checked (= "irl" (:runner-board-order @s))
                              :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
              (tr [:settings_runner-reverse "Runner rig layout is reversed (Top to bottom: Resources, Hardware, Programs)"])]]]

           [:br]
           [:h4 (tr [:settings_log-size "Log size"])]
           [:div
            [log-width-option s]
            [log-top-option s]]
           [:br]
           [:h4 (tr [:settings_log-player-highlight "Log player highlight"])]
           [:div
            [:div.radio
             [:label [:input {:name "log-player-highlight"
                              :type "radio"
                              :value "blue-red"
                              :checked (= "blue-red" (:log-player-highlight @s))
                              :on-change #(swap! s assoc :log-player-highlight (.. % -target -value))}]
              (tr [:settings_log-player-highlight-red-blue "Corp: Blue / Runner: Red"])]]

            [:div.radio
             [:label [:input {:name "log-player-highlight"
                              :type "radio"
                              :value "none"
                              :checked (= "none" (:log-player-highlight @s))
                              :on-change #(swap! s assoc :log-player-highlight (.. % -target -value))}]
              (tr [:settings_log-player-highlight-none "None"])]]]]

          (let [custom-bg-selected (= (:background @s) "custom-bg" )
                custom-bg-url (r/cursor s [:custom-bg-url])]
            [:section
             [:h3 (tr [:settings_background "Game board background"])]
             (doall (for [[title slug] background-list]
                      [:div.radio {:key slug}
                       [:label [:input {:type "radio"
                                        :name "background"
                                        :value slug
                                        :on-change #(swap! s assoc :background (.. % -target -value))
                                        :checked (= (:background @s) slug)}]
                        (tr [:settings_bg title] {:slug slug})]]))

             [:div [:input {:type "text"
                            :hidden (not custom-bg-selected)
                            :on-change #(swap! s assoc :custom-bg-url (.. % -target -value))
                            :value @custom-bg-url}]]])

          [:section
           [:h3 (tr [:settings_corp-card-sleeve "Corp card backs"])]
           [:select {:value (:corp-card-sleeve @s "nsg-card-back")
                     :on-change #(swap! s assoc :corp-card-sleeve (or (.. % -target -value) "nsg-card-back"))}
            (doall
              (for [[k v] (card-backs/card-backs-for-side :corp (-> @s :prizes :card-backs))]
                [:option {:value k :key k}
                 (tr [(keyword (str "card-backs_" k)) (:name v)])]))]


           [:h3 (tr [:settings_runner-card-sleeve "Runner card backs"])]
           [:select {:value (:runner-card-sleeve @s "nsg-card-back")
                     :on-change #(swap! s assoc :runner-card-sleeve (or (.. % -target -value) "nsg-card-back"))}
            (doall
              (for [[k v] (card-backs/card-backs-for-side :runner (-> @s :prizes :card-backs))]
                [:option {:value k :key k}
                 (tr [(keyword (str "card-backs_" k)) (:name v)])]))]
           [:div "You can earn more card backs by placing well in select online tournaments. If you're an artist with art that you think would make for a good card back, please feel free to contact us"]

           [:div {:style {:display "flex" :justifyContent "center"}}
            [:div
             {:style {:display "flex" :flexDirection "column" :alighItems "center" :margin "1rem"}}
             [:img {:src (str "/img/card-backs/corp/"
                              (get-in (card-backs/card-backs-for-side :corp (-> @s :prizes :card-backs)) [(keyword (:corp-card-sleeve @s "nsg-card-back")) :file])
                              ".png")
                    :style {:maxWidth "200px"}
                    :alt "Corp card back"}]
             [:div {:style {:marginTop "0.5rem" :textAlign "center"}} "Corp card back"]]

            [:div
             {:style {:display "flex" :flexDirection "column" :alighItems "center" :margin "1rem"}}
             [:img {:src (str "/img/card-backs/runner/"
                              (get-in (card-backs/card-backs-for-side :runner (-> @s :prizes :card-backs)) [(keyword (:runner-card-sleeve @s "nsg-card-back")) :file])
                              ".png")
                    :style {:maxWidth "200px"}
                    :alt "Runner card back"}]
             [:div {:style {:marginTop "0.5rem" :textAlign "center"}} "Runner card back"]]]]

          [:section
           [:h3  (tr [:settings_card-preview-zoom "Card preview zoom"])]
           (doall (for [option [{:name (tr [:settings_card-iamge "Card Image"]) :ref "image"}
                                {:name (tr [:settings_card-text "Card Text"]) :ref "text"}]]
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
                             :on-change #(swap! s assoc :pin-zoom (.. % -target -checked))}]
             (tr [:settings_pin-zoom "Keep zoomed cards on screen"])]]]

          [:section
           [:h3 (tr [:settings_game-stats " Game Win/Lose statistics "])]
           (doall (for [option [{:name (tr [:settings_always "Always"])                      :ref "always"}
                                {:name (tr [:settings_comp-only "Competitive Lobby Only"])   :ref "competitive"}
                                {:name (tr [:settings_none "None"])                          :ref "none"}]]
                    [:div {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "gamestats"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc :gamestats (.. % -target -value))
                                      :checked (= (:gamestats @s) (:ref option))}]
                      (:name option)]]))]

          [:section
           [:h3 (tr [:settings_deck-stats " Deck statistics "])]
           (doall (for [option [{:name (tr [:settings_always "Always"])                      :ref "always"}
                                {:name (tr [:settings_comp-only "Competitive Lobby Only"])   :ref "competitive"}
                                {:name (tr [:settings_none "None"])                          :ref "none"}]]
                    [:div {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "deckstats"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc :deckstats (.. % -target -value))
                                      :checked (= (:deckstats @s) (:ref option))}]
                      (:name option)]]))]

          [:section {:id "high-res"}
           [:h3 (tr [:settings_card-images "Card images"])]
           [:div
            [:label [:input {:type "checkbox"
                             :name "use-high-res"
                             :checked (= "high" (:card-resolution @s))
                             :on-change #(swap! s assoc :card-resolution (if (.. % -target -checked) "high" "default"))}]
             (tr [:settings_high-res "Enable high-resolution card images"])]]]

          [:section {:id "alt-art"}
           [:h3 (tr [:settings_alt-art "Alt arts"])]
           [:div
            [:label [:input {:type "checkbox"
                             :name "show-alt-art"
                             :checked (:show-alt-art @s)
                             :on-change #(swap! s assoc :show-alt-art (.. % -target -checked))}]
             (tr [:settings_show-alt "Show alternate card arts"])]]
           [:br]

           (when (and (:special @user) (:show-alt-art @s) (:alt-info @app-state))
             [:div {:id "my-alt-art"}
              [:div {:id "set-all"}
               (tr [:settings_set-all "Set all cards to"]) ": "
               [:select {:ref "all-art-select"
                         :value (:all-art-select @s)
                         :on-change #(swap! s assoc :all-art-select (-> % .-target .-value))}
                (doall (for [t (all-alt-art-types)]
                         (when (not= "prev" t)
                           [:option {:value t :key t} (alt-art-name t)])))]
               [:button
                {:type "button"
                 :on-click #(reset-card-art s)}
                (tr [:settings_set "Set"])]]
              [:div.reset-all
               (let [disabled (empty? (:alt-arts @s))]
                 [:button
                  {:type "button"
                   :disabled disabled
                   :class (if disabled "disabled" "")
                   :on-click #(clear-card-art s)}
                  (tr [:settings_reset "Reset All to Official Art"])])]])]

         [:section
          [:h3 (tr [:settings_blocked "Blocked users"])]
          [:div
           [:input {:on-change #(swap! s assoc :block-user-input (-> % .-target .-value))
                    :on-key-down (fn [e]
                                   (when (= e.keyCode 13)
                                     (.preventDefault e)
                                     (add-user-to-block-list user s)))
                    :ref "block-user-input"
                    :value (:block-user-input @s)
                    :type "text"
                    :placeholder (tr [:settings_user-name "User name"])}]
           [:button.block-user-btn {:type "button"
                                    :name "block-user-button"
                                    :on-click #(add-user-to-block-list user s)}
            (tr [:settings_block "Block user"])]]
          (doall (for [bu (:blocked-users @s)]
                   [:div.line {:key bu}
                    [:button.small.unblock-user {:type "button"
                                                 :on-click #(remove-user-from-block-list % s)} "X" ]
                    [:span.blocked-user-name (str "  " bu)]]))]

         [:section
          [:h3  (tr [:settings_connection "Connection"])]
          [:div
           [:label [:input {:type "checkbox"
                            :name "disable-websockets"
                            :checked (:disable-websockets @s)
                            :on-change #(swap! s assoc :disable-websockets (.. % -target -checked))}]
            (tr [:settings_disable-websockets "Disable websockets - requires browser refresh after clicking Update Profile [Not Recommended!]"])]]]

     [api-keys s]

     [:section
      [:span.flash-message (:flash-message @s)]]]])}))

(defn account []
  (let [user (r/cursor app-state [:user])
        scroll-top (atom 0)
        state (r/atom
               (-> (:options @app-state)
                   (select-keys [:pronouns :bespoke-sounds :language :sounds :default-format
                                 :lobby-sounds :volume :background :custom-bg-url :card-zoom
                                 :pin-zoom :show-alt-art :card-resolution :pass-on-rez
                                 :player-stats-icons :stacked-cards :ghost-trojans
                                 :corp-card-sleeve :runner-card-sleeve :prizes
                                 :display-encounter-info :sides-overlap :log-timestamps
                                 :runner-board-order :log-width :log-top :log-player-highlight
                                 :blocked-users :alt-arts :gamestats :deckstats :disable-websockets])
                   (assoc :flash-message ""
                          :all-art-select "wc2015")))]

    (go (let [response (<! (GET "/profile/email"))]
          (when (= 200 (:status response))
            (swap! state assoc :email (:email (:json response))))))

    (go (swap! state assoc :api-keys (:json (<! (GET "/data/api-keys")))))

    (fn []
      [:div.page-container
       [:div.account-bg]
       [account-content user state scroll-top]])))
