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
   [nr.sounds :refer [bespoke-sounds play-sfx random-sound select-random-from-grouping]]
   [nr.translations :refer [tr tr-format]]
   [nr.utils :refer [format-date-time ISO-ish-formatter non-game-toast
                     set-scroll-top slug->format store-scroll-top]]
   [jinteki.i18n :as i18n]
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

(defn save-to-local-storage!
  [k v]
  (when-not (nil? v)
    (.setItem js/localStorage k v)))

(defn handle-post [event s]
  (.preventDefault event)
  (swap! s assoc :flash-message (tr [:settings_updating "Updating profile..."]))
  (let [{:keys [pronouns bespoke-sounds language sounds default-format
                lobby-sounds volume background custom-bg-url card-back card-zoom
                pin-zoom show-alt-art card-resolution pass-on-rez
                player-stats-icons stacked-cards ghost-trojans
                display-encounter-info sides-overlap log-timestamps
                runner-board-order log-width log-top log-player-highlight
                blocked-users alt-arts gamestats deckstats disable-websockets]} @s]
    (swap! app-state update :options
           (fn [options]
             (m/assoc-some options
                           :pronouns pronouns
                           :bespoke-sounds bespoke-sounds
                           :language language
                           :sounds sounds
                           :default-format default-format
                           :lobby-sounds lobby-sounds
                           :volume volume
                           :background background
                           :custom-bg-url custom-bg-url
                           :card-back card-back
                           :card-zoom card-zoom
                           :pin-zoom pin-zoom
                           :show-alt-art show-alt-art
                           :card-resolution card-resolution
                           :pass-on-rez pass-on-rez
                           :player-stats-icons player-stats-icons
                           :stacked-cards stacked-cards
                           :ghost-trojans ghost-trojans
                           :display-encounter-info display-encounter-info
                           :sides-overlap sides-overlap
                           :log-timestamps log-timestamps
                           :runner-board-order runner-board-order
                           :log-width log-width
                           :log-top log-top
                           :log-player-highlight log-player-highlight
                           :blocked-users blocked-users
                           :alt-arts alt-arts
                           :gamestats gamestats
                           :deckstats deckstats
                           :disable-websockets disable-websockets)))
    (save-to-local-storage! "language" language)
    (save-to-local-storage! "sounds" sounds)
    (save-to-local-storage! "default-format" default-format)
    (save-to-local-storage! "lobby_sounds" lobby-sounds)
    (save-to-local-storage! "custom_bg_url" custom-bg-url)
    (save-to-local-storage! "sounds_volume" volume)
    (save-to-local-storage! "log-width" log-width)
    (save-to-local-storage! "log-top" log-top)
    (save-to-local-storage! "log-player-highlight" log-player-highlight)
    (save-to-local-storage! "pass-on-rez" pass-on-rez)
    (save-to-local-storage! "player-stats-icons" player-stats-icons)
    (save-to-local-storage! "stacked-cards" stacked-cards)
    (save-to-local-storage! "ghost-trojans" ghost-trojans)
    (save-to-local-storage! "display-encounter-info" display-encounter-info)
    (save-to-local-storage! "sides-overlap" sides-overlap)
    (save-to-local-storage! "log-timestamps" log-timestamps)
    (save-to-local-storage! "runner-board-order" runner-board-order)
    (save-to-local-storage! "card-back" card-back)
    (save-to-local-storage! "card-zoom" card-zoom)
    (save-to-local-storage! "pin-zoom" pin-zoom)
    (save-to-local-storage! "disable-websockets" disable-websockets))
  (post-options #(post-response s %)))

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
        [tr [:settings_get-log-width "Get current log width"]]]])))

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
        [tr [:settings_get-log-top "Get current log top"]]]])))

(defn change-email [_]
  (let [email-state (r/atom {:flash-message ""
                             :email ""})]
    (fn [s]
      [:div
       [:h3 [tr [:settings_email-title "Change email address"]]]
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
          [:p [:label.email [tr [:settings_current-email "Current email"]] ": "]
           [:input.email {:type "text"
                          :value email
                          :name "current-email"
                          :read-only true}]])
        [:p [:label.email [tr [:settings_desired-email "Desired email"]] ": "]
         [:input.email {:type "text"
                        :placeholder (tr [:settings_email-placeholder "Email address"])
                        :name "email"
                        :on-change #(let [value (-> % .-target .-value)]
                                      (swap! email-state assoc :email value))
                        :on-blur #(if (valid-email? (-> % .-target .-value))
                                    (swap! email-state assoc :flash-message "")
                                    (swap! email-state assoc
                                           :flash-message [tr [:settings_enter-valid "Please enter a valid email address"]]))}]]
        [:p.float-right
         (let [disabled (not (valid-email? (:email @email-state)))]
           [:button
            {:disabled disabled
             :class (when disabled "disabled")}
            [tr [:settings_update "Update"]]])
         [:button {:on-click #(do (.preventDefault %)
                                  (reagent-modals/close-modal!))}
          [tr [:settings_cancel "Cancel"]]]]]])))

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
     [:h3 [tr [:settings_api-keys "API Keys"]]]
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
              [tr [:settings_delete-api-key "Delete"]]]]
            [:span.date
             (format-date-time ISO-ish-formatter (:date d))]
            [:span.title (:api-key d "")]]))]]
     [:button {:on-click #(do (.preventDefault %)
                              (create-api-key s))}
      [tr [:settings_create-api-key "Create API Key"]]]]))

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
  [["Apex" "apex"]
   ["Custom BG (input URL below)" "custom"]
   ["Find The Truth" "find-the-truth"]
   ["Freelancer" "freelancer"]
   ["Monochrome" "monochrome"]
   ["Mushin No Shin" "mushin-no-shin"]
   ["Push Your Luck" "push-your-luck"]
   ["Rumor Mill" "rumor-mill"]
   ["The Root" "the-root"]
   ["Traffic Jam" "traffic-jam"]
   ["Worlds 2020" "worlds2020"]])

(defn account-content [_ _ scroll-top]
  (r/create-class
    {:display-name "account-content"
     :component-did-mount #(set-scroll-top % @scroll-top)
     :component-will-unmount #(store-scroll-top % scroll-top)
     :reagent-render
     (fn [user s _]
       [:div#profile-form.panel.blue-shade.content-page {:ref "profile-form"}
         [:h2 [tr [:nav_settings "Settings"]]]
         [:form {:on-submit #(handle-post % s)}
          [:button.float-right [tr [:settings_update-profile "Update Profile"]]]
          [:section
           [:h3 [tr [:settings_email "Email"]]]
           [:a {:href ""
                :on-click #(do
                             (.preventDefault %)
                             (reagent-modals/modal! [change-email s]))}
            [tr [:settings_change-email "Change email"]]]]
          [:section
           [:h3 [tr [:settings_avatar "Avatar"]]]
           [avatar @user {:opts {:size 38}}]
           [:a {:href "http://gravatar.com" :target "_blank"}
            [tr [:settings_change-avatar "Change on gravatar.com"]]]]
          [:section
           [:h3 [tr [:settings_pronouns "Pronouns"]]]
           [:select {:value (:pronouns @s "none")
                     :on-change #(swap! s assoc :pronouns (.. % -target -value))}
            (doall
             (for [[title ref] pronoun-list]
               [:option {:value ref :key ref}
                (tr [:pronouns title] {:pronoun ref})]))]
           [:div "If your personal pronouns are not represented, you can request them "
            [:a {:href "https://github.com/mtgred/netrunner/issues"} "here"]]]
          [:section
           [:h3 [tr [:settings_language "Language"]]]
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
                            {:name "Português" :ref "pt"}
                            {:name "Русский" :ref "ru"}
                            {:name "Igpay Atinlay" :ref "la-pig"}]]
                [:option {:value (:ref option) :key (:ref option)} (:name option)]))]]
          [:section
           [:h3 [tr [:settings_sounds "Sounds"]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:lobby-sounds @s)
                             :on-change #(swap! s assoc-in [:lobby-sounds] (.. % -target -checked))}]
             [tr [:settings_enable-lobby-sounds "Enable lobby sounds"]]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:sounds @s)
                             :on-change #(swap! s assoc-in [:sounds] (.. % -target -checked))}]
             [tr [:settings_enable-game-sounds "Enable game sounds"]]]]
           [:div [tr [:settings_volume "Volume"]]
            [:input {:type "range"
                     :min 1 :max 100 :step 1
                     :on-mouse-up #(play-sfx [(random-sound)] {:volume (int (.. % -target -value))})
                     :on-change #(swap! s assoc-in [:volume] (.. % -target -value))
                     :value (or (:volume @s) 50)
                     :disabled (not (or (:sounds @s) (:lobby-sounds @s)))}]]]

          [:section
           [:h3 [tr [:settings_bespoke-sounds "Card-Specific Sounds"]
                 {:sound "header"}]]
           (doall
             (for [grouping (distinct (map :grouping (vals bespoke-sounds)))]
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
                 [tr [:settings_bespoke-sounds (name grouping)] {:sound (name grouping)}]]]))]

          [:section
           [:h3 [tr [:lobby_default-game-format "Default game format"]]]
           [:select.format
            {:value (or (:default-format @s) "standard")
             :on-change #(swap! s assoc-in [:default-format] (.. % -target -value))}
            (doall
             (for [[k v] slug->format]
               ^{:key k}
               [:option {:value k} (tr-format v)]))]]

          [:section
           [:h3 [tr [:settings_layout-options "Layout options"]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:player-stats-icons @s)
                             :on-change #(swap! s assoc-in [:player-stats-icons] (.. % -target -checked))}]
             [tr [:settings_player-stats-icons "Use icons for player stats"]]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:stacked-cards @s)
                             :on-change #(swap! s assoc-in [:stacked-cards] (.. % -target -checked))}]
             [tr [:settings_stacked-cards "Card stacking (on by default)"]]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:ghost-trojans @s)
                             :on-change #(swap! s assoc-in [:ghost-trojans] (.. % -target -checked))}]
             [tr [:settings_ghost-trojans "Display ghosts for hosted programs"]]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:display-encounter-info @s)
                             :on-change #(swap! s assoc-in [:display-encounter-info] (.. % -target -checked))}]
             [tr [:settings_display-encounter-info "Always display encounter info"]]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:sides-overlap @s)
                             :on-change #(swap! s assoc-in [:sides-overlap] (.. % -target -checked))}]
             [tr [:settings_sides-overlap "Runner and Corp board may overlap"]]]]
           [:div
            [:label [:input {:type "checkbox"
                             :value true
                             :checked (:log-timestamps @s)
                             :on-change #(swap! s assoc-in [:log-timestamps] (.. % -target -checked))}]
             [tr [:settings_toggle-log-timestamps "Show log timestamps"]]]]

           [:br]
           [:h4 [tr [:settings_runner-layout "Runner layout from Corp perspective"]]]
           [:div
            [:div.radio
             [:label [:input {:name "runner-board-order"
                              :type "radio"
                              :value "jnet"
                              :checked (= "jnet" (:runner-board-order @s))
                              :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
              [tr [:settings_runner-classic "Runner rig layout is classic jnet (Top to bottom: Programs, Hardware, Resources)"]]]]

            [:div.radio
             [:label [:input {:name "runner-board-order"
                              :type "radio"
                              :value "irl"
                              :checked (= "irl" (:runner-board-order @s))
                              :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
              [tr [:settings_runner-reverse "Runner rig layout is reversed (Top to bottom: Resources, Hardware, Programs)"]]]]]

           [:br]
           [:h4 [tr [:settings_log-size "Log size"]]]
           [:div
            [log-width-option s]
            [log-top-option s]]
           [:br]
           [:h4 [tr [:settings_log-player-highlight "Log player highlight"]]]
           [:div
            [:div.radio
             [:label [:input {:name "log-player-highlight"
                              :type "radio"
                              :value "blue-red"
                              :checked (= "blue-red" (:log-player-highlight @s))
                              :on-change #(swap! s assoc :log-player-highlight (.. % -target -value))}]
              [tr [:settings_log-player-highlight-red-blue "Corp: Blue / Runner: Red"]]]]

            [:div.radio
             [:label [:input {:name "log-player-highlight"
                              :type "radio"
                              :value "none"
                              :checked (= "none" (:log-player-highlight @s))
                              :on-change #(swap! s assoc :log-player-highlight (.. % -target -value))}]
              [tr [:settings_log-player-highlight-none "None"]]]]]]

          (let [custom-bg-selected (= (:background @s) "custom-bg" )
                custom-bg-url (r/atom (:custom-bg-url @s))]
            [:section
             [:h3  [tr [:settings_background "Game board background"]]]
             (doall (for [[title slug] background-list
                          :let [option {:name [tr [:settings_bg title] {:slug slug}]
                                        :ref (str slug "-bg")}]]
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
           [:h3  [tr [:settings_card-backs "Card backs"]]]
           (doall (for [option [{:name [tr [:settings_nsg "NSG"]] :ref "nsg"}
                                {:name [tr [:settings_ffg "FFG"]] :ref "ffg"}]]
                    [:div.radio {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "card-back"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc :card-back (.. % -target -value))
                                      :checked (= (:card-back @s) (:ref option))}]
                      (:name option)]]))]

          [:section
           [:h3  [tr [:settings_card-preview-zoom "Card preview zoom"]]]
           (doall (for [option [{:name [tr [:settings_card-iamge "Card Image"]] :ref "image"}
                                {:name [tr [:settings_card-text "Card Text"]] :ref "text"}]]
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
             [tr [:settings_pin-zoom "Keep zoomed cards on screen"]]]]]

          [:section
           [:h3 [tr [:settings_game-stats " Game Win/Lose statistics "]]]
           (doall (for [option [{:name [tr [:settings_always "Always"]]                      :ref "always"}
                                {:name [tr [:settings_comp-only "Competitive Lobby Only"]]   :ref "competitive"}
                                {:name [tr [:settings_none "None"]]                          :ref "none"}]]
                    [:div {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "gamestats"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc-in [:gamestats] (.. % -target -value))
                                      :checked (= (:gamestats @s) (:ref option))}]
                      (:name option)]]))]

          [:section
           [:h3 [tr [:settings_deck-stats " Deck statistics "]]]
           (doall (for [option [{:name [tr [:settings_always "Always"]]                      :ref "always"}
                                {:name [tr [:settings_comp-only "Competitive Lobby Only"]]   :ref "competitive"}
                                {:name [tr [:settings_none "None"]]                          :ref "none"}]]
                    [:div {:key (:name option)}
                     [:label [:input {:type "radio"
                                      :name "deckstats"
                                      :value (:ref option)
                                      :on-change #(swap! s assoc-in [:deckstats] (.. % -target -value))
                                      :checked (= (:deckstats @s) (:ref option))}]
                      (:name option)]]))]

          [:section {:id "high-res"}
           [:h3 [tr [:settings_card-images "Card images"]]]
           [:div
            [:label [:input {:type "checkbox"
                             :name "use-high-res"
                             :checked (= "high" (:card-resolution @s))
                             :on-change #(swap! s assoc-in [:card-resolution] (if (.. % -target -checked) "high" "default"))}]
             [tr [:settings_high-res "Enable high-resolution card images"]]]]]

          [:section {:id "alt-art"}
           [:h3 [tr [:settings_alt-art "Alt arts"]]]
           [:div
            [:label [:input {:type "checkbox"
                             :name "show-alt-art"
                             :checked (:show-alt-art @s)
                             :on-change #(swap! s assoc-in [:show-alt-art] (.. % -target -checked))}]
             [tr [:settings_show-alt "Show alternate card arts"]]]]
           [:br]

           (when (and (:special @user) (:show-alt-art @s) (:alt-info @app-state))
             [:div {:id "my-alt-art"}
              [:div {:id "set-all"}
               [tr [:settings_set-all "Set all cards to"]] ": "
               [:select {:ref "all-art-select"
                         :value (:all-art-select @s)
                         :on-change #(swap! s assoc-in [:all-art-select] (-> % .-target .-value))}
                (doall (for [t (all-alt-art-types)]
                         (when (not= "prev" t)
                           [:option {:value t :key t} (alt-art-name t)])))]
               [:button
                {:type "button"
                 :on-click #(reset-card-art s)}
                [tr [:settings_set "Set"]]]]
              [:div.reset-all
               (let [disabled (empty? (:alt-arts @s))]
                 [:button
                  {:type "button"
                   :disabled disabled
                   :class (if disabled "disabled" "")
                   :on-click #(clear-card-art s)}
                  [tr [:settings_reset "Reset All to Official Art"]]])]])]

         [:section
          [:h3 [tr [:settings_blocked "Blocked users"]]]
          [:div
           [:input {:on-change #(swap! s assoc-in [:block-user-input] (-> % .-target .-value))
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
            [tr [:settings_block "Block user"]]]]
          (doall (for [bu (:blocked-users @s)]
                   [:div.line {:key bu}
                    [:button.small.unblock-user {:type "button"
                                                 :on-click #(remove-user-from-block-list % s)} "X" ]
                    [:span.blocked-user-name (str "  " bu)]]))]

         [:section
          [:h3  [tr [:settings_connection "Connection"]]]
          [:div
           [:label [:input {:type "checkbox"
                            :name "disable-websockets"
                            :checked (:disable-websockets @s)
                            :on-change #(swap! s assoc-in [:disable-websockets] (.. % -target -checked))}]
            [tr [:settings_disable-websockets "Disable websockets - requires browser refresh after clicking Update Profile [Not Recommended!]"]]]]]

     [api-keys s]

     [:section
      [:span.flash-message (:flash-message @s)]]]])}))

(defn account []
  (let [user (r/cursor app-state [:user])
        scroll-top (atom 0)
        state (r/atom
               (-> (:options @app-state)
                   (select-keys [:pronouns :bespoke-sounds :language :sounds :default-format
                                 :lobby-sounds :volume :background :custom-bg-url :card-back :card-zoom
                                 :pin-zoom :show-alt-art :card-resolution :pass-on-rez
                                 :player-stats-icons :stacked-cards :ghost-trojans
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
