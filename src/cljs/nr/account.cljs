(ns nr.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [clojure.string :as s]
            [goog.dom :as gdom]
            [jinteki.cards :refer [all-cards]]
            [nr.auth :refer [valid-email?] :as auth]
            [nr.appstate :refer [app-state]]
            [nr.ajax :refer [POST GET PUT]]
            [nr.avatar :refer [avatar]]
            [reagent.core :as r]))

(defn- all-alt-art-types
  []
  (map :version (:alt-info @app-state)))

(defn- alt-art-name
  [version]
  (let [alt (first (filter #(= (name version) (:version %)) (:alt-info @app-state)))]
    (get alt :name "Official")))

(defn post-response [s response]
  (if (= (:status response) 200)
    (swap! s assoc :flash-message "Profile updated - Please refresh your browser")
    (case (:status response)
      401 (swap! s assoc :flash-message  "Invalid login or password")
      404 (swap! s assoc :flash-message  "No account with that email address exists")
      :else (swap! s assoc :flash-message  "Profile updated - Please refresh your browser"))))

(defn post-options [url callback]
  (let [params (:options @app-state)]
    (go (let [response (<! (PUT url params :json))]
          (callback response)))))

(defn handle-post [event url s]
  (.preventDefault event)
  (swap! s assoc :flash-message "Updating profile...")
  (swap! app-state assoc-in [:options :pronouns] (:pronouns @s))
  (swap! app-state assoc-in [:options :sounds] (:sounds @s))
  (swap! app-state assoc-in [:options :lobby-sounds] (:lobby-sounds @s))
  (swap! app-state assoc-in [:options :sounds-volume] (:volume @s))
  (swap! app-state assoc-in [:options :background] (:background @s))
  (swap! app-state assoc-in [:options :card-back] (:card-back @s))
  (swap! app-state assoc-in [:options :show-alt-art] (:show-alt-art @s))
  (swap! app-state assoc-in [:options :stacked-servers] (:stacked-servers @s))
  (swap! app-state assoc-in [:options :runner-board-order] (:runner-board-order @s))
  (swap! app-state assoc-in [:options :log-width] (:log-width @s))
  (swap! app-state assoc-in [:options :log-top] (:log-top @s))
  (swap! app-state assoc-in [:options :blocked-users] (:blocked-users @s))
  (swap! app-state assoc-in [:options :alt-arts] (:alt-arts @s))
  (swap! app-state assoc-in [:options :gamestats] (:gamestats @s))
  (swap! app-state assoc-in [:options :deckstats] (:deckstats @s))
  (.setItem js/localStorage "sounds" (:sounds @s))
  (.setItem js/localStorage "lobby_sounds" (:lobby-sounds @s))
  (.setItem js/localStorage "sounds_volume" (:volume @s))
  (.setItem js/localStorage "log-width" (:log-width @s))
  (.setItem js/localStorage "log-top" (:log-top @s))
  (.setItem js/localStorage "stacked-servers" (:stacked-servers @s))
  (.setItem js/localStorage "runner-board-order" (:runner-board-order @s))
  (.setItem js/localStorage "card-back" (:card-back @s))
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

(defn- remove-card-art
  [card s]
  (swap! s update :alt-arts dissoc (keyword (:code card))))

(defn- add-card-art
  [card art s]
  (swap! s update :alt-arts assoc (keyword (:code card)) art))

(defn- update-card-art
  "Set the alt art for a card"
  [card art s]
  (when (and card (string? art))
    (if (= "default" art)
      (remove-card-art card s)
      (let [versions (keys (:alt_art card))]
        (when (some #(= % (keyword art)) versions)
          (add-card-art card art s))))))

(defn- clear-card-art [s]
  (swap! s assoc-in [:alt-arts] {}))

(defn- reset-card-art [s]
  (let [art (:all-art-select @s)]
    (doseq [card (vals (:alt-cards @app-state))]
      (update-card-art card art s))))

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
        "Get current log width"]])))

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
        "Get current log top"]])))

(defn change-email [s]
  (let [email-state (r/atom {:flash-message ""
                             :email ""})]
    (fn [s]
      [:div#change-email.modal.fade {:ref "change-email"}
       [:div.modal-dialog
        [:h3 "Change email address"]
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
           [:p [:label "Current email: "]
            [:input {:type "text"
                     :value email
                     :name "current-email"
                     :read-only true}]])
         [:p [:label "Desired email: "]
          [:input {:type "text"
                   :placeholder "Email address"
                   :name "email"
                   :on-change #(let [value (-> % .-target .-value)]
                                 (swap! email-state assoc :email value))
                   :on-blur #(if (valid-email? (-> % .-target .-value))
                               (swap! email-state assoc :flash-message "")
                               (swap! email-state assoc :flash-message "Please enter a valid email address"))}]]
         [:p [:button "Update"]
          [:button {:data-dismiss "modal"} "Cancel"]]]]])))

(defn account-view [user]
  (let [s (r/atom {:flash-message ""
                   :background (get-in @app-state [:options :background])
                   :card-back (get-in @app-state [:options :card-back])
                   :pronouns (get-in @app-state [:options :pronouns])
                   :sounds (get-in @app-state [:options :sounds])
                   :lobby-sounds (get-in @app-state [:options :lobby-sounds])
                   :volume (get-in @app-state [:options :sounds-volume])
                   :show-alt-art (get-in @app-state [:options :show-alt-art])
                   :alt-arts (get-in @app-state [:options :alt-arts])
                   :all-art-select "wc2015"
                   :stacked-servers (get-in @app-state [:options :stacked-servers])
                   :runner-board-order (get-in @app-state [:options :runner-board-order])
                   :log-width (get-in @app-state [:options :log-width])
                   :log-top (get-in @app-state [:options :log-top])
                   :gamestats (get-in @app-state [:options :gamestats])
                   :deckstats (get-in @app-state [:options :deckstats])
                   :blocked-users (sort (get-in @app-state [:options :blocked-users]))})]

    (go (let [response (<! (GET "/profile/email"))]
          (when (= 200 (:status response))
            (swap! s assoc :email (:email (:json response))))))

    (fn [user]
      [:div.account
       [change-email s]
       [:div#profile-form.panel.blue-shade.content-page {:ref "profile-form"}
        [:h2 "Settings"]
        [:form {:on-submit #(handle-post % "/profile" s)}
         [:section
          [:h3 "Email"]
          [:a {:href "" :data-target "#change-email" :data-toggle "modal"} "Change email"]]
         [:section
          [:h3 "Avatar"]
          [avatar @user {:opts {:size 38}}]
          [:a {:href "http://gravatar.com" :target "_blank"} "Change on gravatar.com"]
          [:h3 "Pronouns"]
          [:select {:value (:pronouns @s "none")
                    :on-change #(swap! s assoc :pronouns (.. % -target -value))}
           (doall
             (for [option [{:name "Unspecified" :ref "none"}
                           {:name "Any" :ref "any"}
                           {:name "Prefer not to say" :ref "myodb"}
                           {:name "[blank]" :ref "blank"}
                           {:name "They/them" :ref "they"}
                           {:name "She/her" :ref "she"}
                           {:name "She/they" :ref "shethey"}
                           {:name "He/him" :ref "he"}
                           {:name "He/they" :ref "hethey"}
                           {:name "It" :ref "it"}
                           {:name "Ne/nem" :ref "ne"}
                           {:name "Ve/ver" :ref "ve"}
                           {:name "Ey/em" :ref "ey"}
                           {:name "Ze/hir" :ref "zehir"}
                           {:name "Ze/zir" :ref "zezir"}
                           {:name "Xe/xem" :ref "xe"}]]
               [:option {:value (:ref option) :key (:ref option)} (:name option)]))]]
         [:section
          [:h3 "Sounds"]
          [:div
           [:label [:input {:type "checkbox"
                            :value true
                            :checked (:lobby-sounds @s)
                            :on-change #(swap! s assoc-in [:lobby-sounds] (.. % -target -checked))}]
            "Enable lobby sounds"]]
          [:div
           [:label [:input {:type "checkbox"
                            :value true
                            :checked (:sounds @s)
                            :on-change #(swap! s assoc-in [:sounds] (.. % -target -checked))}]
            "Enable game sounds"]]
          [:div "Volume"
           [:input {:type "range"
                    :min 1 :max 100 :step 1
                    :on-change #(swap! s assoc-in [:volume] (.. % -target -value))
                    :value (or (:volume @s) 50)
                    :disabled (not (or (:sounds @s) (:lobby-sounds @s)))}]]]

         [:section
          [:h3 "Layout options"]
          [:div
           [:label [:input {:type "checkbox"
                            :value true
                            :checked (:stacked-servers @s)
                            :on-change #(swap! s assoc-in [:stacked-servers] (.. % -target -checked))}]
            "Server stacking is on by default"]]

          [:br]
          [:h4 "Runner layout from Corp perspective"]
          [:div
           [:div.radio
            [:label [:input {:name "runner-board-order"
                             :type "radio"
                             :value "jnet"
                             :checked (= "jnet" (:runner-board-order @s))
                             :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
             "Runner rig layout is classic jnet (Top to bottom: Programs, Hardware, Resources)"]]

           [:div.radio
            [:label [:input {:name "runner-board-order"
                             :type "radio"
                             :value "irl"
                             :checked (= "irl" (:runner-board-order @s))
                             :on-change #(swap! s assoc :runner-board-order (.. % -target -value))}]
             "Runner rig layout is reversed (Top to bottom: Resources, Hardware, Programs)"]]]]

         [log-width-option s]
         [log-top-option s]

         [:section
          [:h3  "Game board background"]
          (doall (for [option [{:name "The Root"        :ref "lobby-bg"}
                               {:name "Freelancer"      :ref "freelancer-bg"}
                               {:name "Mushin No Shin"  :ref "mushin-no-shin-bg"}
                               {:name "Traffic Jam"     :ref "traffic-jam-bg"}
                               {:name "Rumor Mill"      :ref "rumor-mill-bg"}
                               {:name "Find The Truth"  :ref "find-the-truth-bg"}
                               {:name "Push Your Luck"  :ref "push-your-luck-bg"}
                               {:name "Apex"            :ref "apex-bg"}
                               {:name "Worlds 2020"     :ref "worlds2020"}
                               {:name "Monochrome"      :ref "monochrome-bg"}]]
                   [:div.radio {:key (:name option)}
                    [:label [:input {:type "radio"
                                     :name "background"
                                     :value (:ref option)
                                     :on-change #(swap! s assoc-in [:background] (.. % -target -value))
                                     :checked (= (:background @s) (:ref option))}]
                     (:name option)]]))]

         [:section
          [:h3  "Card backs"]
          (doall (for [option [{:name "NISEI" :ref "nisei"}
                               {:name "FFG" :ref "ffg"}]]
                   [:div.radio {:key (:name option)}
                    [:label [:input {:type "radio"
                                     :name "card-back"
                                     :value (:ref option)
                                     :on-change #(swap! s assoc :card-back (.. % -target -value))
                                     :checked (= (:card-back @s) (:ref option))}]
                     (:name option)]]))]

         [:section
          [:h3 " Game Win/Lose statistics "]
          (doall (for [option [{:name "Always"                   :ref "always"}
                               {:name "Competitive Lobby Only"   :ref "competitive"}
                               {:name "None"                     :ref "none"}]]
                   [:div {:key (:name option)}
                    [:label [:input {:type "radio"
                                     :name "gamestats"
                                     :value (:ref option)
                                     :on-change #(swap! s assoc-in [:gamestats] (.. % -target -value))
                                     :checked (= (:gamestats @s) (:ref option))}]
                     (:name option)]]))]

         [:section
          [:h3 " Deck statistics "]
          (doall (for [option [{:name "Always"                   :ref "always"}
                               {:name "Competitive Lobby Only"   :ref "competitive"}
                               {:name "None"                     :ref "none"}]]
                   [:div {:key (:name option)}
                    [:label [:input {:type "radio"
                                     :name "deckstats"
                                     :value (:ref option)
                                     :on-change #(swap! s assoc-in [:deckstats] (.. % -target -value))
                                     :checked (= (:deckstats @s) (:ref option))}]
                     (:name option)]]))]

         [:section {:id "alt-art"}
          [:h3 "Alt arts"]
          [:div
           [:label [:input {:type "checkbox"
                            :name "show-alt-art"
                            :checked (:show-alt-art @s)
                            :on-change #(swap! s assoc-in [:show-alt-art] (.. % -target -checked))}]
            "Show alternate card arts"]]

          (when (and (:special @user) (:show-alt-art @s) (:alt-info @app-state))
            [:div {:id "my-alt-art"}
             [:div {:id "set-all"}
              "Set all cards to: "
              [:select {:ref "all-art-select"
                        :value (:all-art-select @s)
                        :on-change #(swap! s assoc-in [:all-art-select] (-> % .-target .-value))}
               (doall (for [t (all-alt-art-types)]
                        (when (not= "prev" t)
                          [:option {:value t :key t} (alt-art-name t)])))]
              [:button
               {:type "button"
                :on-click #(reset-card-art s)}
               "Set"]]
             [:div.reset-all
              (let [disabled (empty? (:alt-arts @s))]
                [:button
                 {:type "button"
                  :disabled disabled
                  :class (if disabled "disabled" "")
                  :on-click #(clear-card-art s)}
                 "Reset All to Official Art"])]])]

         [:section
          [:h3 "Blocked users"]
          [:div
           [:input {:on-change #(swap! s assoc-in [:block-user-input] (-> % .-target .-value))
                    :on-key-down (fn [e]
                                   (when (= e.keyCode 13)
                                     (.preventDefault e)
                                     (add-user-to-block-list user s)))
                    :ref "block-user-input"
                    :value (:block-user-input @s)
                    :type "text" :placeholder "User name"}]
           [:button.block-user-btn {:type "button"
                                    :name "block-user-button"
                                    :on-click #(add-user-to-block-list user s)}
            "Block user"]]
          (doall (for [bu (:blocked-users @s)]
            [:div.line {:key bu}
             [:button.small.unblock-user {:type "button"
                                          :on-click #(remove-user-from-block-list % s)} "X" ]
             [:span.blocked-user-name (str "  " bu)]]))]

         [:p
          [:button "Update Profile"]
          [:span.flash-message (:flash-message @s)]]]]])))

(defn account []
  (let [user (r/cursor app-state [:user])
        active (r/cursor app-state [:active-page])]
    (when (and @user (= "/account" (first @active)))
      [account-view user])))
