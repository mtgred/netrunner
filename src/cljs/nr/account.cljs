(ns nr.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [clojure.string :as s]
            [goog.dom :as gdom]
            [jinteki.cards :refer [all-cards]]
            [netrunner.auth :refer [authenticated avatar] :as auth]
            [netrunner.appstate :refer [app-state]]
            [netrunner.ajax :refer [POST GET PUT]]
            [reagent.core :as r]))

(def account-state (r/atom {}))

(def alt-arts-channel (chan))

;; TODO check if this is unused really
(defn load-alt-arts []
  (go (let [alt_info (->> (<! (GET "/data/cards/altarts"))
                          (:json)
                          (map #(select-keys % [:version :name])))
            cards (->> @all-cards
                       (filter #(not (:replaced_by %)))
                       (map #(select-keys % [:title :setname :code :alt_art :replaces :replaced_by]))
                       (filter :alt_art)
                       (into {} (map (juxt :code identity))))]
        (swap! app-state assoc :alt-arts cards)
        (swap! app-state assoc :alt-info alt_info)
        (put! alt-arts-channel cards))))

(defn image-url [card-code version]
  (let [card (get (:alt-arts @app-state) card-code)
        version-path (get (:alt_art card) (keyword version) card-code)]
    (str "/img/cards/" version-path ".png")))

(defn- all-alt-art-types
  []
  (map :version (:alt-info @app-state)))

(defn alt-art-name
  [version]
  (let [alt (first (filter #(= (name version) (:version %)) (:alt-info @app-state)))]
    (get alt :name "Official")))

(defn post-response [response]
  (if (= (:status response) 200)
    (swap! account-state assoc :flash-message "Profile updated - Please refresh your browser")
    (case (:status response)
      401 (swap! account-state assoc :flash-message  "Invalid login or password")
      404 (swap! account-state assoc :flash-message  "No account with that email address exists")
      :else (swap! account-state assoc :flash-message  "Profile updated - Please refresh your browser"))))

(defn post-options [url callback]
  (let [params (:options @app-state)]
    (go (let [response (<! (PUT url params :json))]
          (callback response)))))

(defn handle-post [event url ref]
  (.preventDefault event)
  (swap! account-state assoc :flash-message "Updating profile...")
  (swap! app-state assoc-in [:options :sounds] (:sounds @account-state))
  (swap! app-state assoc-in [:options :background] (:background @account-state))
  (swap! app-state assoc-in [:options :show-alt-art] (:show-alt-art @account-state))
  (swap! app-state assoc-in [:options :sounds-volume] (:volume @account-state))
  (swap! app-state assoc-in [:options :blocked-users] (:blocked-users @account-state))
  (swap! app-state assoc-in [:options :alt-arts] (:alt-arts @account-state))
  (swap! app-state assoc-in [:options :gamestats] (:gamestats @account-state))
  (swap! app-state assoc-in [:options :deckstats] (:deckstats @account-state))
  (.setItem js/localStorage "sounds" (:sounds @account-state))
  (.setItem js/localStorage "sounds_volume" (:volume @account-state))
  ; TODO no idea how and if this works...
  (post-options url (partial post-response)))

(defn add-user-to-block-list
  [user]
  (let [blocked-user (:block-user-input @account-state)
        my-user-name (:username user)
        current-blocked-list (:blocked-users @account-state)]
    (swap! account-state assoc-in [:block-user-input] "")
    (when (and (not (s/blank? blocked-user))
               (not= my-user-name blocked-user)
               (= -1 (.indexOf current-blocked-list blocked-user)))
      (swap! account-state assoc-in [:blocked-users] (conj current-blocked-list blocked-user)))))

(defn remove-user-from-block-list
  [evt]
  (let [currElt (.-currentTarget evt)
        next-sib (gdom/getNextElementSibling currElt)
        user-name (gdom/getTextContent next-sib)
        current-blocked-list (:blocked-users @account-state)]
    (when user-name
      (swap! account-state assoc-in [:blocked-users] (vec (remove #(= % user-name) current-blocked-list))))))

(defn select-card
  [value]
  (swap! account-state assoc-in [:alt-card] value)
  (swap! account-state assoc-in [:alt-card-version]
                 (get (:alt-arts @account-state) (keyword value) "default")))

(defn- remove-card-art
  [card]
  (swap! account-state update-in [:alt-arts] #(dissoc % (keyword (:code card))))
  (when-let [replaces (:replaces card)]
    (let [replaced-card (some #(when (= replaces (:code %)) %) (:cards @app-state))]
      (when replaced-card
        (remove-card-art replaced-card)))))

(defn- add-card-art
  [card art]
  (swap! account-state update-in [:alt-arts] #(assoc % (keyword (:code card)) art))
  (when-let [replaces (:replaces card)]
    (let [replaced-card (some #(when (= replaces (:code %)) %) (:cards @app-state))]
      (when replaced-card
        (add-card-art replaced-card art)))))

(defn- update-card-art
  "Set the alt art for a card and any card it replaces (recursively)"
  [card art]
  (when (and card (string? art))
    (if (= "default" art)
      (remove-card-art card)
      (let [versions (keys (:alt_art card))]
        (when (some #(= % (keyword art)) versions)
          (add-card-art card art))))))

(defn set-card-art
  [value]
  (swap! account-state assoc-in [:alt-card-version] value)
  (let [code (:alt-card @account-state)
        card (some #(when (= code (:code %)) %) @all-cards)]
    (update-card-art card value)))

(defn reset-card-art
  ([] (let [art (:all-art-select @account-state)]
        (reset-card-art art)))
  ([art]
   (doseq [card (vals (:alt-arts @app-state))]
     (update-card-art card art))))

(defn account-view [user]
  (swap! account-state assoc :flash-message "")
  (go (while true
        (let [cards (<! alt-arts-channel)
              first-alt (first (sort-by :title (vals cards)))]
          (swap! account-state assoc-in [:alt-arts] (get-in @app-state [:options :alt-arts]))
          (swap! account-state assoc-in [:alt-card] (:code first-alt))
          (swap! account-state assoc-in [:alt-card-version]
                 (get-in @app-state [:options :alt-arts (keyword (:code first-alt))]
                         "default")))))
  (fn []
    [:div.account
     [:div#profile-form.panel.blue-shade.content-page {:ref "profile-form"}
      [:h2 "Settings"]
      [:form {:on-submit #(handle-post % "/profile" "profile-form")}
       [:section
        [:h3 "Avatar"]
        ;; TODO FIX AVATAR
        ;(:emailhash @user) (:username @user)
        ;[avatar (:emailhash user) (:username user) {:size 38}]
        [:a {:href "http://gravatar.com" :target "_blank"} "Change on gravatar.com"]]

       [:section
        [:h3 "Sounds"]
        [:div
         [:label [:input {:type "checkbox"
                          :value true
                          :checked (:sounds @account-state)
                          :on-change #(swap! account-state assoc-in [:sounds] (.. % -target -checked))}]
          "Enable sounds"]]
        [:div "Volume"
         [:input {:type "range"
                  :min 1 :max 100 :step 1
                  :on-change #(swap! account-state assoc-in [:volume] (.. % -target -value))
                  :value (:volume @account-state)
                  :disabled (not (:sounds @account-state))}]]]

       [:section
        [:h3  "Game board background"]
        (doall (for [option [{:name "The Root"        :key 1 :ref "lobby-bg"}
                             {:name "Freelancer"      :key 2 :ref "freelancer-bg"}
                             {:name "Mushin No Shin"  :key 3 :ref "mushin-no-shin-bg"}
                             {:name "Traffic Jam"     :key 4 :ref "traffic-jam-bg"}
                             {:name "Rumor Mill"      :key 5 :ref "rumor-mill-bg"}
                             {:name "Find The Truth"  :key 6 :ref "find-the-truth-bg"}
                             {:name "Push Your Luck"  :key 7 :ref "push-your-luck-bg"}
                             {:name "Apex"            :key 8 :ref "apex-bg"}
                             {:name "Monochrome"      :key 9 :ref "monochrome-bg"}]]
                 [:div.radio {:key (:key option)}
                  [:label [:input {:type "radio"
                                   :name "background"
                                   :value (:ref option)
                                   :on-change #(swap! account-state assoc-in [:background] (.. % -target -value))
                                   :checked (= (:background @account-state) (:ref option))}]
                   (:name option)]]))]

       [:section
        [:h3 " Game Win/Lose Statistics "]
        (doall (for [option [{:name "Always"                   :key 1 :ref "always"}
                             {:name "Competitive Lobby Only"   :key 2 :ref "competitive"}
                             {:name "None"                     :key 3 :ref "none"}]]
                 [:div {:key (:key option)}
                  [:label [:input {:type "radio"
                                   :name "gamestats"
                                   :value (:ref option)
                                   :on-change #(swap! account-state assoc-in [:gamestats] (.. % -target -value))
                                   :checked (= (:gamestats @account-state) (:ref option))}]
                   (:name option)]]))]

       [:section
        [:h3 " Deck Statistics "]
        (doall (for [option [{:name "Always"                   :key 1 :ref "always"}
                             {:name "Competitive Lobby Only"   :key 2 :ref "competitive"}
                             {:name "None"                     :key 3 :ref "none"}]]
                 [:div {:key (:key option)}
                  [:label [:input {:type "radio"
                                   :name "deckstats"
                                   :value (:ref option)
                                   :on-change #(swap! account-state assoc-in [:deckstats] (.. % -target -value))
                                   :checked (= (:deckstats @account-state) (:ref option))}]
                   (:name option)]]))]

       [:section {:id "alt-art"}
        [:h3 "Alt arts"]
        [:div
         [:label [:input {:type "checkbox"
                          :name "show-alt-art"
                          :checked (:show-alt-art @account-state)
                          :on-change #(swap! account-state assoc-in [:show-alt-art] (.. % -target -checked))}]
          "Show alternate card arts"]]

        ;; TODO this @app-state will cause a lot of re-rendering as it is a ratom
        (when (and (:special user) (:alt-arts @app-state))
          [:div {:id "my-alt-art"}
           [:h4 "My alternate card arts"]
           [:select {:on-change #(select-card (.. % -target -value))}
            (for [card (sort-by :title (vals (:alt-arts @app-state)))]
              [:option {:value (:code card)} (:title card)])]

           [:div {:class "alt-art-group"}
            (for [version (conj (keys (get-in (:alt-arts @app-state) [(:alt-card @account-state) :alt_art])) :default)]
              (let [curr-alt (:alt-card @account-state)
                    url (image-url curr-alt version)
                    alt (get (:alt-arts @app-state) curr-alt)]
                [:div
                 [:div
                  [:div [:label [:input {:type "radio"
                                         :name "alt-art-radio"
                                         :value (name version)
                                         :on-change #(set-card-art (.. % -target -value))
                                         :checked (= (:alt-card-version @account-state) (name version))}]
                         (alt-art-name version)]]]
                 [:div
                  [:img {:class "alt-art-select"
                         :src url
                         :alt (str (:title alt) " (" (alt-art-name version) ")")
                         :on-click #(set-card-art (name version))
                         :onError #(-> % .-target js/$ .hide)
                         :onLoad #(-> % .-target js/$ .show)}]]]))]
           [:div {:id "set-all"}
            "Set all cards to: "
            [:select {:ref "all-art-select"
                      :value (:all-art-select @account-state)
                      :on-change #(swap! account-state assoc-in [:all-art-select] (-> % .-target .-value))}
             (for [t (all-alt-art-types)]
               [:option {:value t} (alt-art-name t)])]
            [:button
             {:type "button"
              :on-click #(do (reset-card-art)
                             (select-card (:alt-card @account-state)))}
             "Set"]]
           [:div.reset-all
            [:button
             {:type "button"
              :on-click #(do (reset-card-art "default")
                             (select-card (:alt-card @account-state)))}
             "Reset All to Official Art"]]])]

       [:section
        [:h3 "Blocked users"]
        [:div
         [:input {:on-change #(swap! account-state assoc-in [:block-user-input] (-> % .-target .-value))
                  :on-key-down (fn [e]
                                 (when (= e.keyCode 13)
                                   (do (add-user-to-block-list user)
                                       (.preventDefault e))))
                  :ref "block-user-input"
                  :value (:block-user-input @account-state)
                  :type "text" :placeholder "User name"}]
         [:button.block-user-btn {:type "button"
                                  :name "block-user-button"
                                  :on-click #(add-user-to-block-list user)}
          "Block user"]]
        (for [bu (:blocked-users @account-state)]
          [:div.line
           [:button.small.unblock-user {:type "button"
                                        :on-click #(remove-user-from-block-list %)} "X" ]
           [:span.blocked-user-name (str "  " bu)]])]

       [:p
        [:button "Update Profile"]
        [:span.flash-message (:flash-message @account-state)]]]]]))

(defn account [user]
  (when @user
    [account-view user]))


; fix avatar