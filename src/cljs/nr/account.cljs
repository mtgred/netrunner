(ns nr.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [clojure.string :as s]
            [goog.dom :as gdom]
            [jinteki.cards :refer [all-cards]]
            [nr.auth :refer [avatar] :as auth]
            [nr.appstate :refer [app-state]]
            [nr.ajax :refer [POST GET PUT]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [avatar] :as auth]
            [reagent.core :as r]))

(def alt-arts-channel (chan))

(defn load-alt-arts []
  (go (let [alt_info (->> (<! (GET "/data/cards/altarts"))
                          (:json)
                          (map #(select-keys % [:version :name :description :position])))
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
  (swap! app-state assoc-in [:options :sounds] (:sounds @s))
  (swap! app-state assoc-in [:options :lobby-sounds] (:lobby-sounds @s))
  (swap! app-state assoc-in [:options :sounds-volume] (:volume @s))
  (swap! app-state assoc-in [:options :background] (:background @s))
  (swap! app-state assoc-in [:options :show-alt-art] (:show-alt-art @s))
  (swap! app-state assoc-in [:options :stacked-servers] (:stacked-servers @s))
  (swap! app-state assoc-in [:options :runner-board-order] (:runner-board-order @s))
  (swap! app-state assoc-in [:options :log-width] (:log-width @s))
  (swap! app-state assoc-in [:options :blocked-users] (:blocked-users @s))
  (swap! app-state assoc-in [:options :alt-arts] (:alt-arts @s))
  (swap! app-state assoc-in [:options :gamestats] (:gamestats @s))
  (swap! app-state assoc-in [:options :deckstats] (:deckstats @s))
  (.setItem js/localStorage "sounds" (:sounds @s))
  (.setItem js/localStorage "lobby_sounds" (:lobby-sounds @s))
  (.setItem js/localStorage "sounds_volume" (:volume @s))
  (.setItem js/localStorage "log-width" (:log-width @s))
  (.setItem js/localStorage "stacked-servers" (:stacked-servers @s))
  (.setItem js/localStorage "runner-board-order" (:runner-board-order @s))
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
  (swap! s update-in [:alt-arts] #(dissoc % (keyword (:code card))))
  (when-let [replaces (:replaces card)]
    (let [replaced-card (some #(when (= replaces (:code %)) %) (:cards @app-state))]
      (when replaced-card
        (remove-card-art replaced-card s)))))

(defn- add-card-art
  [card art s]
  (swap! s update-in [:alt-arts] #(assoc % (keyword (:code card)) art))
  (when-let [replaces (:replaces card)]
    (let [replaced-card (some #(when (= replaces (:code %)) %) (:cards @app-state))]
      (when replaced-card
        (add-card-art replaced-card art s)))))

(defn- update-card-art
  "Set the alt art for a card and any card it replaces (recursively)"
  [card art s]
  (when (and card (string? art))
    (if (= "default" art)
      (remove-card-art card s)
      (let [versions (keys (:alt_art card))]
        (when (some #(= % (keyword art)) versions)
          (add-card-art card art s))))))

(defn reset-card-art
  ([s] (let [art (:all-art-select @s)]
        (reset-card-art s art)))
  ([s art]
   (doseq [card (vals (:alt-arts @app-state))]
     (update-card-art card art s))))

(defn log-width-option [s]
  (let [log-width (r/atom (:log-width @s))]
    (println @log-width)
    (fn []
      [:div
       [:input {:type "number"
                :min 100 :max 2000
                :on-change #(do (swap! s assoc-in [:log-width] (.. % -target -value))
                                (reset! log-width (.. % -target -value)))
                :value @log-width}]
       [:button.update-log-width {:type "button"
                                  :on-click #(do (swap! s assoc-in [:log-width] (get-in @app-state [:options :log-width]))
                                                 (reset! log-width (get-in @app-state [:options :log-width])))} "Get current log width" ]])))

(defn account-view [user]
  (let [s (r/atom {:flash-message ""
                   :background (get-in @app-state [:options :background])
                   :sounds (get-in @app-state [:options :sounds])
                   :lobby-sounds (get-in @app-state [:options :lobby-sounds])
                   :volume (get-in @app-state [:options :sounds-volume])
                   :show-alt-art (get-in @app-state [:options :show-alt-art])
                   :all-art-select ""
                   :stacked-servers (get-in @app-state [:options :stacked-servers])
                   :runner-board-order (get-in @app-state [:options :runner-board-order])
                   :log-width (get-in @app-state [:options :log-width])
                   :gamestats (get-in @app-state [:options :gamestats])
                   :deckstats (get-in @app-state [:options :deckstats])
                   :blocked-users (sort (get-in @app-state [:options :blocked-users]))})]
    (go (while true
          (let [cards (<! alt-arts-channel)
                first-alt (first (sort-by :title (vals cards)))]
            (swap! s assoc-in [:alt-arts] (get-in @app-state [:options :alt-arts]))
            (swap! s assoc-in [:alt-card] (:code first-alt))
            (swap! s assoc-in [:alt-card-version]
                   (get-in @app-state [:options :alt-arts (keyword (:code first-alt))]
                         "default")))))

    (fn [user]
      [:div.account
       [:div#profile-form.panel.blue-shade.content-page {:ref "profile-form"}
        [:h2 "Settings"]
        [:form {:on-submit #(handle-post % "/profile" s)}
         [:section
          [:h3 "Avatar"]
          [avatar @user {:opts {:size 38}}]
          [:a {:href "http://gravatar.com" :target "_blank"} "Change on gravatar.com"]]
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

          [:div
           [:label [:input {:type "checkbox"
                            :value true
                            :checked (:runner-board-order @s)
                            :on-change #(swap! s assoc-in [:runner-board-order] (.. % -target -checked))}]
            "Runner rig layout is jnet-classic (Top to bottom: Programs, Hardware, Resources)"]]]

         [log-width-option s]

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
                               {:name "Monochrome"      :ref "monochrome-bg"}]]
                   [:div.radio {:key (:name option)}
                    [:label [:input {:type "radio"
                                     :name "background"
                                     :value (:ref option)
                                     :on-change #(swap! s assoc-in [:background] (.. % -target -value))
                                     :checked (= (:background @s) (:ref option))}]
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

          (when (and (:special @user) (:alt-arts @app-state))
            [:div {:id "my-alt-art"}
             [:div {:id "set-all"}
              "Set all cards to: "
              [:select {:ref "all-art-select"
                        :value (:all-art-select @s)
                        :on-change #(swap! s assoc-in [:all-art-select] (-> % .-target .-value))}
               (doall (for [t (all-alt-art-types)]
                        [:option {:value t :key t} (alt-art-name t)]))]
              [:button
               {:type "button"
                :on-click #(do (reset-card-art s))}
               "Set"]]
             [:div.reset-all
              [:button
               {:type "button"
                :on-click #(do (reset-card-art s "default"))}
               "Reset All to Official Art"]]])]

         [:section
          [:h3 "Blocked users"]
          [:div
           [:input {:on-change #(swap! s assoc-in [:block-user-input] (-> % .-target .-value))
                    :on-key-down (fn [e]
                                   (when (= e.keyCode 13)
                                     (do (add-user-to-block-list user s)
                                         (.preventDefault e))))
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

(defn account [user]
  (when @user
    [account-view user]))

