(ns netrunner.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [netrunner.auth :refer [authenticated avatar] :as auth]
            [netrunner.appstate :refer [app-state]]
            [netrunner.ajax :refer [POST GET]]
            [netrunner.cardbrowser :refer [cards-channel]]))

(def alt-arts-channel (chan))
(defn load-alt-arts []
  (go (let [cards (->> (<! (GET "/data/altarts"))
                     :json
                     (filter :versions)
                     (map #(update-in % [:versions] conj "default"))
                     (map #(assoc % :title (some (fn [c] (when (= (:code c) (:code %)) (:title c))) (:cards @app-state))))
                     (into {} (map (juxt :code identity))))]
        (swap! app-state assoc :alt-arts cards)
        (put! alt-arts-channel cards))))

(go (let [cards (<! cards-channel)]
      (load-alt-arts)))

(defn handle-post [event owner url ref]
  (.preventDefault event)
  (om/set-state! owner :flash-message "Updating profile...")
  (swap! app-state assoc-in [:options :sounds] (om/get-state owner :sounds))
  (swap! app-state assoc-in [:options :background] (om/get-state owner :background))
  (swap! app-state assoc-in [:options :opponent-alt-art] (om/get-state owner :opponent-alt-art))
  (swap! app-state assoc-in [:options :sounds-volume] (om/get-state owner :volume))
  (.setItem js/localStorage "sounds" (om/get-state owner :sounds))
  (.setItem js/localStorage "sounds_volume" (om/get-state owner :volume))

  (let [params (:options @app-state)]
    (go (let [response (<! (POST url params :json))]
          (if (= (:status response) 200)
            (om/set-state! owner :flash-message "Profile updated!")
            (case (:status response)
              401 (om/set-state! owner :flash-message "Invalid login or password")
              421 (om/set-state! owner :flash-message "No account with that email address exists")
              :else (om/set-state! owner :flash-message "Profile updated - Please refresh your browser")))))))

(defn image-url [card version]
  (str "/img/cards/" card (when-not (= version "default") (str "-" version)) ".png"))

(defn alt-art-name [type]
  (case type
    "alt" "Alternate"
    "wc2015" "World Champion 2015"
    "Official"))

(defn account-view [user owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IWillMount
    (will-mount [this]
      (om/set-state! owner :background (get-in @app-state [:options :background]))
      (om/set-state! owner :sounds (get-in @app-state [:options :sounds]))
      (om/set-state! owner :opponent-alt-art (get-in @app-state [:options :opponent-alt-art]))
      (om/set-state! owner :volume (get-in @app-state [:options :sounds-volume]))
      (go (while true
            (let [cards (<! alt-arts-channel)]
              (om/set-state! owner :cards cards)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
        [:div.container
         [:div.account
          [:div {:class (:background (:options @app-state))}]
          [:div.panel.blue-shade#profile-form {:ref "profile-form"}
           [:h2 "My Account"]
           [:p.flash-message (:flash-message state)]
           [:form {:on-submit #(handle-post % owner "/update-profile" "profile-form")}
            [:h3 "Avatar"]
            (om/build avatar user {:opts {:size 38}})
            [:a {:href "http://gravatar.com" :target "_blank"} "Change on gravatar.com"]
            [:h3 {:style {:margin-top "1em"}} "Sounds"]
            [:div
             [:label [:input {:type "checkbox"
                              :value true
                              :checked (om/get-state owner :sounds)
                              :on-change #(om/set-state! owner :sounds (.. % -target -checked))}]
              "Enable sounds"]]
            [:div {:style {:margin-top "4px" :margin-left "24px"}}
             "Volume:"
             [:div [:input {:type "range"
                            :min 1 :max 100 :step 1
                            :on-change #(om/set-state! owner :volume (.. % -target -value))
                            :value (om/get-state owner :volume)
                            :disabled (not (om/get-state owner :sounds))}]]]
            [:h3 {:style {:margin-top "1em"}} "Game board background"]
            (for [option [{:name "Beanstalk"      :ref "home-bg"}
                          {:name "The Root"       :ref "lobby-bg"}
                          {:name "Project Atlas"  :ref "deckbuilder-bg"}
                          {:name "Dyson Mem Chip" :ref "cardbrowser-bg"}
                          {:name "Fast Track"     :ref "about-bg"}
                          {:name "Logos"          :ref "reset-bg"}]]
              [:div.radio
               [:label [:input {:type "radio"
                                :name "background"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :background (.. % -target -value))
                                :checked (= (om/get-state owner :background) (:ref option))}]
                (:name option)]])

            [:h3 {:style {:margin-top "1em"}} "Alt arts"]
            [:div
             [:label [:input {:type "checkbox"
                              :name "opponent-alt-art"
                              :checked (om/get-state owner :opponent-alt-art)
                              :on-change #(om/set-state! owner :opponent-alt-art (.. % -target -checked))}]
              "Show opponent's alternate card arts"]]

            (when (:special user)
              [:div {:style {:margin-top "10px"}}
               [:h4 "My alternate card arts"]
               [:select {:on-change #(do (om/set-state! owner :alt-card (.. % -target -value))
                                              (om/set-state! owner :alt-card-version
                                                             (get-in @app-state [:options :alt-arts (keyword (.. % -target -value))] "default")))}
                     (for [card (sort-by :title (vals (:alt-arts @app-state)))]
                       [:option {:value (:code card)} (:title card)])]
               (when (empty? (:alt-arts @app-state))
                 [:span.fake-link {:style {:margin-left "5px"} :on-click #(load-alt-arts)} "Reload"])

               [:div
                (for [version (get-in @app-state [:alt-arts (om/get-state owner :alt-card) :versions])]
                  (let [url (image-url (om/get-state owner :alt-card) version)]
                    [:div {:style {:float "left" :margin "10px"}}
                     [:div {:style {:text-align "center"}}
                      [:div [:label [:input {:type "radio"
                                             :name "alt-art-radio"
                                             :value version
                                             :on-change #(do (om/set-state! owner :alt-card-version (.. % -target -value))
                                                             (swap! app-state update-in [:options :alt-arts]
                                                                    assoc (keyword (om/get-state owner :alt-card)) (.. % -target -value)))
                                             :checked (= (om/get-state owner :alt-card-version) version)}]
                             (alt-art-name version)]]]
                     [:div
                      [:img {:style {:width "150px" :margin-top "5px"} :src url :onError #(-> % .-target js/$ .hide) :onLoad #(-> % .-target js/$ .show)}]]]))]])

            [:div.button-bar {:style {:clear "both"}}
             [:button {:style {:margin "2em 0"}} "Update Profile"]]]]]]))))

(defn unlogged-view [user owner]
  (om/component
   (sab/html
    [:div.account.panel.blue-shade
      [:h4 "Sign up to play Netrunner"]
      [:ul
       [:li
        [:a {:href "" :data-target "#register-form" :data-toggle "modal"
             :on-click (fn [] .focus (js/$ "input[name='email']"))} "Sign up"]]
       [:li
        [:a {:href "" :data-target "#login-form" :data-toggle "modal"} "Login"]]]])))

(defn account [{:keys [user]} owner]
  (om/component
   (if user
     (om/build account-view user)
     (om/build unlogged-view user))))

(om/root account app-state {:target (. js/document (getElementById "account"))})