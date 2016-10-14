(ns netrunner.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [netrunner.auth :refer [authenticated avatar] :as auth]
            [netrunner.appstate :refer [app-state]]
            [netrunner.ajax :refer [POST GET]]))

(defn handle-post [event owner url ref]
  (.preventDefault event)
  (om/set-state! owner :flash-message "Updating profile...")
  (swap! app-state assoc-in [:options :enablesounds] (om/get-state owner :enablesounds))
  (swap! app-state assoc-in [:options :background] (om/get-state owner :background))

  (let [params (-> event .-target js/$ .serialize)
        _ (.-serialize (js/$ (.-target event)))] ;; params is nil when built in :advanced mode. This fixes the issue.
    (go (let [response (<! (POST url params))]
          (if (= (:status response) 200)
            (om/set-state! owner :flash-message "Profile updated!")
            (case (:status response)
              401 (om/set-state! owner :flash-message "Invalid login or password")
              421 (om/set-state! owner :flash-message "No account with that email address exists")
              :else (om/set-state! owner :flash-message "Profile updated - Please refresh your browser")))))))

(defn account-view [user owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IWillMount
    (will-mount [this]
      (om/set-state! owner :background (get-in user [:options :background]))
      (om/set-state! owner :enablesounds (get-in user [:options :enablesounds])))

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
                              :name "enablesounds"
                              :value true
                              :checked (om/get-state owner :enablesounds)
                              :on-change #(om/set-state! owner :enablesounds (.. % -target -checked))}
                      "Enable sounds"]]]
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
            [:div.button-bar
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