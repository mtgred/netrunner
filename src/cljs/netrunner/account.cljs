(ns netrunner.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [netrunner.auth :refer [authenticated avatar] :as auth]
            [netrunner.main :refer [app-state]]
            [netrunner.ajax :refer [POST GET]]))

(defn handle-post [event owner url ref]
  (.preventDefault event)
  (om/set-state! owner :flash-message "")
  (let [params (-> event .-target js/$ .serialize)
        _ (.-serialize (js/$ (.-target event)))] ;; params is nil when built in :advanced mode. This fixes the issue.
    (go (let [response (<! (POST url params))]
          (if (= (:status response) 200))
            (om/set-state! owner :flash-message "Profile updated - Please refresh your browser")
            (case (:status response)
                401 (om/set-state! owner :flash-message "Invalid login or password")
                421 (om/set-state! owner :flash-message "No account with that email address exists")
                (om/set-state! owner :flash-message "Profile updated - Please refresh your browser")
                )))))

(defn update-profile [event owner]
  (.preventDefault event)
  (authenticated
   (fn [user]
     (do (om/set-state! owner :editing false)
      handle-post event owner "/update-profile" "profile-form"))))

(defn account-view [user owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IWillMount
    (will-mount [this]
      (om/set-state! owner :background (:background user)))

    om/IRenderState
    (render-state [this state]
     (sab/html
      [:div.account
       [:div.account-bg {:class (:background (:user @app-state))}]
       [:div.panel.blue-shade#profile-form {:ref "profile-form"}
        [:h3 "My Account"]
        [:hr]
        [:p.flash-message (:flash-message state)]
        [:form {:on-submit #(handle-post % owner "/update-profile" "profile-form")}
         [:h4 "Avatar"]
         (om/build avatar user {:opts {:size 38}})
         [:hr]
         [:h4 "Username"]
         (:username user)
         [:hr]
         [:h4 "Background"]
         (for [option [{:name "Beanstalk"      :ref "home-bg"}
                       {:name "The Root"       :ref "root-bg"}
                       {:name "Project Atlas"  :ref "deckbuilder-bg"}
                       {:name "Dyson Mem Chip" :ref "cardbrowser-bg"}
                       {:name "Fast Track"     :ref "about-bg"}
                       {:name "Logos"          :ref "reset-bg"}]] 
           [:label [:input {:type "radio"
                            :name "background"
                            :value (:ref option)
                            :on-change #(om/set-state! owner :background (.. % -target -value))
                            :checked (= (om/get-state owner :background) (:ref option))}]
            (:name option)])
         [:hr]
         [:div.button-bar
          [:button "Update Profile"]]]]]
     ))))

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