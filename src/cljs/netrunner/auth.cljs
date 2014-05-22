(ns netrunner.auth
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [netrunner.ajax :refer [POST]]))

(def app-state
  (atom {:user (js->clj js/user :keywordize-keys true)}))

(defn logged-menu [user owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:li.dropdown.usermenu
        [:a.dropdown-toggle {:href "" :data-toggle "dropdown"}
         [:img {:src (str "http://www.gravatar.com/avatar/" (:emailhash user) "?d=retro&s=22")}]
         (:username user)
         [:b.caret]]
        [:div.dropdown-menu.blue-shade.float-right
         [:a.block-link {:href "/"} "Profile"]
         [:a.block-link {:href "/"} "Settings"]
         [:a.block-link {:href "/logout"} "Logout"]]]))))

(defn unlogged-menu [user owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:ul
        [:li
         [:a {:href "" :data-target "#register-form" :data-toggle "modal"
              :on-click (fn [] .focus (js/$ "input[name='email']"))} "Sign up"]]
        [:li
         [:a {:href "" :data-target "#login-form" :data-toggle "modal"} "Login"]]]))))

(defn auth-menu [{:keys [user]} owner]
  (om/component
   (if (empty? user)
     (om/build unlogged-menu user)
     (om/build logged-menu user))))

(defn register-form [cursor owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.modal.fade#register-form
        [:div.modal-dialog
         [:h3 "Create an account"]
         [:form {:action "/register" :method "post"}
          [:p [:input {:type "text" :placeholder "Email" :name "email"}]]
          [:p [:input {:type "text" :placeholder "Username" :name "username"}]]
          [:p [:input {:type "password" :placeholder "Password" :name "password"}]]
          [:p [:button "Sign up"]
              [:button {:data-dismiss "modal"} "Cancel"]]]]]))))

(defn handle-login [e owner]
  (.preventDefault e)
  (om/set-state! owner :flash-message "")
  (let [params (-> e .-target js/$ .serialize)]
    (go (let [response (<! (POST "/login" params))]
          (if (:error response)
            (om/set-state! owner :flash-message (str "Invalid login or password"))
            (do (.modal (js/$ "#login-form") "hide")
                (swap! app-state assoc :user response)))))))

(defn login-form [cursor owner]
  (reify
    om/IInitState
    (init-state [this]
      {:flash-message ""})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.modal.fade#login-form
        [:div.modal-dialog
         [:h3 "Log in"]
         [:p.flash-message (:flash-message state)]
         [:form {:on-submit #(handle-login % owner)}
          [:p [:input {:type "text" :placeholder "Username" :name "username"}]]
          [:p [:input {:type "password" :placeholder "Password" :name "password"}]]
          [:p [:button "Log in"]
              [:button {:data-dismiss "modal"} "Cancel"]]]]]))))

(defn auth-forms [cursor owner]
  (om/component
   (sab/html
    [:div
     (om/build register-form cursor)
     (om/build login-form cursor)])))

(om/root auth-menu app-state {:target (. js/document (getElementById "right-menu"))})
(om/root auth-forms app-state {:target (. js/document (getElementById "auth-forms"))})
