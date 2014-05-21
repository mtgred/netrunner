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
       [:li.dropdown
        [:a.dropdown-toggle {:href "" :data-toggle "dropdown"} (:username user)
         [:b.caret]]
        [:ul.dropdown-menu.blue-shade.float-right
         [:li
          [:a {:href "/logout"} "Logout"]]]]))))

(defn unlogged-menu [user owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:ul
        [:li
         [:a {:href "" :data-target "#register-form" :data-toggle "modal"} "Sign up"]]
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
          [:p [:input {:type "text" :placeholder "Username" :name "username"}]]
          [:p [:input {:type "text" :placeholder "Email" :name "email"}]]
          [:p [:input {:type "password" :placeholder "Password" :name "password"}]]
          [:p [:button "Sign up"]
              [:button {:data-dismiss "modal"} "Cancel"]]]]]))))

(defn handle-login [e]
  (.preventDefault e)
  (let [params (-> e .-target js/$ .serialize)]
    (go (let [response (<! (POST "/login" params))]
          (if (:error response)
            (.log js/console (str "Login failed: " (:error response)))
            (do (.modal (js/$ "#login-form") "hide")
                (swap! app-state assoc :user response)))))))

(defn login-form [cursor owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.modal.fade#login-form
        [:div.modal-dialog
         [:h3 "Log in"]
         [:form {:on-submit handle-login}
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
