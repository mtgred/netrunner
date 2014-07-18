(ns netrunner.auth
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [netrunner.ajax :refer [POST GET]]))

(def auth-channel (chan))

(def app-state
  (atom {:user (js->clj js/user :keywordize-keys true)}))

(defn avatar [{:keys [emailhash]} owner opts]
  (om/component
   (sab/html
    (when emailhash
      [:img.avatar
       {:src (str "http://www.gravatar.com/avatar/" emailhash "?d=retro&s=" (:size opts))}]))))

(defn authenticated [f]
  (if-let [user (:user @app-state)]
    (f user)
    (.modal (js/$ "#register-form") "show")))

(defn logged-menu [user owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
       [:li.dropdown.usermenu
        [:a.dropdown-toggle {:href "" :data-toggle "dropdown"}
         (om/build avatar user {:key :username :opts {:size 22}})
         (:username user)
         [:b.caret]]
        [:div.dropdown-menu.blue-shade.float-right
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
   (if user
     (om/build logged-menu user)
     (om/build unlogged-menu user))))

(defn handle-post [e owner url ref]
  (.preventDefault e)
  (om/set-state! owner :flash-message "")
  (let [params (-> e .-target js/$ .serialize)]
    (go (let [response (<! (POST url params))]
          (case (:status response)
            401 (om/set-state! owner :flash-message "Invalid login or password")
            422 (om/set-state! owner :flash-message "Username taken")
            (do (.modal (js/$ (om/get-node owner ref)) "hide")
                (let [data (:json response)]
                  (swap! app-state assoc :user (:user data))
                  (put! auth-channel data))))))))

(defn check-username [event owner]
  (go (let [response (<! (GET (str "/check/" (.-value (om/get-node owner "username")))))]
        (if (= (:status response) 422)
          (om/set-state! owner :flash-message "Username taken")
          (om/set-state! owner :flash-message "")))))

(defn register-form [cursor owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.modal.fade#register-form {:ref "register-form"}
        [:div.modal-dialog
         [:h3 "Create an account"]
         [:p.flash-message (:flash-message state)]
         [:form {:on-submit #(handle-post % owner "/register" "register-form")}
          [:p [:input {:type "text" :placeholder "Email" :name "email"}]]
          [:p [:input {:type "text" :placeholder "Username" :name "username" :ref "username"
                       :on-blur #(check-username % owner)}]]
          [:p [:input {:type "password" :placeholder "Password" :name "password"}]]
          [:p [:button "Sign up"]
              [:button {:data-dismiss "modal"} "Cancel"]]]
         [:p "Already have an account? "
          [:span.fake-link {:on-click #(.modal (js/$ "#login-form") "show")
                            :data-dismiss "modal"} "Log in"]]]]))))

(defn login-form [cursor owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.modal.fade#login-form {:ref "login-form"}
        [:div.modal-dialog
         [:h3 "Log in"]
         [:p.flash-message (:flash-message state)]
         [:form {:on-submit #(handle-post % owner "/login" "login-form")}
          [:p [:input {:type "text" :placeholder "Username" :name "username"}]]
          [:p [:input {:type "password" :placeholder "Password" :name "password"}]]
          [:p [:button "Log in"]
              [:button {:data-dismiss "modal"} "Cancel"]]
          [:p "No account? "
            [:span.fake-link {:on-click #(.modal (js/$ "#register-form") "show")
                              :data-dismiss "modal"} "Sign up!"]]]]]))))

(defn auth-forms [cursor owner]
  (om/component
   (sab/html
    (when-not (:user @app-state)
      [:div
       (om/build register-form cursor)
       (om/build login-form cursor)]))))

(om/root auth-menu app-state {:target (. js/document (getElementById "right-menu"))})
(om/root auth-forms app-state {:target (. js/document (getElementById "auth-forms"))})
