(ns netrunner.auth
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [netrunner.main :refer [app-state]]
            [netrunner.ajax :refer [POST GET]]))

(defn avatar [{:keys [emailhash]} owner opts]
  (om/component
   (sab/html
    (when emailhash
      [:img.avatar
       {:src (str "http://www.gravatar.com/avatar/" emailhash "?d=retro&s=" (:size opts))}]))))

(defn authenticated [f]
  (if-let [user (:user @app-state)]
    (f user)
    (.modal (js/$ "#login-form") "show")))

(defn logged-menu [user owner]
  (om/component
   (sab/html
    [:li.dropdown.usermenu
     [:a.dropdown-toggle {:href "" :data-toggle "dropdown"}
      (om/build avatar user {:key :username :opts {:size 22}})
      (:username user)
      [:b.caret]]
     [:div.dropdown-menu.blue-shade.float-right
      [:a.block-link {:href "/logout"} "Logout"]]])))

(defn unlogged-menu [user owner]
  (om/component
   (sab/html
    [:ul
     [:li
      [:a {:href "" :data-target "#register-form" :data-toggle "modal"
           :on-click (fn [] .focus (js/$ "input[name='email']"))} "Sign up"]]
     [:li
      [:a {:href "" :data-target "#login-form" :data-toggle "modal"} "Login"]]])))

(defn auth-menu [{:keys [user]} owner]
  (om/component
   (if user
     (om/build logged-menu user)
     (om/build unlogged-menu user))))

(defn handle-post [event owner url ref]
  (.preventDefault event)
  (om/set-state! owner :flash-message "")
  (let [params (-> event .-target js/$ .serialize)
        _ (.-serialize (js/$ (.-target event)))] ;; params is nil when built in :advanced mode. This fixes the issue.
    (go (let [response (<! (POST url params))]
          (if (and (= (:status response) 200) (= (:owner "/forgot") "/forgot") )
            (om/set-state! owner :flash-message "Reset password sent")
            (case (:status response)
                401 (om/set-state! owner :flash-message "Invalid login or password")
                421 (om/set-state! owner :flash-message "No account with that email address exists")
                422 (om/set-state! owner :flash-message "Username taken")
                423 (om/set-state! owner :flash-message "Username too short/too long")
                (-> js/document .-location (.reload true))))))))

(defn check-username [event owner]
  (go (let [response (<! (GET (str "/check/" (.-value (om/get-node owner "username")))))]
        (case (:status response)
          422 (om/set-state! owner :flash-message "Username taken")
          423 (om/set-state! owner :flash-message "Username too short/too long")
          (om/set-state! owner :flash-message "")))))

(defn check-email [event owner]
  (go (let [response (<! (GET (str "/check/" (.-value (om/get-node owner "email")))))]
        (if (= (:status response) 421)
          (om/set-state! owner :flash-message "No account with that email address exists")
          (om/set-state! owner :flash-message "")))))

(defn register [event owner]
  (.preventDefault event)
  (let [username (.-value (om/get-node owner "username"))
        email (.-value (om/get-node owner "email"))
        password (.-value (om/get-node owner "password"))]
    (cond
      (empty? email) (om/set-state! owner :flash-message "Email can't be empty")
      (empty? username) (om/set-state! owner :flash-message "Username can't be empty")
      (> 4 (count username)) (om/set-state! owner :flash-message "Username must be 4 characters or longer")
      (< 16 (count username)) (om/set-state! owner :flash-message "Username must be 16 characters or shorter")
      (empty? password) (om/set-state! owner :flash-message "Password can't be empty")
      :else (handle-post event owner "/register" "register-form"))))

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
         [:form {:on-submit #(register % owner)}
          [:p [:input {:type "text" :placeholder "Email" :name "email" :ref "email"}]]
          [:p [:input {:type "text" :placeholder "Username" :name "username" :ref "username"
                       :on-blur #(check-username % owner) :maxlength "16"}]]
          [:p [:input {:type "password" :placeholder "Password" :name "password" :ref "password"}]]
          [:p [:button "Sign up"]
              [:button {:data-dismiss "modal"} "Cancel"]]]
         [:p "Already have an account? "
          [:span.fake-link {:on-click #(.modal (js/$ "#login-form") "show")
                            :data-dismiss "modal"} "Log in"]]
         [:p "Need to reset your password? "
          [:span.fake-link {:on-click #(.modal (js/$ "#forgot-form") "show")
                            :data-dismiss "modal"} "Reset"]]]]))))

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
                             :data-dismiss "modal"} "Sign up!"]]
          [:p "Forgot your password? "
           [:span.fake-link {:on-click #(.modal (js/$ "#forgot-form") "show")
                             :data-dismiss "modal"} "Reset"]]]]]))))

(defn forgot-form [cursor owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.modal.fade#forgot-form {:ref "forgot-form"}
        [:div.modal-dialog
         [:h3 "Reset your Password"]
         [:p.flash-message (:flash-message state)]
         [:form {:on-submit #(handle-post % owner "/forgot" "forgot-form")}
          [:p [:input {:type "text" :placeholder "Email" :name "email"
                       :on-blur #(check-email % owner)}]]
          [:p [:button "Submit"]
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
       (om/build login-form cursor)
       (om/build forgot-form cursor)]))))

(om/root auth-menu app-state {:target (. js/document (getElementById "right-menu"))})
(om/root auth-forms app-state {:target (. js/document (getElementById "auth-forms"))})
