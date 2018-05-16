(ns nr.auth
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put!] :as async]
            [netrunner.ajax :refer [POST GET]]
            [netrunner.appstate :refer [app-state]]
            [reagent.core :as r]))

(def auth-state (r/atom {}))

(defn authenticated [f]
  (if-let [user (:user @app-state)]
    (f user)
    (.modal (js/$ "#login-form") "show")))

(defn handle-post [event url ref]
  (.preventDefault event)
  (swap! auth-state assoc :flash-message "")
  (let [params (-> event .-target js/$ .serialize)
        _ (.-serialize (js/$ (.-target event)))] ;; params is nil when built in :advanced mode. This fixes the issue.
    (go (let [response (<! (POST url params))]
          (if (and (= (:status response) 200) (= (:owner "/forgot") "/forgot") )
            (swap! auth-state assoc :flash-message "Reset password sent")
            (case (:status response)
              401 (swap! auth-state assoc :flash-message "Invalid login or password")
              421 (swap! auth-state assoc :flash-message "No account with that email address exists")
              422 (swap! auth-state assoc :flash-message  "Username taken")
              423 (swap! auth-state assoc :flash-message  "Username too long")
              424 (swap! auth-state assoc :flash-message "Email already used")
              (-> js/document .-location (.reload true))))))))

(defn handle-logout [event]
  (.preventDefault event)
  (go (let [response (<! (POST "/logout" nil))]
        (-> js/document .-location (.reload true)))))

(defn avatar [emailhash username opts]
  (when emailhash
    [:img.avatar
     {:src (str "https://www.gravatar.com/avatar/" emailhash "?d=retro&s=" (:size opts))
      :alt username}]))

(defn logged-menu [user]
  [:ul
   [:li.dropdown.usermenu
    [:a.dropdown-toggle {:href "" :data-toggle "dropdown"}
     [avatar (:emailhash user) (:username user) {:size 22}]
     (:username user)
     [:b.caret]]
    [:div.dropdown-menu.blue-shade.float-right
     [:a.block-link {:href "/account"} "Settings"]
     [:a.block-link {:on-click #(handle-logout %)} "Logout"]]]])

(defn unlogged-menu []
  [:ul
   [:li
    [:a {:href "" :data-target "#register-form" :data-toggle "modal"
         :on-click (fn [] .focus (js/$ "input[name='email']"))} "Sign up"]]
   [:li
    [:a {:href "" :data-target "#login-form" :data-toggle "modal"} "Login"]]])

(defn auth-menu []
  (if-let [user (:user @app-state)]
    [logged-menu user]
    [unlogged-menu]))

(defn check-username [value]
  (go (let [response (<! (GET (str "/check/" value)))]
        (case (:status response)
          422 (swap! auth-state assoc :flash-message "Username taken")
          423 (swap! auth-state assoc :flash-message "Username too short/too long")
          (swap! auth-state assoc :flash-message "")))))

(defn check-email [value]
  (go (let [response (<! (GET (str "/check/" value)))]
        (if (= (:status response) 421)
          (swap! auth-state assoc :flash-message "No account with that email address exists")
          (swap! auth-state assoc :flash-message "")))))

(defn valid-email? [email]
  (let [pattern #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"]
    (and (string? email) (re-matches pattern (.toLowerCase email)))))

(defn register [event email username password]
  (.preventDefault event)
   (cond
     (empty? email) (swap! auth-state assoc :flash-message "Email can't be empty")
     (empty? username) (swap! auth-state assoc :flash-message "Username can't be empty")
     (not (valid-email? email)) (swap! auth-state assoc :flash-message "Please enter a valid email address")
     (< 20 (count username)) (swap! auth-state assoc :flash-message "Username must be 20 characters or shorter")
     (empty? password) (swap! auth-state assoc :flash-message "Password can't be empty")
     :else (handle-post event "/register" "register-form")))

(defn register-form [flash-message]
  (swap! auth-state assoc :flash-message "")
  (let [email (r/cursor auth-state [:email])
        username (r/cursor auth-state [:username])
        password (r/cursor auth-state [:password])]
    (fn []
      [:div#register-form.modal.fade {:ref "register-form"}
       [:div.modal-dialog
        [:h3 "Create an account"]
        [:p.flash-message @flash-message]
        [:form {:on-submit #(register % @email @username @password)}
         [:p [:input {:type "text" :placeholder "Email" :name "email" :ref "email" :value @email
                      :on-change #(reset! email (-> % .-target .-value))
                      :on-blur #(when-not (valid-email? (.. % -target -value))
                                  (swap! auth-state assoc :flash-message "Please enter a valid email address"))}]]
         [:p [:input {:type "text" :placeholder "Username" :name "username" :ref "username" :value @username
                      :on-change #(reset! username (-> % .-target .-value))
                      :on-blur #(check-username (-> % .-target .-value)) :maxLength "16"}]]
         [:p [:input {:type "password" :placeholder "Password" :name "password" :ref "password"
                      :value @password :on-change #(reset! password (-> % .-target .-value))}]]
         [:p [:button "Sign up"]
          [:button {:data-dismiss "modal"} "Cancel"]]]
        [:p "Already have an account? " \
         [:span.fake-link {:on-click #(.modal (js/$ "#login-form") "show")
                           :data-dismiss "modal"} "Log in"]]
        [:p "Need to reset your password? "
         [:span.fake-link {:on-click #(.modal (js/$ "#forgot-form") "show")
                           :data-dismiss "modal"} "Reset"]]]])))

(defn forgot-form [flash-message]
  (swap! auth-state assoc :flash-message "")
  (fn []
    [:div#forgot-form.modal.fade {:ref "forgot-form"}
     [:div.modal-dialog
      [:h3 "Reset your Password"]
      [:p.flash-message @flash-message]
      [:form {:on-submit #(handle-post % "/forgot" "forgot-form")}
       [:p [:input {:type "text" :placeholder "Email" :name "email"
                    :on-blur #(check-email (-> % .-target .-value))}]]
       [:p [:button "Submit"]
        [:button {:data-dismiss "modal"} "Cancel"]]
       [:p "No account? "
        [:span.fake-link {:on-click #(.modal (js/$ "#register-form") "show")
                          :data-dismiss "modal"} "Sign up!"]]]]]))

(defn login-form [flash-message]
  (swap! auth-state assoc :flash-message "")
  (fn []
    [:div#login-form.modal.fade {:ref "login-form"}
     [:div.modal-dialog
      [:h3 "Log in"]
      [:p.flash-message @flash-message]
      [:form {:on-submit #(handle-post % "/login" "login-form")}
       [:p [:input {:type "text" :placeholder "Username" :name "username"}]]
       [:p [:input {:type "password" :placeholder "Password" :name "password"}]]
       [:p [:button "Log in"]
        [:button {:data-dismiss "modal"} "Cancel"]]
       [:p "No account? "
        [:span.fake-link {:on-click #(.modal (js/$ "#register-form") "show")
                          :data-dismiss "modal"} "Sign up!"]]
       [:p "Forgot your password? "
        [:span.fake-link {:on-click #(.modal (js/$ "#forgot-form") "show")
                          :data-dismiss "modal"} "Reset"]]]]]))

(defn auth-forms [user flash-message]
  (when-not @user
    [:div
     [register-form flash-message]
     [login-form flash-message]
     [forgot-form flash-message]]))
