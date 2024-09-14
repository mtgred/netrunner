(ns web.pages
  (:require
   [cheshire.core :as json]
   [cljc.java-time.instant :as inst]
   [hiccup.page :as hiccup]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [ring.middleware.anti-forgery :as anti-forgery]
   [web.utils :refer [response html-response]]
   [web.versions :refer [frontend-version]]
   [stringer.core :as s]))

(defn index-page
  ([request] (index-page request nil nil))
  ([{user :user server-mode :system/server-mode} og replay-id]
   (html-response
     200
     (hiccup/html5
       [:head
        [:meta {:charset "utf-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=0.6, minimal-ui"}]
        [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
        [:meta {:property "og:type" :content (:type og "website")}]
        [:meta {:property "og:url" :content (:url og "https://jinteki.net")}]
        [:meta {:property "og:image" :content (:image og "https://www.jinteki.net/img/icons/jinteki_167.png")}]
        [:meta {:property "og:title" :content (:title og "Play Netrunner in your browser")}]
        [:meta {:property "og:site_name" :content (:site_name og "jinteki.net")}]
        [:meta {:property "og:description" :content (:description og "Build Netrunner decks and test them online against other players.")}]
        [:link {:rel "apple-touch-icon" :href "/img/icons/jinteki_167.png"}]
        [:title "Jinteki"]
        (hiccup/include-css "/lib/css/toastr.min.css")
        (if (= "dev" server-mode)
          (hiccup/include-css "/css/netrunner.css")
          (hiccup/include-css (s/strcat "/css/netrunner.css?v=" @frontend-version)))]
       [:body
        [:div#sente-csrf-token
         {:style {:display "hidden"}
          :data-csrf-token (force anti-forgery/*anti-forgery-token*)}]
        [:div {:style {:display "hidden"}
               :id "server-originated-data"
               :data-version @frontend-version
               :data-replay-id replay-id}]
        [:div#main-content]
        [:audio#ting
         [:source {:src "/sound/ting.mp3" :type "audio/mp3"}]
         [:source {:src "/sound/ting.ogg" :type "audio/ogg"}]]
        (hiccup/include-js "https://code.jquery.com/jquery-2.1.1.min.js")
        (hiccup/include-js "https://code.jquery.com/ui/1.13.0/jquery-ui.min.js")
        (hiccup/include-js "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
        (hiccup/include-js "/lib/js/toastr.min.js")
        [:script {:type "text/javascript"}
         (s/strcat "var user=" (json/generate-string user) ";")]
        (if (= "dev" server-mode)
          (list (hiccup/include-js "/js/cljs-runtime/goog.base.js")
                (hiccup/include-js "/js/main.js"))
          (list (hiccup/include-js (s/strcat "/js/main.js?v=" @frontend-version))))]))))

(defn reset-password-page
  [{db :system/db
    {:keys [token]} :path-params}]
  (if (mc/find-one-as-map db "users"
                          {:resetPasswordToken token
                           :resetPasswordExpires {"$gt" (inst/now)}})
    (html-response
      200
      (hiccup/html5
        [:head
         [:title "Jinteki"]
         (hiccup/include-css "/css/netrunner.css")]
        [:body
         [:div.reset-bg]
         [:form.panel.blue-shade.reset-form {:method "POST"}
          [:h3 "Password Reset"]
          [:p
           [:input.form-control {:type "password"
                                 :name "password"
                                 :value ""
                                 :placeholder "New password"
                                 :autofocus true
                                 :required "required"}]]
          [:p
           [:input.form-control {:type "password"
                                 :name "confirm"
                                 :value ""
                                 :placeholder "Confirm password"
                                 :required "required"}]]
          [:p
           [:button.btn.btn-primary {:type "submit"} "Update Password"]]]]))
    (response 404 {:message "Sorry, but that reset token is invalid or has expired."})))
