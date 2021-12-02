(ns web.pages
  (:require
   [cheshire.core :as json]
   [cljc.java-time.instant :as inst]
   [hiccup.page :as hiccup]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [ring.middleware.anti-forgery :as anti-forgery]
   [web.config :refer [frontend-version server-mode]]
   [web.utils :refer [response]]))

(defn index-page
  ([req] (index-page req nil nil))
  ([{:keys [user]} og replay-id]
   (hiccup/html5
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=0.6, minimal-ui"}]
      [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
      [:meta {:property "og:type" :content (:type og "website")}]
      [:meta {:property "og:url" :content (:url og "https://jinteki.net")}]
      [:meta {:property "og:image" :content (:image og "https://www.jinteki.net/img/icons/jinteki_167.png")}]
      [:meta {:property "og:title" :content (:title og "Play Android: Netrunner in your browser")}]
      [:meta {:property "og:site_name" :content (:site_name og "jinteki.net")}]
      [:meta {:property "og:description" :content (:description og "Build Netrunner decks and test them online against other players.")}]
      [:link {:rel "apple-touch-icon" :href "/img/icons/jinteki_167.png"}]
      [:title "Jinteki"]
      (hiccup/include-css "/css/carousel.css")
      (hiccup/include-css (str "/css/netrunner.css?v=" @frontend-version))
      (hiccup/include-css "/lib/toastr/toastr.min.css")
      (hiccup/include-css "/lib/jqueryui/themes/base/jquery-ui.min.css")]
     [:body
      [:div {:style {:display "hidden"}
             :id "server-originated-data"
             :data-version @frontend-version
             :data-replay-id replay-id}]
      [:div#main-content]
      [:audio#ting
       [:source {:src "/sound/ting.mp3" :type "audio/mp3"}]
       [:source {:src "/sound/ting.ogg" :type "audio/ogg"}]]
      (hiccup/include-js "/lib/jquery/jquery.min.js")
      (hiccup/include-js "/lib/jqueryui/jquery-ui.min.js")
      (hiccup/include-js "/lib/bootstrap/dist/js/bootstrap.js")
      (hiccup/include-js "/lib/moment/min/moment.min.js")
      (hiccup/include-js "/lib/toastr/toastr.min.js")
      (hiccup/include-js "/lib/howler/dist/howler.min.js")
      [:div#sente-csrf-token {:data-csrf-token (force anti-forgery/*anti-forgery-token*)}]
      [:script {:type "text/javascript"}
       (str "var user=" (json/generate-string user) ";")]

      (if (= "dev" @server-mode)
        (list (hiccup/include-js "/cljs/goog/base.js")
              (hiccup/include-js (str "/cljs/app10.js?v=" @frontend-version))
              [:script
               (for [req ["dev.figwheel"]]
                 (str "goog.require(\"" req "\");"))])
        (list (hiccup/include-js (str "/js/app10.js?v=" @frontend-version))
              [:script
               "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');
               ga('create', 'UA-20250150-2', 'www.jinteki.net');"
               (when user
                 (str "ga('set', '&uid', '" (:username user) "');"))
               "ga('send', 'pageview');"]))])))

(defn reset-password-page
  [{db :system/db
    {:keys [token]} :params}]
  (if (mc/find-one-as-map db "users"
                          {:resetPasswordToken token
                           :resetPasswordExpires {"$gt" (inst/now)}})
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
         [:button.btn.btn-primary {:type "submit"} "Update Password"]]]])
    (response 404 {:message "Sorry, but that reset token is invalid or has expired."})))
