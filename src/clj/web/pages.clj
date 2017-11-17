(ns web.pages
  (:require [web.utils :refer [response]]
            [hiccup.page :as hiccup]
            [cheshire.core :as json]
            ))


(defn layout [{:keys [version] :as req} & content]
  (hiccup/html5
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=0.6, minimal-ui"}]
     [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
     [:title "Jinteki"]
     (hiccup/include-css "/css/carousel.css")
     (hiccup/include-css "/css/netrunner.css")
     (hiccup/include-css "/lib/toastr/toastr.min.css")
     (hiccup/include-css "/lib/jqueryui/themes/base/jquery-ui.min.css")]
    [:body
     content
     (hiccup/include-js "/lib/jquery/jquery.min.js")
     (hiccup/include-js "/lib/jqueryui/jquery-ui.min.js")
     (hiccup/include-js "/lib/bootstrap/dist/js/bootstrap.js")
     (hiccup/include-js "/lib/moment/min/moment.min.js")
     (hiccup/include-js "/lib/marked/marked.min.js")
     (hiccup/include-js "/lib/toastr/toastr.min.js")
     (hiccup/include-js "/lib/howler/howler.min.js")
     [:script {:type "text/javascript"}
      (str "var user=" (json/generate-string (:user req)))]

     [:script (str "var iourl = window.location.origin;")]

     (hiccup/include-js "/cljs/goog/base.js")
     (hiccup/include-js (str "js/app.js?v=" version))
     [:script
      (for [req ["netrunner.appstate"
                 "netrunner.main"
                 "netrunner.ajax"
                 "netrunner.auth"
                 "netrunner.chat"
                 "netrunner.gameboard"
                 "netrunner.gamelobby"
                 "netrunner.cardbrowser"
                 "netrunner.deckbuilder"
                 "netrunner.help"
                 "netrunner.about"
                 "netrunner.account"
                 "netrunner.stats"
                 "netrunner.news"]]
            (str "goog.require(\"" req "\");"))
      (str "goog.require(\"dev.figwheel\");")]
     ]))


(defn index-page [req]
  (layout
    req

     [:nav.topnav.blue-shade
      [:div#left-menu]
      [:div#right-menu]
      [:div#status]]
     [:div#auth-forms]
     [:div#main.carousel.slide {:data-interval "false"}
      [:div.carousel-inner
       [:div.item.active
        [:div.home-bg]
        [:div.container
         [:h1 "Play Android: Netrunner in your browser"]
         [:div#news]
         [:div#chat]]]
       [:div.item
        [:div.cardbrowser-bg]
        [:div#cardbrowser]]
       [:div.item
        [:div.deckbuilder-bg]
        [:div.container
          [:div#deckbuilder]]]
       [:div.item
        [:div#gamelobby]
        [:div#gameboard]]
       [:div.item
        [:div.help-bg]
        [:div#help]]
       [:div.item
        [:div.account-bg]
        [:div#account]]
       [:div.item
        [:div.stats-bg]
        [:div#stats]]
       [:div.item
        [:div.about-bg]
        [:div#about]]]]
    [:audio#ting
      [:source {:src "/sound/ting.mp3" :type "audio/mp3"}]
     [:source {:src "/sound/ting.ogg" :type "audio/ogg"}]]
    ))

(defn announce-page [req]
  (hiccup/html5
    [:head
     [:title "Announce"]
     (hiccup/include-css "/css/netrunner.css")]
    [:body
     [:div.reset-bg]
     [:form.panel.blue-shade.reset-form {:method "POST"}
      [:h3 "Announcement"]
      [:p
       [:textarea.form-control {:rows 5 :style "height: 80px; width: 250px"
                                :name "message" :autofocus true :required "required"}]]
      [:p
       [:button.btn.btn-primary {:type "submit"} "Submit"]]]]))

(defn version-page [{:keys [version] :as req}]
  (hiccup/html5
    [:head
     [:title "App Version"]
     (hiccup/include-css "/css/netrunner.css")]
    [:body
     [:div.reset-bg]
     [:form.panel.blue-shade.reset-form {:method "POST"}
      [:h3 "App Version"]
      [:p
       [:input {:type "text" :name "version" :value version}]]
      [:p
       [:button.btn.btn-primary {:type "submit"} "Submit"]]]]))