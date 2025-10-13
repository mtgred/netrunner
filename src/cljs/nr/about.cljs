(ns nr.about
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [nr.ajax :refer [GET]]
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr tr-element tr-span]]
   [nr.utils :refer [set-scroll-top store-scroll-top]]
   [reagent.core :as r]))

(defn- single-artist [info]
  ^{:key (:name info)}
  [:li [:a {:href (:artist-link info "#")} (:name info)] ": " (:artist-about info)])

(defn- make-artists [alt-info]
  [:<> (->> @alt-info
            (filter #(contains? % :artist-about))
            (map single-artist))])

(defn- linked-person [name url] [:a {:href url :target "_blank"} name])

(defn about-content [state scroll-top]
  (r/with-let [donors (r/cursor state [:donors])
               alt-info (r/cursor app-state [:alt-info])
               !node-ref (r/atom nil)]
    (r/create-class
      {:display-name "about-content"
       :component-did-mount (fn [_] (set-scroll-top @!node-ref @scroll-top))
       :component-will-unmount (fn [_] (store-scroll-top @!node-ref scroll-top))
       :reagent-render
       (fn [_ _]
         [:div.about.panel.content-page.blue-shade {:ref #(reset! !node-ref %)}
          [tr-element :h3 [:about_about "About"]]
          [tr-element :p [:about_founded-by "This website was founded by @mtgred, an avid Netrunner player from Belgium. The goal is to provide a great way to create and test Netrunner decks online."]]
          [tr-element :h3 [:about_development "Development"]]
          [tr-element :h4 [:about_software-development-team "Software Development Team"]]
          [:ul.list.compact
           [:li [linked-person "@mtgred" "http://twitter.com/mtgred"] ": " [tr-span [:about_founder-attribution "Founder, original sole developer. Retired."]]]
           [:li [linked-person "NoahTheDuke" "https://noahbogart.com/"] ": " [tr-span [:about_maintainer-attribution "Project maintainer, lead developer."]]]
           [:li
            [linked-person "nbkelly" "https://ko-fi.com/nbkelly"] ", "
            [linked-person "butzopower" "https://github.com/butzopower"] ", "
            [linked-person "francescopellegrini" "https://github.com/francescopellegrini"] ": "
            [tr-span [:about_active-contributors "Current active contributors."]]]
            [:li [:a {:href "https://github.com/mtgred/netrunner/graphs/contributors" :target "_blank" :data-i18n-key :about_past-contributors} (tr [:about_past-contributors "Many past contributors"])]]]

          [tr-element :h4 [:about_content-creators "Content Creators"]]
          [:ul.list.compact
           [:li
            [linked-person "0thmxma" "https://web-cdn.bsky.app/profile/0thmxma.bsky.social"] ", "
            [linked-person "Sanjay" "https://nullsignal.games/blog/author/sanjay-kulkacek/"] ", "
            "quarg, "
            [linked-person "znsolomon" "https://contactthearchivists.podbean.com/"]
            ", hbarsquared, yankeeflatline, rumirumirumirumi: "
            [tr-span [:about_start-of-game-quotes "Corp and Runner quotes for start-of-game splash screen."]]]
           [:li [linked-person "nbkelly" "https://ko-fi.com/nbkelly"] ": "
            [tr-span [:about_translated-images "Processing/handling of translated NSG card images, and card backs for community tournaments."]]]
           [:li [linked-person "nbkelly" "https://ko-fi.com/nbkelly"] ", " [linked-person "xiaat" "https://github.com/xiaat"] ": "
            [tr-span [:about_alt-art-management "Management/handling/processing of community alt arts for jinteki.net. If you want your art on jinteki.net, contact one of us."]]]
           [:li "PopTartNZ: " [tr-span [:about_high-res-images "High-resolution scans for FFG cards."]]]
           [:li
            [linked-person "Rhahi" "https://github.com/Rhahi"] ": Labelling and other QoL functionality ported with permission from "
            [:a {:href "https://addons.mozilla.org/en-US/firefox/addon/cyberfeeder/" :target "_blank"} "Cyberfeeder firefox plugin"]]
           [make-artists alt-info]]

          [tr-element :h4 [:about_ui-translators "UI Translators"]]
          [:ul.list.compact
           [:li [tr-span [:lang_chinese_simp "Chinese (Simplified)"]] ": " [linked-person "bbbbbbbbba" "https://github.com/bbbbbbbbba"] ", " [linked-person "klingeling" "https://github.com/klingeling"]]
           [:li [tr-span [:lang_french "French"]] ": canisinhorto"]
           [:li [tr-span [:lang_japanese "Japanese"]] ": " [linked-person "csbisa" "https://github.com/csbisa"]]
           [:li [tr-span [:lang_korean "Korean"]] ": Seojun Park"]
           [:li [tr-span [:lang_pig-latin "Pig-Latin"]] ": " [linked-person "jwarwick" "https://github.com/jwarwick"]]
           [:li [tr-span [:lang_polish "Polish"]] ": " [linked-person "vesperius" "https://vesper.cyberpunk.me/"]]
           [:li [tr-span [:lang_portuguese "Portuguese"]] ": Vacilotto"]
           [:li [tr-span [:lang_russian "Russian"]] ": " [linked-person "xiaat" "https://github.com/xiaat"]]]

          [tr-element :h4 [:about_tech-stack "Tech Stack"]]
          [:ul.list.compact
           [:li [tr-element :b [:about_game-engine "Game engine:"]] " Clojure. Card data from " [:a {:href "https://netrunnerdb.com/" :target "_blank"} "NetrunnerDB"] " API."]
           [:li [tr-element :b [:about_server "Server:"]] " Clojure. Ring and Compojure running on http-kit. Sente for websocket communications."]
           [:li [tr-element :b [:about_front-end-client "Front-end client:"]] " ClojureScript. Reagent (React). "]]

          [:p [tr-span [:about_open-source "The code is open source and available on"]] " " [:a {:href "https://github.com/mtgred/netrunner" :target "_blank"} "Github"] "."]

          [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank" :data-i18n-key :about_bug-reports} (tr [:about_bug-reports "Bug reports and feature suggestions can be submitted here."])]

          [tr-element :h3 [:about_donations "Donations"]]
          [tr-element :p [:about_donations-long "Donations are appreciated and help finance fast servers. You can support the project financially with PayPal or Bitcoin. Alternate art cards will be enabled on your account as a token of gratitude. Please specify your username with your donation."]]
          [:ul.list.compact
           [:li "PayPal: mtgred@gmail.com or " [:a {:href "https://www.paypal.me/mtgred" :title "PayPal" :target "_blank"} "paypal.me/mtgred"]]
           [:li "Bitcoin: " [:span.bitcoin "1MRRtCsZYGdgwvRo4NMhmo14q7KJNtAiKL" [:img.qr {:src "/img/bitcoin.png" :alt "Bitcoin QR Code"}]]]]

          [tr-element :p [:about_thank-you "Many thanks to all the donors. Your contributions and kind words are greatly appreciated. You help finance fast servers."]]
          [:ul.list.compact
           (for [d @donors]
             ^{:key d}
             [:li d])]
          [tr-element :h3 [:about_disclaimer "Disclaimer"]]
          [tr-element :p [:about_netrunner-trademark "Netrunner is a trademark of Fantasy Flight Publishing, Inc. and/or Wizards of the Coast LLC."]]
          [tr-element :p [:about_unaffiliated "This is website is not affiliated with Fantasy Flight Games or Wizards of the Coast."]]
          [:p "Targeting icon made by "
           [:a {:href "http://www.freepik.com" :title "Freepik" :target "_blank"} "Freepik"]
           " from "
           [:a {:href "http://www.flaticon.com" :title "Flaticon" :target "_blank"} "www.flaticon.com"]
           " is licensed under "
           [:a {:href "http://creativecommons.org/licenses/by/3.0/" :title "Creative Commons BY 3.0" :target "_blank"} "CC BY 3.0"]]])})))

(defn about []
  (let [about-state (r/atom {})
        scroll-top (atom 0)]
    (go (swap! about-state assoc :donors (:json (<! (GET "/data/donors")))))
    (fn []
      [:div.page-container
       [:div.about-bg]
       [about-content about-state scroll-top]])))
