(ns nr.about
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<!]]
   [nr.ajax :refer [GET]]
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr tr-element]]
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
          (tr-element :h3 [:about_about "About"])
          ;; note - I couldn't think of a good way to do this in terms of a single tr call
          ;; without requiring us to parse the output and insert the href in afterwards,
          ;; so I moved it slightly down a little into the dev team area
          (tr-element :p [:about_founded-by "This website was founded by @mtgred, an avid Netrunner player from Belgium. The goal is to provide a great way to create and test Netrunner decks online."])
          (tr-element :h3 [:about_development "Development"])
          (tr-element :h4 [:about_software-development-team "Software Development Team"])
          [:ul.list.compact
           [:li (linked-person "@mtgred" "http://twitter.com/mtgred") ": " (tr-element :span [:about_founder-attribution "Founder, original sole developer. Retired."])]
           [:li (linked-person "NoahTheDuke" "https://noahbogart.com/") ": " (tr-element :span [:about_maintainer-attribution "Project maintainer, lead developer."])]
           [:li
            (linked-person "nbkelly" "https://ko-fi.com/nbkelly") ", "
            (linked-person "butzopower" "https://github.com/butzopower") ", "
            (linked-person "francescopellegrini" "https://github.com/francescopellegrini") ": "
            (tr-element :span [:about_active-contributors "Current active contributors."])]
           [:li [:a {:href "https://github.com/mtgred/netrunner/graphs/contributors" :target "_blank" :data-i18-key :about_past-contributors} (tr [:about_past-contributors "Many past contributors"])]]]

          (tr-element :h4 [:about_content-creators "Content Creators"])
          [:ul.list.compact
           [:li
            (linked-person "0thmxma" "https://web-cdn.bsky.app/profile/0thmxma.bsky.social") ", "
            (linked-person "Sanjay" "https://nullsignal.games/blog/author/sanjay-kulkacek/") ", "
            "quarg, znsolomon, hbarsquared, yankeeflatline, rumirumirumirumi: "
            (tr-element :span [:about_start-of-game-quotes "Corp and Runner quotes for start-of-game splash screen."])]
           [:li (linked-person "nbkelly" "https://ko-fi.com/nbkelly") ": "
            (tr-element :span [:about_translated-images "Processing/handling of translated NSG card images, and card backs for community tournaments."])]
           [:li (linked-person "nbkelly" "https://ko-fi.com/nbkelly") ", " (linked-person "xiaat" "https://github.com/xiaat") ": "
            (tr-element :span [:about_alt-art-management "Management/handling/processing of community alt arts for jinteki.net. If you want your art on jinteki.net, contact one of us."])]

           [:li "PopTartNZ: " (tr-element :span [:about_high-res-images "High-resolution scans for FFG cards."])]
           [:li
            (linked-person "Rhahi" "https://github.com/Rhahi") ": Labelling and other QoL functionality ported with permission from "
            [:a {:href "https://addons.mozilla.org/en-US/firefox/addon/cyberfeeder/" :target "_blank"} "Cyberfeeder firefox plugin"]]
           [make-artists alt-info]]

          [:h4 "UI Translators"]
          [:ul.list.compact
           [:li "Chinese (Simplified): bbbbbbbbba, klingeling"]
           [:li "French: canisinhorto"]
           [:li "Japanese: csbisa"]
           [:li "Korean: Seojun Park"]
           [:li "Pig-Latin: jwarwick"]
           [:li "Polish: vesperius"]
           [:li "Portuguese: Vacilotto"]
           [:li "Russian: xiaat"]]

          [:h4 "Tech Stack"]
          [:ul.list.compact
           [:li [:b "Game engine:"] " Clojure. Card data from " [:a {:href "https://netrunnerdb.com/" :target "_blank"} "NetrunnerDB"] " API."]
           [:li [:b "Server:"] " Clojure. Ring and Compojure running on http-kit. Sente for websocket communications."]
           [:li [:b "Front-end client:"] " ClojureScript. Reagent (React). "]]

          [:p "The code is open source and available on "
           [:a {:href "https://github.com/mtgred/netrunner" :target "_blank"} "Github"] "."]

          [:p "Bug reports and feature suggestions can be submitted "
           [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "here"] "."]

          [:p "Card automation "
           [:a {:href "https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml"
                :target "_blank"} "status"] "."]

          [:h3 "Donations"]
          [:p "Donations are appreciated and help finance fast servers. You can support the project financially with PayPal or Bitcoin. Alternate art cards will be enabled on your account as a token of gratitude. Please specify your username with your donation."]
          [:ul.list.compact
           [:li "PayPal: mtgred@gmail.com or " [:a {:href "https://www.paypal.me/mtgred" :title "PayPal" :target "_blank"} "paypal.me/mtgred"]]
           [:li "Bitcoin: " [:span.bitcoin "1MRRtCsZYGdgwvRo4NMhmo14q7KJNtAiKL" [:img.qr {:src "/img/bitcoin.png" :alt "Bitcoin QR Code"}]]]]

          [:p "Many thanks to all the donors. Your contributions and kind words are greatly appreciated. You help finance fast servers."]
          [:ul.list.compact
           (for [d @donors]
             ^{:key d}
             [:li d])]
          [:h3 "Disclaimer"]
          [:p "Netrunner is a trademark of Fantasy Flight Publishing, Inc. and/or Wizards of the Coast LLC."]
          [:p "This is website is not affiliated with Fantasy Flight Games or Wizards of the Coast."]
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
