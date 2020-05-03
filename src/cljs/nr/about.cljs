(ns nr.about
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [GET]]
            [nr.appstate :refer [app-state]]
            [reagent.core :as r]))

(def about-state (r/atom {}))

(go (swap! about-state assoc :donators (:json (<! (GET "/data/donors")))))

(defn about []
  (r/with-let [active (r/cursor app-state [:active-page])]
    (when (= "/about" (first @active))
      (let [donators (r/cursor about-state [:donators])]
        (fn []
          [:div.about.panel.content-page.blue-shade
           [:h3 "About"]
           [:p "This website was founded by " [:a {:href "http://twitter.com/mtgred" :target "_blank"} "@mtgred"]
            ", an avid Netrunner player from Belgium. The goal is to provide a great way to create and test Netrunner decks online."]

           [:h3 "Development"]
           [:h4 "The Team"]
           [:ul.list.compact
            [:li "mtgred: Founder, original sole developer. Retired."]
            [:li "nealterrell: Project maintainer, lead developer."]
            [:li "Saintis, danhut, jwarwick, NoahTheDuke, nicohasa, presheaf, Msbeck: Primary contributors."]
            [:li "JoelCFC25, domtancredi, Zaroth, queueseven, erbridge, and "
             [:a {:href "https://github.com/mtgred/netrunner/graphs/contributors" :target "_blank"} "many more"]
             ": Past contributors."]
            [:li "0thmxma, Sanjay, quarg, znsolomon, hbarsquared, yankeeflatline, rumirumirumirumi: Corp and Runner quotes for start-of-game splash screen."]]

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
            [:li "Bitcoin: " [:span.bitcoin "1ByjuLNZRvG17YaRKgKUjNi3c4qQ3daX3g" [:img.qr {:src "/img/bitcoin.png" :alt "Bitcoin QR Code"}]]]]

           [:p "Many thanks to all the donors. Your contributions and kind words are greatly appreciated. You help finance fast servers."]
           [:ul.list.compact
            (for [d @donators]
              ^{:key d}
              [:li d])]
           [:h3 "Disclaimer"]
           [:p "Netrunner and Android are trademarks of Fantasy Flight Publishing, Inc. and/or Wizards of the Coast LLC."]
           [:p "This is website is not affiliated with Fantasy Flight Games or Wizards of the Coast."]
           [:p "Targeting icon made by "
            [:a {:href "http://www.freepik.com" :title "Freepik" :target "_blank"} "Freepik"]
            " from "
            [:a {:href "http://www.flaticon.com" :title "Flaticon" :target "_blank"} "www.flaticon.com"]
            " is licensed under "
            [:a {:href "http://creativecommons.org/licenses/by/3.0/" :title "Creative Commons BY 3.0" :target "_blank"} "CC BY 3.0"]]])))))
