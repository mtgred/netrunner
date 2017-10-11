(ns meccg.about
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [meccg.ajax :refer [GET]]))

(def app-state (atom {}))

(go (swap! app-state assoc :donators (:json (<! (GET "/data/donators")))))

(defn about [cursor owner]
  (om/component
   (sab/html
    [:div.about.panel.content-page.blue-shade
     [:h3 "About"]
     [:p "This website is created and run by avid Middle-earth CCG players from around the world. The goal is to provide a great way to create and test MECCG decks online, and keep the game alive."]

     [:h3 "Contact"]
     [:p "Twitter: " [:a {:href "http://twitter.com/rezwits" :target "_blank"} "@rezwits"]]
     [:p "Email: " [:a {:href "mailto:rezwits@gmail.com"} "rezwits@gmail.com"]]

     [:h3 "Development"]

     [:p "The code is open source and available on "
      [:a {:href "https://github.com/rezwits/cardnum" :target "_blank"} "Github"] "."]

     [:p "Bug reports and feature suggestions can be submitted "
      [:a {:href "https://github.com/rezwits/cardnum/issues" :target "_blank"} "here"] "."]

     [:p "Card automation "
      [:a {:href "https://docs.google.com/spreadsheets/d/";;"1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml"
           :target "_blank"} "status"] "."]

     [:h3 "Pre-fork Contributors"]
     [:p "A big props to Neal Terrell (nealpro), Joel Koepp (JoelCFC25), Dominic Kexel (queueseven), Lukasz Dobrogowski (zaroth), Filip Gokstorp (Saintis) and Felix Laurie von Massenbach (erbridge) who actively contribute to the success of the project with regular code submissions, the management of the issue tracker and answers to questions in the chat. It would not be the same without your help."]

     [:h3 "Donations"]
     [:p "Donations are appreciated and help finance fast servers. You can support the project financially with PayPal or Bitcoin. Alternate art cards will be enabled on your account as a token of gratitude. Please specify your username with your donation."]
     [:ul.list.compact
      [:li "PayPal: rezwits@me.com or " [:a {:href "https://www.paypal.me/rezwits" :title "PayPal" :target "_blank"} "paypal.me/rezwits"]]
      [:li "Bitcoin: " [:span.bitcoin "<none>" [:img.qr {:src "/img/bitcoin.png"}]]]
      ]

     [:p "Many thanks to all the donors. Your contributions and kind words are greatly appreciated. You help finance fast servers and keep the developer motivated."]
     [:ul.list.compact
      (for [d (:donators cursor)]
        [:li d])]
     [:h3 "Disclaimer"]
     [:p "Middle-earth CCG and LotR are trademarks of Middle-earth Enterprises and/or Iron Crown Enterprises."]
     [:p "This is website is not affiliated with Middle-earth Enterprises or Iron Crown Enterprises."]
     [:p "Targeting icon made by "
      [:a {:href "http://www.freepik.com" :title "Freepik" :target "_blank"} "Freepik"]
      " from "
      [:a {:href "http://www.flaticon.com" :title "Flaticon" :target "_blank"} "www.flaticon.com"]
      " is licensed under "
      [:a {:href "http://creativecommons.org/licenses/by/3.0/" :title "Creative Commons BY 3.0" :target "_blank"} "CC BY 3.0"]]])))

(om/root about app-state {:target (. js/document (getElementById "about"))})
