(ns netrunner.about
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [netrunner.ajax :refer [GET]]))

(def app-state (atom {}))

(go (swap! app-state assoc :donators (:json (<! (GET "/data/donators")))))

(defn about [cursor owner]
  (om/component
   (sab/html
    [:div.about.panel.blue-shade
     [:h3 "About"]
     [:p "This website is created and runned by an avid Netrunner player from Belgium. The goal is to provide a great way to create and test Netrunner decks online."]

     [:h3 "Contact"]
     [:p "Twitter: " [:a {:href "http://twitter.com/mtgred" :target "_blank"} "@mtgred"]]
     [:p "Email: " [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"]]

     [:h3 "Development"]

     [:p "The gameplay functionnalities are currently under development. Card automation "
      [:a {:href "https://www.dropbox.com/s/rpkhxafgile5spp/Cards%20status.xlsx"
           :target "_blank"} "status"] "."]

     [:p "The code is open source and available on "
      [:a {:href "https://github.com/mtgred/netrunner" :target "_blank"} "Github"] "."]

     [:p "Bug reports and feature suggestions can be submitted "
      [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "here"] "."]

     [:h3 "Frequently Asked Questions"]
     [:ul.list
      [:li
       [:h4 "How can I change my avatar?"]
       [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
        " and create an account with the same email used to register on Jinteki.net."]]
      [:li
       [:h4 "I forgot my password. How can I reset it?"]
       [:p "An automated password reset is in the todo list. Meanwhile, send an email to mtgred@gmail.com with your username."]]
      [:li
       [:h4 "How can I make a donation?"]
       [:p "My PayPal account is mtgred@gmail.com."]
       [:p "If you use Bitcoin, my address is "
        [:span.bitcoin "1ByjuLNZRvG17YaRKgKUjNi3c4qQ3daX3g" [:img.qr {:src "/img/bitcoin.png"}]] "."]
       [:p "Contributions are appreciated and help financing fast servers."]]
      [:li
       [:h4 "There are too many cards or menu options to fit the screen."]
       [:p "Decrease the zoom level of your browser and you should be able to see everything."]]
      [:li
       [:h4 "What is the best supported browser?"]
       [:p "Google Chrome on a desktop or laptop is recommended. Firefox and Safari should work fine too."]]
      [:li
       [:h4 "Is there some documentation on how to use the jinteki.net?"]
       [:p "A "
        [:a {:href "https://mythbuilder.wordpress.com/2015/03/25/the-definitive-guide-to-netiquette-on-jinteki-net-wip/" :target "_blank"} "guide"] " has been written by Mythbuilder." ]]]

     [:h3 "Contributors"]
     [:p "A big props to Neal Terrell (nealpro) and Joel Koepp (JoelCFC25) who actively contribute to the success of the project with regular code submissions, the management of the issue tracker and answers to questions in the chat. It would not be the same without your help."]
     [:p "Many thanks to all the donators. Your contributions and kind words are greatly appreciated. You help financing fast servers and keep the developer motivated."]
     [:ul.list.compact
      (for [d (:donators cursor)]
        [:li d])]
     [:h3 "Disclaimer"]
     [:p "Netrunner and Android are trademarks of Fantasy Flight Publishing, Inc. and/or Wizards of the Coast LLC."]
     [:p "This is website is not affiliated with Fantasy Flight Games or Wizards of the Coast."]])))

(om/root about app-state {:target (. js/document (getElementById "about"))})
