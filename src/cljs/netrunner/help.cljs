(ns netrunner.help
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]))

(def app-state (atom {}))

(defn help [cursor owner]
  (om/component
    (sab/html
      [:div.help.panel.blue-shade
       [:h3 "Frequently Asked Questions"]
       [:ul.list
        [:li
         [:h4 "How can I change my avatar?"]
         [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
          " and create an account with the same email as the one used to register on Jinteki.net."]]
        [:li
         [:h4 "How can I make a donation?"]
         [:p "Donation info can be found on the " [:a {:href "/about"} "About"] " page."]]
        [:li
         [:h4 "There are too many cards or menu options to fit the screen."]
         [:p "Decrease the zoom level of your browser and you should be able to see everything."]]
        [:li
         [:h4 "What is the best supported browser?"]
         [:p "Google Chrome on a desktop or laptop is recommended. Firefox and Safari should work fine too."]]
        [:li
         [:h4 "Is there documentation on how to use jinteki.net?"]
         [:p "Read the "
          [:a {:href "https://github.com/mtgred/netrunner/wiki/Jinteki.net-Guide" :target "_blank"} "Jinteki.net Guide"] " on the Github wiki." ]]]])))

(om/root help app-state {:target (. js/document (getElementById "help"))})
