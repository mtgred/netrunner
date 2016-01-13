(ns netrunner.help
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [clojure.string :refer [split]]))

(def app-state (atom {}))

(def help-data
  "List of maps with FAQ about jinteki.net. Every section MUST have an :id here, so the links can work."
  (list
    {:id "general"
     :title "General"
     :sub (list
            {:id "dostuff"
             :title "How do I do stuff in game?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "undo"
             :title "How do I undo an action?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "closemenu"
             :title "How do I close a card's menu?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "commands"
             :title "What are the commands available during a game?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "documentation"
             :title "Is there more documentation on how to use Jinteki.net?"
             :content [:p "Read the "
                       [:a {:href "https://github.com/mtgred/netrunner/wiki/Jinteki.net-Guide" :target "_blank"}
                        "Jinteki.net Guide"] " on the GitHub wiki."]}
            )}
    {:id "beginners"
     :title "Beginners"
     :sub (list
            {:id "learnrules"
             :title "Where can I find the game's rules explanation?"
             :content (list [:p "The first step is reading " [:a {:href "https://www.fantasyflightgames.com/ffg_content/android-netrunner/support/android-netrunner-core-rules.pdf" :target "_blank"} "the official rulebook"]
                             ". If you prefer video form, FFG has prepared " [:a {:href "https://www.youtube.com/watch?v=VAslVfZ9p-Y" :target "_blank"} "a video tutorial"]
                             ", too."]
                            [:p "Once familiar with the basics, the finer points of rules/card interactions can be found in "
                             [:a {:href "https://www.fantasyflightgames.com/ffg_content/android-netrunner/support/FAQ/Android-Netrunner%20FAQ.pdf"} "the official FFG FAQ"] "."])}
            {:id "firstgame"
             :title "Can I play my first game on jinteki.net even though I'm a total beginner and never played in meatspace?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "finddecks"
             :title "Where can I find some good starting decks?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "communities"
             :title "Where can I find other Netrunner players to talk to?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            )}
    {:id "site"
     :title "Website"
     :sub (list
            {:id "avatar"
             :title "How do I change my avatar?"
             :content [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
                       " and create an account with the same email as the one used to register on Jinteki.net."]}
            {:id "bestbrowser"
             :title "What is the best supported browser?"
             :content [:p "Google Chrome or Firefox on a desktop or laptop is recommended. Safari should work fine too. "
                       "Touchscreen devices (smartphones, tablets etc.) are currently not supported."]}
            {:id "privatemsg"
             :title "How do I send a private message / add someone to friendlist?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            )}
    {:id "cards"
     :title "Cards and Specific Interactions"
     :sub (list
            {:id "shards"
             :title "How do I install Eden/Hades/Utopia Shard during a run?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "nasir"
             :title "How do I use Nasir's ability?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "adam"
             :title "How do I install Adam's directives?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "napdmwl"
             :title "What is MWL? Why is my deck marked as \"Casual play only\"?"
             :content [:p "Lorem Ipsum"]} ;; TODO
            {:id "rezaccess"
             :title "How do I rez cards as Corp in the 4.3 run timing window?"
             :content [:p "Lorem Ipsum"]} ;; TODO
             )}
    {:id "troubleshooting"
     :title "Troubleshooting"
     :sub (list
            {:id "touchproblems"
             :title "The website doesn't work well on my touchscreen device."
             :content [:p "Touchscreen devices are currently not supported. See answer to " [:a {:href "#bestbrowser"} "this question"]
                       " for best browsers to use with Jinteki.net."]}
            {:id "toomanyservers"
             :title "There are too many servers to fit on my screen."
             :content [:p "Decrease the zoom level of your browser and you should be able to see everything. If you are using "
                       "Chrome, you can do it by pressing CTRL and - (minus). If you are using Firefox, you may need to install "
                       [:a {:href "https://addons.mozilla.org/pl/firefox/addon/zoom-page/"} "Zoom Page addon"] " before the zoom works correctly."]}
            {:id "zerogames"
             :title "Whenever I connect to the site, I see there are 0 games in the lobby."
             :content [:p "This is most likely a websocket issue. Check if your network filters let through traffic from ws.jinteki.net. "
                       "Whitelisting *.jinteki.net should solve the problem."]}
            )}
    {:id "getinvolved"
     :title "Getting Involved"
     :sub (list
            {:id "reportingbugs"
             :title "How can I report a bug?"
             :content [:p "The best place to report bugs is the " [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "GitHub issue tracker"]
                       ". Before reporting, it is best to make a quick search to see if it's already been reported. "
                       "If the bug concerns a card, look it up in "
                       [:a {:href "https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml" :target "_blank"} "Card implementation status"]
                       " - the card in question may be unimplemented yet."]}
            {:id "features"
             :title "How can I suggest a feature?"
             :content [:p "Same as bugs - feature requests should go on the " [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "GitHub issue tracker"]
                       ". Again, it's best to make a quick search first to avoid duplicating existing issues."]}
            {:id "donations"
             :title "How can I make a donation?"
             :content [:p "Donation info can be found on the " [:a {:href "/about"} "About"] " page."]}
            {:id "devs"
             :title "How can I help with the coding/webdesign?"
             :content (list [:p "Visit the project page on " [:a {:href "https://github.com/mtgred/netrunner/" :target "_blank"} "GitHub"] " and fork "
                             "the repository. Implement the changes you were planning on doing and create a PR (Pull Request). If you are in "
                             "need of some ideas, check out " [:a {:href "https://github.com/mtgred/netrunner/labels/easy" :target "_blank"} "issues marked 'easy' on GitHub"] "."]
                            [:p "After two of your PRs have been merged into the master branch, send an e-mail to "
                             [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"] " stating who you are on GitHub and ask for access "
                             "to Jinteki.net Slack, so you can get in better contact with the dev team."])}
            {:id "awesome"
             :title "Why is this site so awesome?"
             :content [:p "Because We Built It."]}
            )}))

(def help-toc
  "Generates list serving as help's table of contents. Parses help-data."
  [:nav {:role "navigation" :class "table-of-contents"}
    [:ul (for [{:keys [id title sub] :as section} help-data]
      [:li [:a (when id {:href (str "#" id)}) title]
       [:ul (for [{:keys [id title] :as question} sub]
              [:li [:a (when id {:href (str "#" id)}) title]])]])]])

(def help-contents
  "Takes help-data and translates it to HTML tags."
  (for [{:keys [id title sub] :as section} help-data]
    (list [:h3 {:id id} title]
          (for [{:keys [id title content] :as question} sub]
            (list [:h4 {:id id} title]
                  content)))))

(defn help [cursor owner]
  (om/component
    (sab/html
      [:div.help.panel.blue-shade
       [:h3 "Table of contents"]
       help-toc
       help-contents])))

(om/root help app-state {:target (. js/document (getElementById "help"))})
