(ns netrunner.help
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [clojure.string :refer [split]]))

(def app-state (atom {}))

(def help-contents
  "Sablono tags with FAQ about jinteki.net. Every header tag MUST have an ID here, defined with # in the keyword."
  (list [:h3#general "General"]

        [:h4#dosomething "How do I do X (in game)?"] ;; TODO

        [:h4#undo "How do I undo an action?"] ;; TODO

        [:h4#closemenu "How do I close a card's menu?"] ;; TODO

        [:h4#commands "What are the commands available during a game?"] ;; TODO

        [:h4#documentation "Is there more documentation on how to use jinteki.net?"]
        [:p "Read the "
         [:a {:href "https://github.com/mtgred/netrunner/wiki/Jinteki.net-Guide" :target "_blank"}
          "Jinteki.net Guide"] " on the GitHub wiki."]

        [:h3#beginners "Beginners"]

        [:h4#learnrules "Where can I find the game's rules explanation?"]
        [:p "First step is reading " [:a {:href "https://www.fantasyflightgames.com/ffg_content/android-netrunner/support/android-netrunner-core-rules.pdf" :target "_blank"} "the official rulebook"]
         ". If you prefer video form, FFG has prepared " [:a {:href "https://www.youtube.com/watch?v=VAslVfZ9p-Y" :target "_blank"} "a video tutorial"]
         ", too."]
        [:p "Once familiar with the basics, the finer points of rules/card interactions can be found in "
         [:a {:href "https://www.fantasyflightgames.com/ffg_content/android-netrunner/support/FAQ/Android-Netrunner%20FAQ.pdf"} "the official FFG FAQ"] "."]

        [:h4#firstgame "Can I play my first game on jinteki.net even though I'm a total beginner and never played in meatspace?"] ;; TODO

        [:h4#finddecks "Where can I find some good starting decks?"] ;; TODO

        [:h4#communities "Where can I find other Netrunner players to talk to?"] ;; TODO

        [:h3#site "Website"]

        [:h4#avatar "How do I change my avatar?"]
        [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
         " and create an account with the same email as the one used to register on Jinteki.net."]

        [:h4#bestbrowser "What is the best supported browser?"]
        [:p "Google Chrome or Firefox on a desktop or laptop is recommended. Safari should work fine too. "
         "Touchscreen devices (smartphones, tablets etc.) are currently not supported."]

        [:h4#privatemsg "How do I send a private message / add someone to friendlist?"] ;; TODO

        [:h3#cards "Cards and Rules"]

        [:h4#shards "How do I install Eden/Hades/Utopia Shard during a run?"] ;; TODO

        [:h4#nasir "How do I use Nasir's ability?"] ;; TODO

        [:h4#adam "How do I install Adam's directives?"] ;; TODO

        [:h4#mwl "What is MWL? Why is my deck marked as \"Casual play only\"?"] ;; TODO

        [:h4#rezaccess "How do I rez cards as Corp in the 4.3 run timing window?"] ;;TODO

        [:h3#troubleshooting "Troubleshooting"]

        [:h4#touchproblems "The website doesn't work on my touchscreen device."]
        [:p "Touchscreen devices are currently not supported. See answer to " [:a {:href "#bestbrowser"} "this question"]
         " for best browsers to use with Jinteki.net."]

        [:h4#toomanycards "There are too many cards or menu options to fit the screen."]
        [:p "Decrease the zoom level of your browser and you should be able to see everything. If you are using "
         "Chrome, you can do it by pressing CTRL and - (minus). If you are using Firefox, you may need to install "
         [:a {:href "https://addons.mozilla.org/pl/firefox/addon/zoom-page/"} "Zoom Page addon"] " before the zoom works correctly."]

        [:h4#zerogames "Whenever I connect to the site, I see there are 0 games in the lobby."]
        [:p "This is most likely a websocket issue. Check if your network filters let through traffic from ws.jinteki.net. "
         "Whitelisting *.jinteki.net should solve the problem."]

        [:h3#getinv "Getting Involved"]

        [:h4#bugs "How can I report a bug?"]
        [:p "The best place to report bugs is the " [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "GitHub issue tracker"]
         ". Before reporting, it is best to make a quick search to see if it's already been reported. "
         "If the bug concerns a card, look it up in "
         [:a {:href "https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml" :target "_blank"} "Card implementation status"]
         " - the card in question may be unimplemented yet."]

        [:h4#features "How can I suggest a feature?"]
        [:p "Same as bugs - feature requests should go on the " [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "GitHub issue tracker"]
         ". Again, it's best to make a quick search first to avoid duplicating existing issues."]

        [:h4#donations "How can I make a donation?"]
        [:p "Donation info can be found on the " [:a {:href "/about"} "About"] " page."]

        [:h4#devs "How can I help with the coding/webdesign?"]
        [:p "Visit the project page on " [:a {:href "https://github.com/mtgred/netrunner/" :target "_blank"} "GitHub"] " and fork "
         "the repository. Implement the changes you were planning on doing and create a PR (Pull Request). If you are in "
         "need of some ideas, check out " [:a {:href "https://github.com/mtgred/netrunner/labels/easy" :target "_blank"} "issues marked 'easy' on GitHub"] "."]
        [:p "After two of your PRs have been merged into the master branch, send an e-mail to "
         [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"] " stating who you are on GitHub and ask for access "
         "to Jinteki.net Slack, so you can get in better contact with the dev team."]

        [:h4#awesome "Why is this site so awesome?"]
        [:p "Because We Built It."]))

(def help-toc
  "Generates list serving as help's table of contents. Parses help-contents and looks for :h3 and :h4 tags."
  [:nav {:role "navigation" :class "table-of-contents"}
   [:ul
    (for [tag help-contents
          :let [tagkey (name (first tag))
                tagcontents (second tag)
                tagname (first (split tagkey #"#"))
                tagid (second (split tagkey #"#"))]
          :when (= "h4" tagname)]
      [:li [:a (when tagid {:href (str "#" tagid)}) tagcontents]])]])

(defn help [cursor owner]
  (om/component
    (sab/html
      [:div.help.panel.blue-shade
       [:h3 "Table of contents"]
       help-toc
       help-contents])))

(om/root help app-state {:target (. js/document (getElementById "help"))})
