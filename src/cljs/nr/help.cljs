(ns nr.help
  (:require [clojure.string :refer [split]]))

(def help-data
  "List of maps with FAQ about jinteki.net. Every section MUST have an :id here, so the links can work."
  (list
    {:id "general"
     :title "General"
     :sub (list
            {:id "dostuff"
             :title "How do I do I perform actions in a game?"
             :content [:ul
                        [:p "In general, if you want to perform an action connected to a card, try clicking that card. "
                         "Either something will happen or a menu should appear."]
                        [:p "Cards can be moved by clicking them and dragging. Clicking and dragging excessive cards from one's "
                         "hand to discard pile is normally done after one's turn ends and they are over their hand size limit."]
                        [:p "A player's clicks, credits, tags etc. can be manipulated by hand by using plus/minus signs next "
                         "to their numbers in the panel on the left."]]}
            {:id "undo"
             :title "How do I undo an action?"
             :content [:ul
                        [:p "There are two undo functions - undo to turn start, and undo the last click. "
                         "To undo the start of the current turn both players must use the /undo-turn command. "
                         "To undo to the start of the click the active player must use the /undo-click command. "]
                        [:p "There are some non-click based interactions such as using clone-chip and rezzing ICE or assets which are "
                         "not supported via the undo-click function and players will need to handle manually. "
                         " Trashed/played cards can be dragged back to hand and reinstalled if needed. If there"
                         " are lingering/hard to dismiss prompts, try using " [:code "/close-prompt"] " command as a last resort."]]}
            {:id "breakice"
             :title "How do I break ICE and fire ICE subroutines?"
             :content [:ul
                        [:p "Breaking ICE during a run and subroutines firing is currently not automated. Runner signals using their "
                         "icebreakers by clicking them and using their abilities. When some subroutines are left unbroken and fire, "
                         "Corp clicks the piece of ICE with subroutines firing and uses them."]
                        [:p "It's considered common courtesy to wait as Corp for the runner to say \"fire\" before firing the "
                         "subroutines, since Runner may have ways of breaking/avoiding the effects that are not immediately obvious "
                         "and the effects of a fired subroutine may be hard to undo."]]}
            {:id "closemenu"
             :title "How do I close a card's menu?"
             :content [:ul
                        [:p "Click that card again. If it isn't a menu, but a bugged prompt that shouldn't be there, "
                       "try using " [:code "/close-prompt"]]]}
            {:id "commands"
             :title "How do I use commands during a game?"
             :content [:ul
                        [:div "To use a command, type it in chatbox and press Enter. Some of the commands will bring up a prompt "
                         "requiring you to select something. List of available commands:"
                         [:ul
                          [:li [:code "/adv-counter n"] " - set advancement counters on a card to n (player's own cards only). Deprecated in favor of " [:code "/counter ad n"]]
                          [:li [:code "/bp n"] " - Set your bad publicity to n"]
                          [:li [:code "/card-info"] " - display debug info about a card (player's own cards only)"]
                          [:li [:code "/clear-win"] " - requests game to clear the current win state.  Requires both players to request it"]
                          [:li [:code "/click n"] " - Set your clicks to n"]
                          [:li [:code "/close-prompt"] " - close an active prompt and show the next waiting prompt, or the core click actions"]
                          [:li [:code "/counter n"] " - set counters on a card to n (player's own cards only). Attempts to infer the type of counter to place. If the inference fails, you must use the next command to specify the counter type."]
                          [:li [:code "/counter type n"] " - set the specified counter type on a card to n (player's own cards only). Type must be " [:code "agenda"] ", " [:code "advance"] ", " [:code "credit"] ", " [:code "power"] ", or " [:code "virus"] ". Can be abbreviated as " [:code "ag"] ", "  [:code "ad"] ", "  [:code "c"] ", "  [:code "p"] ", or " [:code "v"] " respectively."]
                          [:li [:code "/credit n"] " - Set your credits to n"]
                          [:li [:code "/deck #n"] " - Put card number n from your hand on top of your deck"]
                          [:li [:code "/discard #n"] " - Discard card number n from your hand"]
                          [:li [:code "/discard-random"] " - Discard a random card from your hand"]
                          [:li [:code "/draw n"] " - Draw n cards"]
                          [:li [:code "/end-run"] " - End the run (Corp only)"]
                          [:li [:code "/facedown"] " - Install a card facedown (Runner only)"]
                          [:li [:code "/handsize n"] " - Set your handsize to n"]
                          [:li [:code "/install-ice"] " - Install a piece of ICE at any position in a server (Corp only)"]
                          [:li [:code "/jack-out"] " - Jack out (Runner only)"]
                          [:li [:code "/link n"] " - Set your link to n"]
                          [:li [:code "/memory n"] " - Set your memory to n"]
                          [:li [:code "/move-bottom"] " - Pick a card in your hand to put on the bottom of your deck"]
                          [:li [:code "/move-deck"] " - Pick a card from your play-area to put on top of your deck"]
                          [:li [:code "/move-hand"] " - Pick a card from your play-area to put into your hand"]
                          [:li [:code "/peek n"] " - See n top cards of your deck"]
                          [:li [:code "/psi"] " - Start a Psi game (Corp only)"]
                          [:li [:code "/replace-id n"] " - Replace your ID with the card \"n\""]
                          [:li [:code "/rez"] " - Select a card to rez, ignoring all costs (Corp only)"]
                          [:li [:code "/rez-all"] " - Rez all cards, ignoring all costs and flip cards in archives faceup (Corp only). For revealing your servers at the end of a game."]
                          [:li [:code "/rfg"] " - Select a card to remove from the game"]
                          [:li [:code "/roll n"] " - Roll an n-sided die"]
                          [:li [:code "/summon n"] " - Add card \"n\" to your hand (from outside the game)"]
                          [:li [:code "/swap-ice"] " - Swap the position of two installed ICE (Corp only)"]
                          [:li [:code "/swap-installed"] " - Swap the position of two installed non-ICE (Corp only)"]
                          [:li [:code "/tag n"] " - Set your tags to n"]
                          [:li [:code "/take-brain n"] " - Take n brain damage (Runner only)"]
                          [:li [:code "/take-meat n"] " - Take n meat damage (Runner only)"]
                          [:li [:code "/take-net n"] " - Take n net damage (Runner only)"]
                          [:li [:code "/trace n"] " - Start a trace with base strength n (Corp only)"]
                          [:li [:code "/undo-click"] " - Resets the game back to start of the click.  One click only retained. Only allowed for active player"]
                          [:li [:code "/undo-turn"] " - Resets the game back to end of the last turn. Requires both players to request it"]
                          [:li [:code "/unique"] " - Toggles uniqueness of selected card (can be used to e.g. play with non-errata version of Wireless Net Pavillion)"]]]]}
            {:id "documentation"
             :title "Is there more documentation on how to use Jinteki.net?"
             :content [:ul
                        [:p "Read the "
                         [:a {:href "https://github.com/mtgred/netrunner/wiki/Jinteki.net-Guide" :target "_blank"} "Jinteki.net Guide"]
                         " on the GitHub wiki."]]})}
    {:id "beginners"
     :title "Beginners"
     :sub (list
            {:id "learnrules"
             :title "Where can I find the game's rules explanation?"
             :content [:ul [:p "The first step is reading " [:a {:href "https://www.fantasyflightgames.com/ffg_content/android-netrunner/support/android-netrunner-core-rules.pdf" :target "_blank"} "the official rulebook"]
                             ". If you prefer video form, FFG has prepared " [:a {:href "https://www.youtube.com/watch?v=VAslVfZ9p-Y" :target "_blank"} "a video tutorial"]
                             ", too."]
                            [:p "Once familiar with the basics, the finer points of rules/card interactions can be found in "
                             "the official FAQ on "
                             [:a {:href "https://www.fantasyflightgames.com/en/products/android-netrunner-the-card-game/"} "the FFG page"] ". "
                             "There is also " [:a {:href "http://ancur.wikia.com/wiki/Project_ANCUR_Wiki"} "Project ANCUR"] ", which is a collection "
                             "of rulings (also unofficial) regarding various cards and game situations."]]}
            {:id "firstgame"
             :title "Can I play my first game on jinteki.net even though I'm a total beginner and never played in meatspace?"
             :content [:ul
                        [:p "Sure! Many players will be happy to play/teach a beginner if they know what they're getting into beforehand. "
                         "So just create a new game with name such as \"beginner here\" or \"core set only please\", someone "
                         "happy to play with a beginner should join after a while."]]}
            {:id "finddecks"
             :title "Where can I find some good starting decks?"
             :content [:ul [:p [:a {:href "https://netrunnerdb.com/"} "NetrunnerDB"] " is a good resource for finding decks of all kinds. "
                             "For finding decks consisting of core set only try setting some filters in "
                             [:a {:href "http://netrunnerdb.com/en/decklists/search#allowed_packs"} "the decklist search"] "."]
                            [:p "Once you find a deck you like, export it in Jinteki.net's format (or plain text format if the "
                             "site doesn't offer the former), copy and paste it into the deckbuilder."]]}
            {:id "communities"
             :title "Where can I find other Netrunner players to talk to?"
             :content [:ul
                        [:div "Apart from the chatrooms here on Jinteki.net, here are a few links to online Netrunner communities:"
                         [:ul
                          [:li [:a {:href "http://forum.stimhack.com/"} "Stimhack forums"]]
                          [:li [:a {:href "http://reddit.com/r/netrunner/"} "/r/netrunner subreddit"]]
                          [:li "multiple Facebook groups, such as "
                           [:a {:href "https://www.facebook.com/groups/netrunnergeeks/"} "Netrunner Geeks"]]]]]})}
    {:id "formats"
     :title "Formats"
     :sub (list
            {:id "standard"
             :title "What is the Standard format?"
             :content [:ul
                        [:p "The flagship format of NISEI Organized Play, Standard is "
                         "frequently changing to keep the meta exciting and engaging for "
                         "players of all levels. Most official Organised Play events will "
                         "follow the Standard format. "
                         "Refer to " [:a {:href "https://nisei.net/op/supported-formats"} "NISEI Supported Formats"]]]}
            {:id "eternal"
             :title "What is Eternal format?"
             :content [:ul
                        [:p "Eternal is not affected by rotation and has a much less "
                         "stringent Most Wanted List. The largest and most complex format, "
                         "it encompasses nearly the entirety of the printed card pool and "
                         "only grows larger with time. "
                         "Refer to " [:a {:href "https://nisei.net/op/supported-formats"} "NISEI Supported Formats"]]]}
            {:id "core-experience"
             :title "What is the Core Experience format?"
             :content [:ul
                        [:p "The \"core\" of the game experience, and an excellent "
                         "starting point for new or returning players. A single copy of "
                         "System Core 2019 is the only legal product; there is no MWL. "
                         "Refer to " [:a {:href "https://nisei.net/op/supported-formats"} "NISEI Supported Formats"]]]}
           {:id "snapshot"
             :title "What is the Snapshot format?"
             :content [:ul
                        [:p "This format is a \"snapshot\" of the meta at Magnum Opus; "
                         "the culmination of FFG Organized Play. It will see minimal "
                         "changes unless strictly necessary. "
                         "Refer to " [:a {:href "https://nisei.net/op/supported-formats"} "NISEI Supported Formats"]]]}
           {:id "snapshot-plus"
             :title "What is the Snapshot Plus format?"
             :content [:ul
                        [:p "This is the Snapshot format but with the cards that were "
                         "released at Worlds 2018 (Magnum Opus) included as well. The "
                         "included cards are Labor Rights, Embolus, Slot Machine, Border "
                         "Control, Timely Public Release, Hired Help, and Watch The "
                         "World Burn. "
                         "Refer to " [:a {:href "https://nisei.net/op/supported-formats"} "NISEI Supported Formats"]]]}
           {:id "socr"
             :title "What is the SOCR format?"
             :content [:ul
                        [:p "SOCR stands for Stimhack Online Cache Refresh. It's a limited "
                         "cardpool tournament originally based on FFG's Cache Refresh "
                         "format but with an updated MWL and cardpool. It is currently "
                         "in it's 9th iteration. "
                         "Refer to the " [:a {:href "https://forum.stimhack.com/t/stimhack-online-cache-refresh-9-information-thread/10419"} "Stimhack Thread"]]]}
           {:id "classic"
             :title "What is the Classic format?"
             :content [:ul
                        [:p "An alternate Eternal format created by thebigboy. The entire "
                         "ANR cardpool is legal, except for a Ban-list of (currently) 49 cards. "
                         "Refer to the " [:a {:href "https://runthenet.wordpress.com/2019/01/01/the-classic-format-netrunners-final-form/"} "announcement article"]]]})}
    {:id "site"
     :title "Website"
     :sub (list
            {:id "avatar"
             :title "How do I change my avatar?"
             :content [:ul
                        [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
                         " and create an account with the same email as the one used to register on Jinteki.net. Please note that "
                         "it can sometimes take up to a few hours for the new avatar to be visible on the site."]]}
            {:id "bestbrowser"
             :title "What is the best supported browser?"
             :content [:ul
                        [:p "Google Chrome or Firefox on a desktop or laptop is recommended. Safari should work fine too."]
                        [:p "There is limited support for tablet browsers. If you have too many cards to fit on the screen you might not able to see all of them."]
                        [:p "Using a phone is not recommended. The screen will most likely be too small to fit the gameboard."]]}
            {:id "fullscreen"
             :title "How to use jinteki.net in fullscreen mode on a tablet?"
             :content [:ul
                        [:p "Add jinteki.net to your homescreen as described "
                         [:a {:href "http://www.howtogeek.com/196087/how-to-add-websites-to-the-home-screen-on-any-smartphone-or-tablet/"} "here"]
                         ". If you tap on the homescreen icon, you will be in fullscreen."]]}
            {:id "privatemsgs"
             :title "How do I send a private message / add someone to friendlist?"
             :content [:ul
                        [:p "The community management issues such as private messages or friendlist are currently not implemented. "
                         "They are planned, but no specific date is set, as all of our code is written by volunteers."]]}
            {:id "competitive"
             :title "What is the point of the \"Competitive\" room in lobby? How does it differ from \"Casual\"?"
             :content [:ul [:p "Different rooms in lobby are meant to help people with similar expectations about the game find each other. "
                             "In general, competitive room is for games with players intending to play competitively. "
                             "This may mean something different to each of them... However, since it's a non-default room, "
                             "going there and creating or joining a game usually isn't accidental and is a declaration of some kind of competitive intent."]
                            [:div "Some recommendations for playing in the competitive room:"
                             [:br] [:br]
                             [:ul
                              [:li "a decent knowledge of the game's rules"]
                              [:li "familiarity with the site's interface"]
                              [:li "a " [:span.legal "tournament legal"] " deck"]
                              [:li "enough time reserved for a full game and no distractions"]]]
                            [:p "Games with players not able or willing to follow above recommendations are probably better suited to the Casual room. "
                             "Some examples would be: learning the game, learning the site's interface, testing a completely new and crazy deck idea, "
                             "testing future spoilers, playing on a touchscreen, playing at work and likely to have to quit on short notice, etc. "
                             "All of these circumstances may cause needless frustration of players expecting to play a game in a competitive setting."]]}
            {:id "aboutstats"
             :title "What are the options for tracking Game and Deck Statistics, and what do they mean?"
             :content [:ul [:div "Games Started vs. Completed is always logged and displayed.  We want to discourage people dropping in games. "
                             "You can toggle between the modes listed below if you feel like being a casual player one moment then logging stats the next. "
                             "No data is lost or cleared when you toggle between modes."
                            [:br] [:br]
                             [:ul
                              [:li "Always - statistics are kept and displayed for all games you play"]
                              [:li "Competitive lobby only - statistics are kept and displayed only for competitive games"]
                              [:li "None - statistics are neither logged or displayed"]]]
                            [:div "What do the game statistics mean?"
                             [:br] [:br]
                             [:ul
                              [:li "Games Started - games you have entered."]
                              [:li "Games Completed - games that had a winner, or games that did not complete but opponent dropped first."]
                              [:li "Games Incomplete -  games with no winner where you dropped first, and did not concede."]
                              [:li "Games Won - games won.  The percentage is compared to those games lost."]
                              [:li "Games Lost - games lost.  The percentage is compared to those games won."]]]
                            [:p "Your game completion rate is visible in the player lobby so people can determine if they should play against you."
                             " Don't quit during games - please concede if you have to leave."]]})}
    {:id "cards"
     :title "Cards and Specific Interactions"
     :sub (list
            {:id "shards"
             :title "How do I install Eden/Hades/Utopia Shard during a run?"
             :content [:ul
                        [:p "At the last run step on the relevant server, instead of pressing \"Successful Run\" button, "
                         "click the shard card you want to install in hand. You should end the run with the shard installed "
                         "at no cost."]]}
            {:id "nasir"
             :title "How do I use Nasir's ability?"
             :content [:ul
                        [:p "Nasir's ability is currently triggered manually - when encountering a piece of ICE, click Nasir's "
                       "identity card to trigger the ability."]]}
            {:id "adam"
             :title "How do I install Adam's directives?"
             :content [:ul
                        [:p "Adam's directives are installed automatically at the game start. The directives are pulled "
                         "directly from the game-server so do not need to be a part of your deck. The previous workaround "
                         "of explicitly adding the 3 directives to the deck is no longer necessary."]]}
            {:id "napdmwl"
             :title "What is MWL and \"Tournament legal\"? Why is my deck marked as \"Casual play only\"?"
             :content [:ul
                        [:p "New Angeles Police Department Most Wanted List, also known as NAPD MWL or just MWL, is a list "
                         "of cards with additional deck building restrictions for tournament play. "
                         "There are two categories of MWL cards: \"restricted\" and \"removed\". "
                         "You may only include up to one card (up to its maximum number of copies) from the restricted category. "
                         "You may not include cards from the removed category. "
                         "For more information about the MWL read Tournament Rules from "
                         [:a {:href "https://www.fantasyflightgames.com/en/products/android-netrunner-the-card-game/"} "the official FFG page"] "."]
                        [:p "Decks that are valid and fit within tournament restrictions are marked " [:span.legal "Tournament legal" ] ". "
                         "Decks that fit within the printed influence limit, but not within the tournament restrictions, "
                         "are marked " [:span.casual "Casual play only"] ". Decks that do not fit basic deckbuilding rules are marked " [:span.invalid "Invalid"] "."]
                        [:p "Putting cards in your deck that are not yet available for sale (i.e. future spoilers) or ones that are "
                         "out of competitive rotation will also result in your deck being marked as " [:span.casual "Casual play only"] ". Such cards "
                         "should be easy to identify - they are " [:span.casual "highlighted"] " in the deckbuilder."]]}
            {:id "altarts"
             :title "How do I change my decks to use alternative art versions of cards (or promotional ones)?"
             :content [:ul
                        [:p "Alternative art cards are enabled for the " [:a {:href "#donations"} "donators"] " and "
                         [:a {:href "#devs"} "developers"] " of the site. If you belong to one of the aforementioned groups and you feel like you should have them enabled, "
                         "but you don't, " [:a {:href "/about"} "contact us"] "."]]})}
    {:id "troubleshooting"
     :title "Troubleshooting"
     :sub (list
            {:id "weird"
             :title "The site is behaving weird."
             :content [:ul
                        [:p "The server code may have been freshly updated and you don't have the latest Javascript code. "
                         "First step in every troubleshooting should be a forced refresh of your browser by doing a "
                         [:a {:href "http://refreshyourcache.com/en/cache/"} "force refresh"] " (" [:code "Ctrl + F5"] " on Windows). "
                         "Also read the announcements on the main page, something about server problems may be written there."]]}
            {:id "touchproblems"
             :title "The website doesn't work well on my touchscreen device."
             :content [:ul
                        [:p "Touchscreen devices are currently not supported. See answer to " [:a {:href "#bestbrowser"} "this question"]
                         " for best browsers to use with Jinteki.net."]]}
            {:id "toomanyservers"
             :title "There are too many servers to fit on my screen."
             :content [:ul
                        [:p "Decrease the zoom level of your browser and you should be able to see everything. If you are using "
                         "Chrome, you can do it by pressing CTRL and - (minus). If you are using Firefox, you may need to install "
                         [:a {:href "https://addons.mozilla.org/pl/firefox/addon/zoom-page/"} "Zoom Page addon"] " before the zoom works correctly."]]}
            {:id "zerogames"
             :title "Whenever I connect to the site, I see there are 0 games in the lobby."
             :content [:ul
                        [:p "This is most likely a websocket issue. Check if your network filters let through traffic from ws.jinteki.net. "
                         "Whitelisting *.jinteki.net should solve the problem."]]})}
    {:id "getinvolved"
     :title "Getting Involved"
     :sub (list
            {:id "reportingbugs"
             :title "How can I report a bug?"
             :content [:ul
                        [:p "The best place to report bugs is the " [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "GitHub issue tracker"]
                         ". Before reporting, it is best to make a quick search to see if it's already been reported. "
                         "If the bug concerns a card, look it up in "
                         [:a {:href "https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml" :target "_blank"} "Card implementation status"]
                         " - the card in question may be unimplemented yet."]]}
            {:id "features"
             :title "How can I suggest a feature?"
             :content [:ul
                        [:p "Same as bugs - feature requests should go on the " [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "GitHub issue tracker"]
                       ". Again, it's best to make a quick search first to avoid duplicating existing issues."]]}
            {:id "donations"
             :title "How can I make a donation?"
             :content [:ul
                        [:p "Donation info can be found on the " [:a {:href "/about"} "About"] " page."]]}
            {:id "devs"
             :title "How can I help with the coding/webdesign?"
             :content [:ul [:p "Visit the project page on " [:a {:href "https://github.com/mtgred/netrunner/" :target "_blank"} "GitHub"] " and fork "
                             "the repository. Implement the changes you were planning on doing and create a PR (Pull Request). If you are in "
                             "need of some ideas, check out " [:a {:href "https://github.com/mtgred/netrunner/labels/easy" :target "_blank"} "issues marked 'easy' on GitHub"] "."]
                            [:p "After two of your PRs have been merged into the master branch, send an e-mail to "
                             [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"] " stating who you are on GitHub and ask for access "
                             "to Jinteki.net Slack, so you can get in better contact with the dev team."]]}
            {:id "awesome"
             :title "Why is this site so awesome?"
             :content [:ul
                        [:p "Because We Built It."]]}
            )}))

(def help-toc
  "Generates list serving as help's table of contents. Parses help-data."
  [:nav {:role "navigation" :class "table-of-contents" :key "nav"}
   [:ul (doall
          (for [{:keys [id title sub] :as section} help-data]
            ^{:key id}
            [:li [:a (when id {:href (str "#" id)}) title]
             [:ul (doall
                    (for [{:keys [id title] :as question} sub]
                      ^{:key id}
                      [:li [:a (when id {:href (str "#" id)}) title]]))]]))]])

(def help-contents
  "Takes help-data and translates it to HTML tags."
  (doall
    (for [{:keys [id title sub] :as section} help-data]
      (list [:h2 {:id id :key id} title]
            (doall
              (for [{:keys [id title content] :as question} sub]
                ^{:key title}
                [:div [:h3 {:id id :key title} title]
                 content]))))))

(defn help []
  [:div.help.panel.content-page.blue-shade
   [:h2 "Help Topics"]
   help-toc
   help-contents])
