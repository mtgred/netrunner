(ns nr.help)

(def command-info
  [{:name "/adv-counter"
    :has-args :required
    :usage "/adv-counter n"
    :help "set advancement counters on a card to n (player's own cards only). Deprecated in favor of /counter ad n"}
   {:name "/bp"
    :has-args :required
    :usage "/bp n"
    :help "Set your bad publicity to n"}
   {:name "/bug"
    :usage "/bug"
    :help "Report a bug on GitHub"}
   {:name "/card-info"
    :usage "/card-info"
    :help "display debug info about a card (player's own cards only)"}
   {:name "/charge"
    :usage "/charge"
    :help "Charge an installed card"}
   {:name "/clear-win"
    :usage "/clear-win"
    :help "requests game to clear the current win state.  Requires both players to request it"}
   {:name "/click"
    :has-args :required
    :usage "/click n"
    :help "Set your clicks to n"}
   {:name "/close-prompt"
    :usage "/close-prompt"
    :help "close an active prompt and show the next waiting prompt, or the core click actions"}
   {:name "/counter"
    :has-args :required
    :usage "/counter n"
    :help "set counters on a card to n (player's own cards only). Attempts to infer the type of counter to place. If the inference fails, you must use the next command to specify the counter type."}
   {:name "/counter"
    :has-args :required
    :usage "/counter type n"
    :help "set the specified counter type on a card to n (player's own cards only). Type must be agenda, advance, credit, power, or virus. Can be abbreviated as ag, ad, c, p, or v respectively."}
   {:name "/credit"
    :has-args :required
    :usage "/credit n"
    :help "Set your credits to n"}
   {:name "/deck"
    :has-args :required
    :usage "/deck #n"
    :help "Put card number n from your hand on top of your deck"}
   {:name "/derez"
    :usage "/derez"
    :help "derez a rezzed card (corp only)"}
   {:name "/disable-card"
    :usage "/disable-card"
    :help "Disable a card"}
   {:name "/discard"
    :has-args :required
    :usage "/discard #n"
    :help "Discard card number n from your hand"}
   {:name "/discard-random"
    :usage "/discard-random"
    :help "Discard a random card from your hand"}
   {:name "/draw"
    :has-args :optional
    :usage "/draw n"
    :help "Draw n cards"}
   {:name "/enable-card"
    :usage "/enable-card"
    :help "Enable a card"}
   {:name "/end-run"
    :usage "/end-run"
    :help "End the run (Corp only)"}
   {:name "/facedown"
    :usage "/facedown"
    :help "Install a card facedown (Runner only)"}
   {:name "/handsize"
    :has-args :required
    :usage "/handsize n"
    :help "Set your handsize to n"}
   {:name "/host"
    :usage "/host"
    :help "Manually host a card on another card"}
   {:name "/install-ice"
    :usage "/install-ice"
    :help "Install a piece of ice at any position in a server (Corp only)"}
   {:name "/jack-out"
    :usage "/jack-out"
    :help "Jack out (Runner only)"}
   {:name "/link"
    :has-args :required
    :usage "/link n"
    :help "Set your link to n"}
   {:name "/mark"
    :usage "/mark"
    :help "Identify your mark"}
   {:name "/memory"
    :has-args :required
    :usage "/memory n"
    :help "Set your memory to n"}
   {:name "/move-bottom"
    :usage "/move-bottom"
    :help "Pick a card in your hand to put on the bottom of your deck"}
   {:name "/move-deck"
    :usage "/move-deck"
    :help "Pick a card from your play-area to put on top of your deck"}
   {:name "/move-hand"
    :usage "/move-hand"
    :help "Pick a card from your play-area to put into your hand"}
   {:name "/peek"
    :has-args :optional
    :usage "/peek n"
    :help "See n top cards of your deck"}
   {:name "/psi"
    :usage "/psi"
    :help "Start a Psi game (Corp only)"}
   {:name "/reload-id"
    :usage "/reload-id"
    :help "Reloads your ID (this can sometimes fix gamestates)"}
   {:name "/replace-id"
    :has-args :required
    :usage "/replace-id n"
    :help "Replace your ID with the card \"n\""}
   {:name "/rez"
    :usage "/rez"
    :help "Choose a card to rez, ignoring all costs (Corp only)"}
   {:name "/rez-all"
    :usage "/rez-all"
    :help "Rez all cards, ignoring all costs and flip cards in archives faceup (Corp only). For revealing your servers at the end of a game."}
   {:name "/rez-free"
    :usage "/rez-free"
    :help "Choose a card to rez, ignoring all costs and on-rez abilities (Corp only)"}
   {:name "/rfg"
    :usage "/rfg"
    :help "Choose a card to remove from the game"}
   {:name "/roll"
    :has-args :required
    :usage "/roll n"
    :help "Roll an n-sided die"}
   {:name "/sabotage"
    :has-args :required
    :usage "/sabotage n"
    :help "Sabotage n cards"}
   {:name "/save-replay"
    :usage "/save-replay"
    :help "Save a replay of the game"}
   {:name "/set-mark"
    :has-args :required
    :usage "/set-mark n"
    :help "Set the central server n as your mark (Runner only)"}
   {:name "/show-hand"
    :usage "/show-hand"
    :help "Shows your hand in the chat log (does not proc reveal triggers)"}
   {:name "/summon"
    :has-args :required
    :usage "/summon n"
    :help "Add card \"n\" to your hand (from outside the game)"}
   {:name "/swap-ice"
    :usage "/swap-ice"
    :help "Swap the position of 2 installed pieces of ice (Corp only)"}
   {:name "/swap-installed"
    :usage "/swap-installed"
    :help "Swap the position of two installed non-ice (Corp only)"}
   {:name "/tag"
    :has-args :required
    :usage "/tag n"
    :help "Set your tags to n"}
   {:name "/take-core"
    :has-args :required
    :usage "/take-core n"
    :help "Take n core damage (Runner only)"}
   {:name "/take-meat"
    :has-args :required
    :usage "/take-meat n"
    :help "Take n meat damage (Runner only)"}
   {:name "/take-net"
    :has-args :required
    :usage "/take-net n"
    :help "Take n net damage (Runner only)"}
   {:name "/trace"
    :has-args :required
    :usage "/trace n"
    :help "Start a trace with base strength n (Corp only)"}
   {:name "/trash"
    :usage "/trash"
    :help "Trash an installed card"}
   {:name "/undo-click"
    :usage "/undo-click"
    :help "Resets the game back to start of the click.  One click only retained. Only allowed for active player"}
   {:name "/undo-turn"
    :usage "/undo-turn"
    :help "Resets the game back to end of the last turn. Requires both players to request it"}
   {:name "/unique"
    :usage "/unique"
    :help "Toggles uniqueness of selected card (can be used to e.g. play with non-errata version of Wireless Net Pavillion)"}])

(def keyboard-control-info
  [{:name "Space"
    :usage "Space"
    :help "Performs a default action if there are no controls focused. Otherwise, activates the focused control. Default actions: Clicking for credits, Starting/Ending turns, and continuing a run"}
   {:name "Enter"
    :usage "Enter"
    :help "Focuses the chat if there are no controls focused. Otherwise, activates the focused control"}
   {:name "/"
    :usage "/ (forward slash)"
    :help "Focuses the chat and brings up the command menu"}
   {:name "numbers"
    :usage "Number keys"
    :help "Activates options in the button panel or card menu. Numbers are mapped to options from top to bottom"}])

(def help-data
  "List of maps with FAQ about jinteki.net. Every section MUST have an :id here, so the links can work."
  (list
    {:id "general"
     :title "General"
     :sub (list
            {:id "dostuff"
             :title "How do I perform actions in a game?"
             :content [:ul
                       [:p "In general, if you want to perform an action connected to a card, try clicking that card. "
                        "Either something will happen or a menu should appear. Your mouse cursor may also turn into a \"target\" icon if you need to choose a target. "
                        "You will be prompted discard down to your hand size after you choose \"End Turn\"."]
                       [:p "Most cards in the game are now automated, but be aware that some cards' restrictions or trigger conditions are not implemented. "
                        "If you want to spend credits from a card, but the game is not giving you the option, just click the card with credits and take some."]]}
            {:id "manual"
             :title "What if the card I'm playing is not implemented?"
             :content [:ul
                       [:p "Once in a while you may need to do something manually. A player's clicks, credits, tags etc. can be manipulated by hand by using plus/minus signs next "
                        "to their numbers in the panel on the left (which will appear when you move your mouse there)."]
                       [:p "Cards can be moved by clicking them and dragging, but this does not work when moving a card into the play area (including from one server to another server). "
                        "One workaround is to manually add a click and any credits needed, then click on the card to install it. This works even if it's not your turn."]]}
            {:id "undo"
             :title "How do I undo an action?"
             :content [:ul
                       [:p "There are two undo functions - undo to turn start, and undo the last click. "
                        "To undo the start of the current turn both players must use the /undo-turn command. "
                        "To undo to the start of the click the active player must use the /undo-click command. "]
                       [:p "There are some non-click based interactions such as using clone-chip and rezzing ice or assets which are "
                        "not supported via the undo-click function and players will need to handle manually. "
                        " Trashed/played cards can be dragged back to hand and reinstalled if needed. If there"
                        " are lingering/hard to dismiss prompts, try using " [:code "/close-prompt"] " command as a last resort."]]}
            {:id "breakice"
             :title "How do I break ice and fire ice subroutines?"
             :content [:ul
                       [:p "Once the Runner encounters a piece of ice, both the Runner and the Corp will see a menu. "
                        "To break subroutines, the Runner should click on their icebreakers and use their abilities. "
                        "If some subroutines are left unbroken, after the Runner chooses \"Let all subroutines fire\", "
                        "the Corp clicks \"Fire unbroken subroutines\" to fire them."]
                       [:p "It's considered common courtesy to wait as Corp for the Runner to indicate to fire unbroken subroutines, "
                        "since the Runner may have ways of breaking/avoiding the effects that are not immediately obvious "
                        "and the effects of a fired subroutine may be hard to undo."]]}
            {:id "closemenu"
             :title "How do I close a card's menu?"
             :content [:ul
                       [:p "Click outside the menu or press Escape. If it isn't a menu, but a bugged prompt that shouldn't be there, "
                        "try using " [:code "/close-prompt"] "."]]}
            {:id "keyboard"
             :title "Are there any keyboard controls?"
             :content [:ul
                       [:div "The keyboard can control some basic functionality. "
                        "List of available keyboard controls:"
                        [:ul (doall (map-indexed (fn [idx {:keys [usage help]}] [:li {:key idx} [:code usage] " - " help]) keyboard-control-info))]]]}
            {:id "commands"
             :title "How do I use commands during a game?"
             :content [:ul
                       [:div "To use a command, type it in chatbox and press Enter. Some of the commands will bring up a prompt "
                        "requiring you to select something. List of available commands:"
                        [:ul (doall (map-indexed (fn [idx {:keys [usage help]}] [:li {:key idx} [:code usage] " - " help]) command-info))]]]}
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
             :content [:ul [:p "The first step is " [:a {:href "https://nullsignal.games/players/learn-to-play/" :target "_blank"} "the Learn to Play page"]
                             ". If you prefer video form, Null Signal Games has prepared " [:a {:href "https://youtube.com/watch?v=aG0eTf7BncU" :target "_blank"} "a video tutorial"]
                             ", too."]
                           [:p "Once familiar with the basics, the finer points of rules/card interactions can be found in "
                             [:a {:href "https://nullsignal.games/about/frequently-asked-questions/"} "the FAQ page"] ". "
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
                          [:li [:a {:href "https://stimslackinvite.herokuapp.com/"} "Stimslack"] " (herokuapp invite link)"]
                          [:li [:a {:href "https://discord.gg/VxgbNj5"} "Green Level Clearance Discord server"]]
                          [:li [:a {:href "http://reddit.com/r/netrunner/"} "/r/netrunner subreddit"]]
                          [:li [:a {:href "https://www.facebook.com/groups/netrunnerdorks/"} "Netrunner Dorks Facebook group"]]
                          [:li [:a {:href "https://www.nearearthhub.net/#h.c28pw9eqowgt"} "NearEarthHub#Community Resources"]]]]]})}
    {:id "formats"
     :title "Formats"
     :sub (list
            {:id "standard"
             :title "What is the Standard format?"
             :content [:ul
                        [:p "The flagship format of Null Signal Games' Organized Play, Standard is "
                         "frequently changing to keep the meta exciting and engaging for "
                         "players of all levels. Most official Organised Play events will "
                         "follow the Standard format. "
                         "Refer to " [:a {:href "https://nullsignal.games/players/supported-formats/"} "Supported Formats"] "."]]}
            {:id "startup"
             :title "What is the Startup format?"
             :content [:ul
                        [:p "Startup is a limited-cardpool format, intended for new players "
                         "taking their first steps into Organized Play as well as experienced "
                         "players who want a slimmed-down deckbuilding challenge. "
                         "Refer to " [:a {:href "https://nullsignal.games/players/supported-formats/"} "Supported Formats"] "."]]}
            {:id "sunset"
             :title "What is the Sunset format?"
             :content [:ul
                        [:p "Sunset is essentially Standard with an extended, experimental banlist added to it."
                         "Refer to the " [:a {:href "https://nullsignal.games/blog/introducing-sunset/"} "Sunset Announcement"] "."]]}
            {:id "system-gateway"
             :title "What is the System Gateway format?"
             :content [:ul
                        [:p "System Gateway is Null Signal Games' foundational set. It is designed as an "
                         "out-of-the-box learning experience and provides everything you need "
                         "to start playing Netrunner. "
                         "Refer to " [:a {:href "https://nullsignal.games/products/system-gateway/"} "System Gateway"] "."]]}
            {:id "eternal"
             :title "What is the Eternal format?"
             :content [:ul
                        [:p "Eternal is not affected by rotation and has a much less "
                         "stringent Most Wanted List. The largest and most complex format, "
                         "it encompasses nearly the entirety of the printed card pool and "
                         "only grows larger with time. "
                         "Refer to " [:a {:href "https://nullsignal.games/players/supported-formats/"} "Supported Formats"] "."]]}
           {:id "snapshot"
             :title "What is the Snapshot format?"
             :content [:ul
                        [:p "This format is a \"snapshot\" of the meta at Magnum Opus; "
                         "the culmination of FFG Organized Play. It will see minimal "
                         "changes unless strictly necessary. "
                         "Refer to " [:a {:href "https://nullsignal.games/players/supported-formats/"} "Supported Formats"] "."]]}
           {:id "snapshot-plus"
             :title "What is the Snapshot Plus format?"
             :content [:ul
                        [:p "This is the Snapshot format but with the cards that were "
                         "released at Worlds 2018 (Magnum Opus) included as well. The "
                         "included cards are Labor Rights, Embolus, Slot Machine, Border "
                         "Control, Timely Public Release, Hired Help, and Watch The "
                         "World Burn. "
                         "Refer to " [:a {:href "https://nullsignal.games/players/supported-formats/"} "Supported Formats"] "."]]}
           {:id "neo"
             :title "What is the Neo format?"
             :content [:ul
                        [:p "The Neo format is all the cards released by NSG with a small "
                         "ban list. Refer to the #neo channel on "
                         [:a {:href "https://discord.gg/glc"} "GLC Discord"] "."]]})}
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
            {:id "adam"
             :title "How do I install Adam's directives?"
             :content [:ul
                        [:p "Adam's directives are installed automatically at the game start. The directives are pulled "
                         "directly from the game-server so do not need to be a part of your deck. The previous workaround "
                         "of explicitly adding the 3 directives to the deck is no longer necessary."]]}
            {:id "banlist"
             :title "What is SBL?"
             :content [:ul
                        [:p "Standard Ban List, also known as SBL, is a list "
                         "of cards with additional deck building restrictions for tournament play. "
                         "For more information refer to "
                         [:a {:href "https://nullsignal.games/players/supported-formats/"} "the Supported Formats page"] "."]
                        [:p "Decks that are valid and fit within tournament restrictions are marked " [:span.legal "Standard legal" ] ". "
                         "Decks that do not fit basic deckbuilding rules are marked " [:span.invalid "Standard invalid"] "."]
                        [:p "Putting cards in your deck that are not yet available for sale (i.e. future spoilers) or ones that are "
                         "out of competitive rotation will also result in your deck being marked as " [:span.casual "Casual legal"] ". Such cards "
                         "should be easy to identify - they are " [:span.casual "highlighted"] " in the deckbuilder."]]}
            {:id "altarts"
             :title "How do I change my decks to use alternative art versions of cards (or promotional ones)?"
             :content [:ul
                        [:p "Alternative art cards are enabled for " [:a {:href "#donations"} "donors"] " and "
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
                        [:p "Because We Built It."]]})}))

(def help-toc
  "Generates list serving as help's table of contents. Parses help-data."
  [:nav {:role "navigation" :class "table-of-contents" :key "nav"}
   (into [:ul]
         (for [{:keys [id title sub]} help-data]
           ^{:key id}
           [:li
            [:a (when id {:href (str "#" id)}) title]
            (into [:ul]
                  (for [{:keys [id title]} sub]
                    ^{:key id}
                    [:li [:a (when id {:href (str "#" id)}) title]]))]))])

(def help-contents
  "Takes help-data and translates it to HTML tags."
  (doall
    (for [{:keys [id title sub]} help-data]
      ^{:key id}
      [:<>
       [:h2 {:id id :key id} title]
       (for [{:keys [id title content]} sub]
         ^{:key title}
         [:div [:h3 {:id id :key title} title]
          content])])))

(defn help []
  [:div.page-container
   [:div.help-bg]
   [:div.help.panel.content-page.blue-shade
    [:h2 "Help Topics"]
    help-toc
    help-contents]])
