(ns nr.landing
  (:require
   [nr.translations :refer [tr-element tr-element-with-embedded-content tr-span]]))

(def landing-content
  [:div.landing.panel.content-page.blue-shade
   [tr-element :h2 [:landing_welcome "Welcome!"]]
   [tr-element :p  [:landing_rules-are-incomplete "This website is for the facilitation of Netrunner games online. Please note that jinteki.net may not provide a complete implementation of the rules of the game."]]
   [tr-element :h4 [:landing_make-jinteki-better "Making Jinteki.net better:"]]
   ;; todo - this may not conjugate correctly for other languages.
   (let [link [:a {:href "https://github.com/mtgred/netrunner" :target "_blank"}
               [tr-span [:landing_github "Github"]]]]
     [tr-element-with-embedded-content :p [:landing_jinteki-is-free [:span "Jinteki.net is the product of voluntary contributions made by many individuals. If you wish to make Jinteki.net better, found a bug and need to report an issue, or just like reading code, simply visit our " link " page."]] {:link link}])
   [tr-element :h4 [:landing_use-header "The use of Jinteki.net:"]]
   [:ul.list.compact
    [tr-element :li [:landing_please-be-nice "Please be respectful. Any disrespectful conduct will not be tolerated regardless of the circumstance or rationale."]]
    [tr-element :li [:landing_prison-decks-exist "There are many deck archetypes and playstyles in Netrunner. All are valid and should be respected. If you do not wish to play against a certain deck or playstyle please write it in the game title (“No Project Vacheron” or “Experienced/New players only”). If the game has already started, politely explain to your opponent and concede the game."]]
    [tr-element :li [:landing_please-be-nice-in-global "The global chat tab should only be used for Netrunner-related discussion (including unofficial rules clarifications) and for trying to reach out to users who may have disconnected. Inappropriate use of global chat includes disputes with other players, airing grievances, and everything outlined as unacceptable behavior below."]]]
   [tr-element :h4 [:landing_please-dont-do-these "Examples of unacceptable behavior include, but are not limited to, the following:"]]
   [:ul.list.compact
    [tr-element :li [:landing_please-no-harassment "Harassing your opponent based on their playstyle or deck."]]
    [tr-element :li [:landing_please-no-ugly-titles "Game titles which could reasonably be considered inappropriate or offensive"]]
    [tr-element :li [:landing_please-no-trolling  "Trolling, insulting/derogatory comments, casual use of slurs, pejorative language, personal/political attacks, harassment, intimidation, threats, or anything outside the scope of playing Netrunner."]]
    [tr-element :li [:landing_please-no-smut "The use of sexualized language or imagery."]]
    [tr-element :li [:landing_please-no-trigger-mocking "Making light of/making mocking comments about trigger warnings or content warnings."]]
    [tr-element :li [:landing_please-no-misgendering "Deliberately using incorrect pronouns for a person, especially after being informed of the correct ones. If unsure, use gender-neutral language."]]]
   (let [mail [:a {:href "mailto:jnetmods@gmail.com"}"jnetmods@gmail.com"]]
     [tr-element-with-embedded-content :p [:landing_report-here [:span "To report an incident or to contact the moderation team please email " mail ". If reporting an incident, please include screenshots if possible."]] {:email mail}])
   [tr-element :p [:landing_moderators-will-respond "Moderators will respond to offenses by attempting to contact users for resolution where possible. Repeated/severe offenses will be reviewed by the moderation team and met with temporary or permanent bans. All bans are reviewed by the entire moderation team."]]
   [tr-element :p [:landing_moderators-arent-judges "Moderators are not here to settle rules disputes or otherwise serve as judges. If there is a rules disagreement, bringing it to a community space is the best plan for resolution."]]])

(defn landing []
  [:div.page-container
   [:div.worlds2020]
   [:div.landing-message
    (let [link [:a {:href "https://www.nearearthhub.net/" :target "_blank"} "nearearthhub.net"]]
      [tr-element-with-embedded-content :h4 [:landing_visit-neh-for-rules [:span "Visit " link " for links to rules and other resources"]] {:link link} nil])
    landing-content]])
