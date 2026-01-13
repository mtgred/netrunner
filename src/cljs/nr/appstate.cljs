(ns nr.appstate
  (:require
   [cljs.core.async :refer [<!] :refer-macros [go]]
   [jinteki.i18n :as i18n]
   [jinteki.settings :as settings]
   [jinteki.utils :refer [str->int]]
   [nr.ajax :refer [GET]]
   [nr.local-storage :as ls]
   [clojure.string :as str]
   [clojure.set :as set]
   [reagent.core :as r]))

(defn- migrate-legacy-localStorage-keys!
  "Migrates legacy localStorage keys to new consistent kebab-case format"
  []
  (ls/migrate-keys! {"custom_bg_url" "custom-bg-url"
                     "sounds_volume" "sounds-volume"
                     "lobby_sounds" "lobby-sounds"
                     "volume" "sounds-volume"}))


;; These "new-formats" will automatically be added into the visible-formats for a player exactly once
(def new-formats #{"quick-draft" "chimera"})

(defn- get-new-formats!
  "Mark formats when they get added so they are discoverable"
  []
  (let [seen-formats (ls/load "seen-formats" #{})
        fresh-formats (set/difference new-formats seen-formats)
        new-seen-formats (set/union fresh-formats seen-formats)]
    (ls/save! "seen-formats" new-seen-formats)
    fresh-formats))

(defn- load-visible-formats
  "Loading visible formats from localStorage"
  []
  (let [default-visible-formats #{"standard"
                                  "system-gateway"
                                  "quick-draft"
                                  "core"
                                  "throwback"
                                  "startup"
                                  "eternal"
                                  "preconstructed"
                                  "chimera"
                                  "casual"}
        visible (ls/load "visible-formats" default-visible-formats)
        unseen (get-new-formats!)]
    (if (seq unseen)
      (let [visible (set/union visible unseen)]
        (ls/save! "visible-formats" visible)
        visible)
      visible)))

;; Run migration before creating app-state
(migrate-legacy-localStorage-keys!)

(def app-state
  (let [js-user (js->clj js/user :keywordize-keys true)]
    (r/atom {:active-page "/"
             :user js-user
             :options (let [defaults (settings/defaults)
                            localStorage-settings (settings/filter-valid-settings
                                                    (settings/load-from-storage ls/load))
                            user-profile-settings (settings/filter-valid-settings
                                                    (:options js-user))]
                        (merge defaults localStorage-settings user-profile-settings))
             :cards-loaded false
             :connected false
             :previous-cards {}
             :sets [] :mwl [] :cycles []
             :decks [] :decks-loaded false
             :stats (:stats js-user)
             :visible-formats (load-visible-formats)
             :channels {:general [] :america [] :europe [] :asia-pacific [] :united-kingdom [] :français []
                        :español [] :italia [] :polska [] :português [] :sverige [] :stimhack-league [] :русский []}
             :games []
             :current-game nil
             :block-game-creation false})))

(go (let [lang (get-in @app-state [:options :language] "en")
          response (<! (GET (str "/data/language/" lang)))]
      (when (= 200 (:status response))
        (i18n/insert-lang! lang (:json response)))))

(defn current-gameid [app-state]
  (get-in @app-state [:current-game :gameid]))
