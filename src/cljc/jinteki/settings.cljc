(ns jinteki.settings
  "Centralized settings definitions for the application.

   Settings are loaded with precedence: defaults < localStorage < user profile (database).

   Device-specific settings (:sync? false) are never saved to the database and only
   persist in localStorage. All other settings are synced to the user profile.

   All settings are validated before being stored in app state. Invalid values are
   filtered out at each source level, ensuring app state always contains valid settings."
  (:require [clojure.string]))

;; Validation constants
(def valid-background-slugs
  #{"apex-bg" "custom-bg" "find-the-truth-bg" "freelancer-bg"
    "monochrome-bg" "mushin-no-shin-bg" "push-your-luck-bg" "rumor-mill-bg"
    "the-root-bg" "traffic-jam-bg" "worlds2020"})

(def valid-languages
  #{"en" "fr" "ja" "ko" "pl" "pt" "ru" "zh-simp"})

(def valid-pronouns
  #{"none" "any" "myodb" "blank" "they" "she" "sheit" "shethey"
    "he" "heit" "hethey" "heshe" "it" "faefaer" "ne" "ve" "ey"
    "zehir" "zezir" "xe" "xi"})

(def valid-stats-options #{"always" "competitive" "none"})
(def valid-card-zoom-options #{"image" "text"})
(def valid-card-resolution-options #{"default" "high"})
(def valid-runner-board-order #{"jnet" "irl"})
(def valid-log-player-highlight #{"blue-red" "none"})
(def valid-card-back-display #{"them" "me" "ffg" "nsg"})
(def valid-card-sleeves #{"ffg-card-back" "nsg-card-back" "ffg" "nsg"})
(def valid-formats #{"standard" "throwback" "startup" "system-gateway"
                     "core" "preconstructed" "eternal" "casual"})

;; Validation combinators
(defn- validate-coll-of
  "Returns a validator that checks if value is a collection of items matching pred"
  [item-pred coll-pred]
  (fn [value]
    (and (coll-pred value)
         (every? item-pred value))))

(defn- validate-map-of
  "Returns a validator that checks if value is a map with keys/vals matching predicates"
  [key-pred val-pred]
  (fn [value]
    (and (map? value)
         (every? key-pred (keys value))
         (every? val-pred (vals value)))))

;; Basic validation functions
(defn- validate-enum [valid-set value]
  "Returns true if value is in the valid set"
  (contains? valid-set value))

(defn- validate-boolean [value]
  "Returns true if value is a boolean"
  (boolean? value))

(defn- validate-number [value]
  "Returns true if value is a number"
  (number? value))


;; Composed validators
(def validate-blocked-users
  "Validates blocked-users is a vector of strings"
  (validate-coll-of string? vector?))

(def validate-alt-arts
  "Validates alt-arts is a map with keyword keys and any values"
  (validate-map-of keyword? (constantly true)))

(def validate-bespoke-sounds
  "Validates bespoke-sounds is a map with keyword keys and boolean values"
  (validate-map-of keyword? boolean?))

(defn- validate-card-sleeve [value]
  "Validates card sleeve value - accepts base sleeves and any string (for prize sleeves)"
  (or (contains? valid-card-sleeves value)
      (string? value)))

(defn- validate-prizes [value]
  "Validates prizes structure - nil or map with :card-backs containing keyword->boolean map"
  (or (nil? value)
      (and (map? value)
           (or (nil? (:card-backs value))
               ((validate-map-of keyword? boolean?) (:card-backs value))))))

(def validate-visible-formats
  "Validates visible-formats is a set of valid format strings"
  (validate-coll-of #(contains? valid-formats %) set?))

(def all-settings
  "Vector of all application settings with their metadata.
   Each setting has:
   - :key - the setting keyword
   - :default - default value if not set
   - :sync? - whether this setting syncs to database (vs local-only)
   - :validate-fn - optional validation function that returns true if value is valid
   - :doc - optional documentation string describing the setting's purpose"
  [{:key :alt-arts
    :default {}
    :sync? true
    :validate-fn validate-alt-arts
    :doc "User's selected alternate art set when :show-alt-art is true"}
   {:key :archives-sorted
    :default false
    :sync? true
    :validate-fn validate-boolean
    :doc "Whether to sort cards in Archives by name"}
   {:key :background
    :default "worlds2020"
    :sync? true
    :validate-fn #(validate-enum valid-background-slugs %)
    :doc "Selected game board background or 'custom' for a custom image with :custom-bg-url"}
   {:key :bespoke-sounds
    :default {}
    :sync? true
    :validate-fn validate-bespoke-sounds
    :doc "Card-specific sound effect preferences"}
   {:key :blocked-users
    :default []
    :sync? true
    :validate-fn validate-blocked-users
    :doc "List of usernames to block in chat and lobbies"}
   {:key :card-back-display
    :default "them"
    :sync? true
    :validate-fn #(validate-enum valid-card-back-display %)
    :doc "Which card backs to display (them/me/ffg/nsg)"}
   {:key :card-resolution
    :default "default"
    :sync? false  ; device-specific
    :validate-fn #(validate-enum valid-card-resolution-options %)
    :doc "Card image quality preference for this device"}
   {:key :card-zoom
    :default "image"
    :sync? true
    :validate-fn #(validate-enum valid-card-zoom-options %)
    :doc "How to display zoomed cards (image/text)"}
   {:key :corp-card-sleeve
    :default "nsg-card-back"
    :sync? true
    :validate-fn validate-card-sleeve
    :doc "Selected card back design for Corp deck"}
   {:key :custom-bg-url
    :default "https://nullsignal.games/wp-content/uploads/2022/07/Mechanics-of-Midnight-Sun-Header.png"
    :sync? true
    :validate-fn string?
    :doc "URL for custom game board background image"}
   {:key :deckstats
    :default "always"
    :sync? true
    :validate-fn #(validate-enum valid-stats-options %)
    :doc "When to show deck statistics (always/competitive/none)"}
   {:key :default-format
    :default "standard"
    :sync? true
    :validate-fn #(validate-enum valid-formats %)
    :doc "Default game format when creating new games"}
   {:key :disable-websockets
    :default false
    :sync? false  ; device-specific
    :validate-fn validate-boolean
    :doc "Disable WebSocket connections on this device (uses polling instead)"}
   {:key :display-encounter-info
    :default false
    :sync? true
    :validate-fn validate-boolean
    :doc "Show detailed encounter information during runs"}
   {:key :gamestats
    :default "always"
    :sync? true
    :validate-fn #(validate-enum valid-stats-options %)
    :doc "When to record game statistics (always/competitive/none)"}
   {:key :ghost-trojans
    :default true
    :sync? true
    :validate-fn validate-boolean
    :doc "Show ghost images for Trojan programs"}
   {:key :heap-sorted
    :default false
    :sync? true
    :validate-fn validate-boolean
    :doc "Whether to sort cards in Heap by name"}
   {:key :labeled-cards
    :default false
    :sync? false  ; device-specific
    :validate-fn validate-boolean
    :doc "Show card name labels on game board (device-specific)"}
   {:key :labeled-unrezzed-cards
    :default false
    :sync? false  ; device-specific
    :validate-fn validate-boolean
    :doc "Show labels on unrezzed cards (device-specific)"}
   {:key :language
    :default "en"
    :sync? true
    :validate-fn #(validate-enum valid-languages %)
    :doc "User interface language preference"}
   {:key :lobby-sounds
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean
    :doc "Play sounds in lobby on this device"}
   {:key :log-player-highlight
    :default "blue-red"
    :sync? true
    :validate-fn #(validate-enum valid-log-player-highlight %)
    :doc "Color scheme for highlighting players in game log"}
   {:key :log-timestamps
    :default true
    :sync? true
    :validate-fn validate-boolean
    :doc "Show timestamps in game log"}
   {:key :log-top
    :default 419
    :sync? false  ; device-specific
    :validate-fn validate-number
    :doc "Vertical position of game log panel (device-specific)"}
   {:key :log-width
    :default 300
    :sync? false  ; device-specific
    :validate-fn validate-number
    :doc "Width of game log panel in pixels (device-specific)"}
   {:key :pass-on-rez
    :default false
    :sync? true
    :validate-fn validate-boolean
    :doc "Automatically pass priority after rezzing cards"}
   {:key :pin-zoom
    :default false
    :sync? true
    :validate-fn validate-boolean
    :doc "Keep card zoom window pinned open"}
   {:key :player-stats-icons
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean
    :doc "Show icons in player stats area (device-specific)"}
   {:key :prizes
    :default nil
    :sync? true
    :validate-fn validate-prizes
    :doc "Unlocked prize content (card backs, etc.); set by admins not user"}
   {:key :pronouns
    :default "none"
    :sync? true
    :validate-fn #(validate-enum valid-pronouns %)
    :doc "User's preferred pronouns for display"}
   {:key :runner-board-order
    :default "irl"
    :sync? true
    :validate-fn #(validate-enum valid-runner-board-order %)
    :doc "Layout order for Runner board areas (irl/jnet)"}
   {:key :runner-card-sleeve
    :default "nsg-card-back"
    :sync? true
    :validate-fn validate-card-sleeve
    :doc "Selected card back design for Runner deck"}
   {:key :show-alt-art
    :default true
    :sync? true
    :validate-fn validate-boolean
    :doc "Display alternate card art when available"}
   {:key :sides-overlap
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean
    :doc "Allow Corp/Runner areas to overlap on small screens (device-specific)"}
   {:key :sounds
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean
    :doc "Enable in-game sound effects on this device"}
   {:key :sounds-volume
    :default 100
    :sync? false  ; device-specific
    :validate-fn validate-number
    :doc "Sound effects volume level (0-100) on this device"}
   {:key :stacked-cards
    :default true
    :sync? true
    :validate-fn validate-boolean
    :doc "Stack un-iced servers of the same card"}
   {:key :visible-formats
    :default nil
    :sync? false  ; handled separately in account.cljs
    :validate-fn validate-visible-formats
    :doc "Set of game formats to show in lobby (device-specific)"}])

(defn setting-keys
  "Returns a vector of all setting keys"
  []
  (mapv :key all-settings))

(defn sync-keys
  "Returns a vector of settings that sync to the database"
  []
  (mapv :key (filter :sync? all-settings)))

(defn local-only-keys
  "Returns a vector of settings that are local-only (device-specific)"
  []
  (mapv :key (remove :sync? all-settings)))

(defn browser-language
  "Get browser language, mapped to supported language or 'en' fallback"
  []
  #?(:cljs
     (let [lang (some-> js/navigator.language (clojure.string/split #"-") first)]
       (cond
         ;; if we ever implement proper zh, fix this
         (= lang "zh") "zh-simp"
         (contains? valid-languages lang) lang
         :else "en"))
     :clj "en"))  ; fallback for server-side

(defn defaults
  "Returns a map of setting keys to their default values.
   Language default is computed from browser language when available."
  []
  (into {} (map (fn [{:keys [key default]}]
                  (if (= key :language)
                    [key (browser-language)]
                    [key default]))
                all-settings)))

(defn get-setting
  "Get a setting definition by key"
  [key]
  (first (filter #(= (:key %) key) all-settings)))

(defn filter-valid-settings
  "Filter a settings map to only include valid values, removing invalid ones"
  [settings-map]
  (reduce (fn [filtered {:keys [key validate-fn]}]
            (let [value (get settings-map key)]
              (if (and (some? value) validate-fn (validate-fn value))
                (assoc filtered key value)
                filtered)))
          {}
          all-settings))

(defn load-from-storage
  "Load settings from localStorage, returning only the values that exist.
   Takes a storage-load-fn that accepts (storage-key default-value)."
  [storage-load-fn]
  (let [defaults-map (defaults)]
    (reduce (fn [opts {:keys [key]}]
              (let [storage-name (name key)
                    default-val (get defaults-map key)
                    loaded-value (storage-load-fn storage-name default-val)]
                ;; Only include if different from default (meaning it was actually stored)
                (if (not= loaded-value default-val)
                  (assoc opts key loaded-value)
                  opts)))
            {}
            all-settings)))
