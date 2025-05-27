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

;; Validation functions
(defn- validate-enum [valid-set value]
  "Returns true if value is in the valid set"
  (contains? valid-set value))

(defn- validate-boolean [value]
  "Returns true if value is a boolean"
  (boolean? value))

(defn- validate-number [value]
  "Returns true if value is a number"
  (number? value))

(defn- validate-any [value]
  "Placeholder validator that accepts any value - to be replaced with specific validation later"
  true)

(defn- validate-blocked-users [value]
  "Validates blocked-users is a vector of strings"
  (and (vector? value)
       (every? string? value)))

(defn- validate-alt-arts [value]
  "Validates alt-arts is a map with keyword keys"
  (and (map? value)
       (every? keyword? (keys value))))

(defn- validate-bespoke-sounds [value]
  "Validates bespoke-sounds is a map with keyword keys and boolean values"
  (and (map? value)
       (every? keyword? (keys value))
       (every? boolean? (vals value))))

(def all-settings
  "Vector of all application settings with their metadata.
   Each setting has:
   - :key - the setting keyword
   - :default - default value if not set
   - :sync? - whether this setting syncs to database (vs local-only)
   - :validate-fn - optional validation function that returns true if value is valid"
  [{:key :alt-arts
    :default {}
    :sync? true
    :validate-fn validate-alt-arts}
   {:key :archives-sorted
    :default false
    :sync? true
    :validate-fn validate-boolean}
   {:key :background
    :default "worlds2020"
    :sync? true
    :validate-fn #(validate-enum valid-background-slugs %)}
   {:key :bespoke-sounds
    :default {}
    :sync? true
    :validate-fn validate-bespoke-sounds}
   {:key :blocked-users
    :default []
    :sync? true
    :validate-fn validate-blocked-users}
   {:key :card-back-display
    :default "default"
    :sync? true
    :validate-fn validate-any}  ; TODO: define valid card back values
   {:key :card-resolution
    :default "default"
    :sync? false  ; device-specific
    :validate-fn #(validate-enum valid-card-resolution-options %)}
   {:key :card-zoom
    :default "image"
    :sync? true
    :validate-fn #(validate-enum valid-card-zoom-options %)}
   {:key :corp-card-sleeve
    :default "nsg-card-back"
    :sync? true
    :validate-fn validate-any}  ; TODO: define valid card sleeve values
   {:key :custom-bg-url
    :default "https://nullsignal.games/wp-content/uploads/2022/07/Mechanics-of-Midnight-Sun-Header.png"
    :sync? true
    :validate-fn string?}
   {:key :deckstats
    :default "always"
    :sync? true
    :validate-fn #(validate-enum valid-stats-options %)}
   {:key :default-format
    :default "standard"
    :sync? true
    :validate-fn validate-any}  ; TODO: define valid format values
   {:key :disable-websockets
    :default false
    :sync? false  ; device-specific
    :validate-fn validate-boolean}
   {:key :display-encounter-info
    :default false
    :sync? true
    :validate-fn validate-boolean}
   {:key :gamestats
    :default "always"
    :sync? true
    :validate-fn #(validate-enum valid-stats-options %)}
   {:key :ghost-trojans
    :default true
    :sync? true
    :validate-fn validate-boolean}
   {:key :heap-sorted
    :default false
    :sync? true
    :validate-fn validate-boolean}
   {:key :labeled-cards
    :default false
    :sync? false  ; device-specific
    :validate-fn validate-boolean}
   {:key :labeled-unrezzed-cards
    :default false
    :sync? false  ; device-specific
    :validate-fn validate-boolean}
   {:key :language
    :default "en"
    :sync? true
    :validate-fn #(validate-enum valid-languages %)}
   {:key :lobby-sounds
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean}
   {:key :log-player-highlight
    :default "blue-red"
    :sync? true
    :validate-fn #(validate-enum valid-log-player-highlight %)}
   {:key :log-timestamps
    :default true
    :sync? true
    :validate-fn validate-boolean}
   {:key :log-top
    :default 419
    :sync? false  ; device-specific
    :validate-fn validate-number}
   {:key :log-width
    :default 300
    :sync? false  ; device-specific
    :validate-fn validate-number}
   {:key :pass-on-rez
    :default false
    :sync? true
    :validate-fn validate-boolean}
   {:key :pin-zoom
    :default false
    :sync? true
    :validate-fn validate-boolean}
   {:key :player-stats-icons
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean}
   {:key :prizes
    :default nil
    :sync? true
    :validate-fn validate-any}  ; TODO: define valid prize structure
   {:key :pronouns
    :default "none"
    :sync? true
    :validate-fn #(validate-enum valid-pronouns %)}
   {:key :runner-board-order
    :default "irl"
    :sync? true
    :validate-fn #(validate-enum valid-runner-board-order %)}
   {:key :runner-card-sleeve
    :default "nsg-card-back"
    :sync? true
    :validate-fn validate-any}  ; TODO: define valid card sleeve values
   {:key :show-alt-art
    :default true
    :sync? true
    :validate-fn validate-boolean}
   {:key :sides-overlap
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean}
   {:key :sounds
    :default true
    :sync? false  ; device-specific
    :validate-fn validate-boolean}
   {:key :sounds-volume
    :default 100
    :sync? false  ; device-specific
    :validate-fn validate-number}
   {:key :stacked-cards
    :default true
    :sync? true
    :validate-fn validate-boolean}
   {:key :visible-formats
    :default nil
    :sync? false  ; handled separately in account.cljs
    :validate-fn validate-any}])  ; TODO: define valid format set structure  ; handled separately in account.cljs

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
