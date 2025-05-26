(ns jinteki.settings
  "Centralized settings definitions for the application.
   Defines all settings with their metadata including default values
   and whether they sync to the database.")

(def all-settings
  "Vector of all application settings with their metadata.
   Each setting has:
   - :key - the setting keyword
   - :default - default value if not set
   - :sync? - whether this setting syncs to database (vs local-only)
   - :storage-key - localStorage key name (if different from key name)"
  [{:key :alt-arts
    :default {}
    :sync? true}
   {:key :archives-sorted
    :default false
    :sync? true}
   {:key :background
    :default "worlds2020"
    :sync? true}
   {:key :bespoke-sounds
    :default {}
    :sync? true}
   {:key :blocked-users
    :default []
    :sync? true}
   {:key :card-back-display
    :default "default"
    :sync? true}
   {:key :card-resolution
    :default "default"
    :sync? false}  ; device-specific
   {:key :card-zoom
    :default "image"
    :sync? true}
   {:key :corp-card-sleeve
    :default "nsg-card-back"
    :sync? true}
   {:key :custom-bg-url
    :default "https://nullsignal.games/wp-content/uploads/2022/07/Mechanics-of-Midnight-Sun-Header.png"
    :sync? true}
   {:key :deckstats
    :default "always"
    :sync? true}
   {:key :default-format
    :default "standard"
    :sync? true}
   {:key :disable-websockets
    :default false
    :sync? false}  ; device-specific
   {:key :display-encounter-info
    :default false
    :sync? true}
   {:key :gamestats
    :default "always"
    :sync? true}
   {:key :ghost-trojans
    :default true
    :sync? true}
   {:key :heap-sorted
    :default false
    :sync? true}
   {:key :labeled-cards
    :default false
    :sync? false}  ; device-specific
   {:key :labeled-unrezzed-cards
    :default false
    :sync? false}  ; device-specific
   {:key :language
    :default "en"
    :sync? true}
   {:key :lobby-sounds
    :default true
    :sync? false}  ; device-specific
   {:key :log-player-highlight
    :default "blue-red"
    :sync? true}
   {:key :log-timestamps
    :default true
    :sync? true}
   {:key :log-top
    :default 419
    :sync? false}  ; device-specific
   {:key :log-width
    :default 300
    :sync? false}  ; device-specific
   {:key :pass-on-rez
    :default false
    :sync? true}
   {:key :pin-zoom
    :default false
    :sync? true}
   {:key :player-stats-icons
    :default true
    :sync? false}  ; device-specific
   {:key :prizes
    :default nil
    :sync? true}
   {:key :pronouns
    :default "none"
    :sync? true}
   {:key :runner-board-order
    :default "irl"
    :sync? true}
   {:key :runner-card-sleeve
    :default "nsg-card-back"
    :sync? true}
   {:key :show-alt-art
    :default true
    :sync? true}
   {:key :sides-overlap
    :default true
    :sync? false}  ; device-specific
   {:key :sounds
    :default true
    :sync? false}  ; device-specific
   {:key :sounds-volume
    :default 100
    :sync? false  ; device-specific
    :storage-key "sounds-volume"}  ; special storage key
   {:key :stacked-cards
    :default true
    :sync? true}
   {:key :visible-formats
    :default nil
    :sync? false}])  ; handled separately in account.cljs

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

(defn defaults
  "Returns a map of setting keys to their default values"
  []
  (into {} (map (juxt :key :default) all-settings)))

(defn get-setting
  "Get a setting definition by key"
  [key]
  (first (filter #(= (:key %) key) all-settings)))

(defn storage-key
  "Get the localStorage key for a setting (handles special cases)"
  [key]
  (or (:storage-key (get-setting key))
      (name key)))