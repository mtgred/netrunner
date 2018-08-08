(ns tasks.cards
  "Utilities for card tests"
  (:require [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [jinteki.cards :refer [all-cards]]
            [game.utils :as utils :refer [make-cid]]
            [game-test.core :refer [load-cards]]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]))

(defn- get-card-by-type
  "Get the normalized title (as a symbol) for cards of a specific type in the database.
  This is needed because we put icebreakers in their own file/folder, even tho their
  type _is technically_ Program. If we want to focus on Programs, we remove Icebreakers;
  if we want to focus on Icebreakers, we filter for Icebreakers. If we're doing other
  types, we just return the collection."
  [t]
  (let [card-type (if (= "Icebreaker" t) "Program" t)
        f (case t
            "Icebreaker" filter
            "Program" remove
            nil)
        func (fn [coll]
               (if f
                 (f #(and (:subtype %)
                          (string/includes? (:subtype %) "Icebreaker")) coll)
                 coll))]
    (->> (vals @all-cards)
         (map #(select-keys % [:normalizedtitle :type :subtype]))
         (filter #(= card-type (:type %)))
         func
         (map :normalizedtitle)
         (map symbol))))

(defn- get-tests
  "Returns the names of all tests in a namespace"
  [nspace]
  (doall (pmap load-file
               (->> (io/file (str "test/clj/game_test/cards/" nspace))
                    file-seq
                    (filter #(and (.isFile %)
                                  (string/ends-with? (.getPath %) ".clj")))
                    sort
                    (map str))))
  (->> (all-ns)
       (filter #(string/starts-with? % (str "game-test.cards." nspace)))
       (map ns-publics)
       (apply merge)
       (filter (fn [[k v]] (contains? (meta v) :test)))
       (remove (fn [[k v]] (or (:skip-card-coverage (meta v))
                               (contains? (meta v) :private))))
       (map (fn [[k v]] (if-let [title (:card-title (meta v))]
                          (symbol title) k)))))

(def ansi-esc "\u001B")
(def ansi-reset "\u001B[0m")
(def ansi-bold "[1m")
(def ansi-red "[1;31m")
(def ansi-green "[1;32m")
(def ansi-blue "[1;34m")

(defn- format-output
  [s cnt color]
  (str ansi-esc color s ansi-reset ansi-esc ansi-bold cnt ansi-reset))

(def cards-total (atom 0))
(def cards-with-tests (atom 0))
(def cards-without-tests (atom 0))

(defn- compare-tests
  [[k v] show-all show-none]
  (let [cards (get-card-by-type k)
        tests (get-tests v)
        [cards-wo tests-wo both] (diff (set cards)
                                       (set tests))]
    (swap! cards-total #(+ % (count (set cards))))
    (swap! cards-with-tests #(+ % (count both)))
    (swap! cards-without-tests #(+ % (count cards-wo)))
    (println (str ansi-esc ansi-blue k ansi-reset))
    (println "\tUnique cards in db:" (count (set cards)))
    (println "\tTests:" (count tests))
    (println (format-output "\tCards with tests: " (count both) ansi-green))
    (when (and show-all (not show-none))
      (doseq [c (sort both)]
        (println "\t\t" c)))
    (println (format-output "\tCards without tests: " (count cards-wo) ansi-red))
    (when (not show-none)
      (doseq [c (sort cards-wo)]
        (println "\t\t" c)))
    (println (format-output "\tTests without cards: " (count tests-wo) ansi-red))
    (when (not show-none)
      (doseq [c (sort tests-wo)]
        (println "\t\t" c)))))

(defn- load-all-cards []
  (->> (load-cards)
       (map #(assoc % :cid (make-cid)))
       (map (juxt :title identity))
       (into {})
       (reset! all-cards)))

(defn test-coverage
  "Determine which cards have tests written for them. Takes an `--only <Type>` argument to limit output to a specific card type."
  [& args]
  (load-all-cards)
  (let [only (some #{"--only"} args)
        card-type (first (remove #(string/starts-with? % "--") args))
        show-all (some #{"--show-all"} args)
        show-none (some #{"--show-none"} args)
        nspaces {"Agenda" "agendas"
                 "Asset" "assets"
                 "Event" "events"
                 "Hardware" "hardware"
                 "ICE" "ice"
                 "Icebreaker" "icebreakers"
                 "Identity" "identities"
                 "Operation" "operations"
                 "Program"  "programs"
                 "Resource" "resources"
                 "Upgrade" "upgrades"}
        filtered-nspaces (if only
                           (select-keys nspaces [card-type])
                           (into (sorted-map) nspaces))]
    (when only
      (println "Only checking cards of type" (str ansi-esc ansi-blue card-type ansi-reset)))
    (doseq [ct filtered-nspaces]
      (compare-tests ct show-all show-none)
      (println))
    (println (str ansi-esc ansi-blue "Totals" ansi-reset))
    (println "\tTotal cards: " @cards-total)
    (println (format-output "\tCards with tests: " @cards-with-tests ansi-green))
    (println (format-output "\tCards without tests: " @cards-without-tests ansi-red))))
