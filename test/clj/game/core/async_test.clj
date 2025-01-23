(ns game.core.async-test
  (:require
   [clojure.test :refer :all]
   [instaparse.core :as insta]
   [clojure.string :as str]))

(def scuffed-grammar
  (insta/parser
    "<S>=P_EXPR*

     <EXPR>=<COMMENT?>(S_EXPR|TOKEN|STR)
     <P_EXPR>=<SPACE>EXPR<SPACE>
     <COMMENT>='#_'
     (* note that S_EXPR cannot contain tokens or strings, to prevent left-expansion *)
     (* from eradicating the heap *)
     <S_EXPR>=VEC|MAP|FN|SET
     VEC=<'['>P_EXPR*<']'>
     MAP=<'{'>KEYPAIR*<'}'>
     FN=<('#(' | '(' | '\\'(' )>P_EXPR*<')'>
     SET=<'#{'>P_EXPR*<'}'>
     <KEYPAIR>=P_EXPR P_EXPR

     (* basic strings and tokens *)
     S_TOKEN=STR|TOKEN
     <QUOT>=<'\"'>
     <TOKEN>=#'[^\\s()\\[\\]{}#\\\"]*'
     <SPACE>=<#'\\s'*>
     (* note that we dont care about the contents of strings, we can just hide them *)
     STR=<'#'?>QUOT<('\\\"' | #'[^\"]')*>QUOT"))

(def card-base-str "src/clj/game/cards/")
(def relevant-cards-files ["basic.clj" "agendas.clj" "assets.clj" "events.clj" "hardware.clj"
                           "ice.clj" "identities.clj" "operations.clj" "programs.clj"
                           "resources.clj" "upgrades.clj"])

(defn stitch-and-split-card-files
  [file]
  (let [split-file (str/split file #"(?=\(def)")
        restitch-fn (fn [chunk]
                      (let [lines (str/split-lines chunk)
                            ;; special case specifically for the hydra subs, which have semicolons
                            sans-comments (map first (map #(str/split % #"(?<!tagged);" 2) lines))]
                        (str/join " " (map str/trim sans-comments))))
        split-and-stitch (map restitch-fn split-file)]
    split-and-stitch))

(defn- contains-eid?
  [chunk]
  (some #(cond
           (string? %) (= % "eid")
           (vector? %) (contains-eid? %)
           :else nil)
        chunk))

(defn completes?
  "does a chunk complete an eid (probably)?"
  [chunk depth]
  (cond
    (and (string? chunk) (zero? depth)) :maybe
    ;; special case for fns which defer the def elsewhere
    (and (vector? chunk) (= 2 (count chunk)) (zero? depth)) :maybe
    (and (vector? chunk) (= (first chunk) :FN))
    (let [func-name (second chunk)]
      (if (or (contains-eid? chunk)
              (= func-name "continue-ability"))
        :maybe
        (completes? (last chunk) (inc depth))))
    :else nil))

;; TODO - can add a few more to these as errors get picked up down the line
(def terminal-fns #{"checkpoint" "complete-with-result" "continue-ability" "corp-install" "damage" "draw" "effect-completed" "gain-credits" "resolve-ability" "runner-install"
                    "trash" "trash-cards" "trigger-event-sync" "wait-for"})
(defn should-complete?
  "Should a chunk (probably) complete an eid?"
  [chunk depth]
  (cond
    (and (string? chunk) (zero? depth)) nil
    (and (vector? chunk) (= 2 (count chunk)) (zero? depth)) nil
    (and (vector? chunk) (= (first chunk) :FN))
    (let [func-name (second chunk)]
      (if (contains? terminal-fns func-name)
        true
        (some #(should-complete? % (inc depth)) (rest (rest chunk)))))
    :else nil))

(defn is-valid-chunk?
  ([chunk]
   (cond
     (not (sequential? chunk)) true
     (= :FN (first chunk)) (every? is-valid-chunk? (rest chunk))
     (= :VEC (first chunk)) (every? is-valid-chunk? (rest chunk))
     (= :SET (first chunk)) (every? is-valid-chunk? (rest chunk))
     (= [:STR] chunk) true
     (= :MAP (first chunk)) (is-valid-chunk? (rest chunk) :MAP)
     (sequential? chunk) (every? is-valid-chunk? chunk)
     :else true))
  ([chunk conditional?]
   (cond
     (= conditional? :MAP)
     (do (let [keypairs (partition 2 chunk)
               mapped (zipmap (map first keypairs) (map second keypairs))]
           ;; if it contains an :effect, then:
           ;;   see if it contains :async true. If it does, effect must complete eid
           ;;   if it does not, effect should not complete the eid
           (and
             (if (:effect mapped)
               (if (:async mapped)
                 (is-valid-chunk? (:effect mapped) :async)
                 (is-valid-chunk? (:effect mapped) :sync))
               (every? is-valid-chunk? (vals mapped)))
             ;; note that async effects require cancels to be async, etc
             (if (:cancel-effect mapped)
               (if (:async mapped)
                 (is-valid-chunk? (:cancel-effect mapped) :async)
                 (is-valid-chunk? (:cancel-effect mapped) :sync))
               true))))
     (= conditional? :async)
     (and (completes? chunk 0) (is-valid-chunk? chunk))
     (= conditional? :sync)
     (and (not (should-complete? chunk 0)) (is-valid-chunk? chunk)))))

(defn clean-chunks
  "remove the empties and nils, and swaps keywords in"
  [chunks]
  (cond
    (= "" chunks) nil
    (= "true" chunks) true
    (= "nil" chunks) :nil
    (and (string? chunks) (str/starts-with? chunks ":")) (keyword (subs chunks 1))
    (sequential? chunks) (filterv identity (map clean-chunks chunks))
    :else chunks))

(defn validate-chunk [chunk] (->> chunk scuffed-grammar clean-chunks is-valid-chunk?))

(deftest cards-are-async-test
  (doseq [fname relevant-cards-files]
    (let [f (slurp (str card-base-str fname))
          chunks (stitch-and-split-card-files f)]
      (let [invalid-chunks (filter (complement validate-chunk) chunks)
            titles (map #(re-find #" \".+?\"" %) invalid-chunks)]
        (when (seq titles)
          (is nil (str "The following cards/fns in file '" fname "' may be invalid (async/sync): " (str/join ", " titles))))))))
