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
    (and (vector? chunk) (= (first chunk) :FN))
    (let [func-name (second chunk)]
      (if (or (contains-eid? chunk)
              (= func-name "continue-ability"))
        :maybe
        (completes? (last chunk) (inc depth))))
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
     (do ;;(println "Make a map out of: " chunk)
         (let [keypairs (partition 2 chunk)
               mapped (zipmap (map first keypairs) (map second keypairs))]
           ;; if it contains an :effect, then:
           ;;   see if it contains :async true. If it does, effect must complete eid
           ;;   if it does not, effect should not complete the eid
           (if (:effect mapped)
             ;; TODO - prompts that are async may require the cancel effect to be async too
             (if (:async mapped)
               (is-valid-chunk? (:effect mapped) :async)
               (is-valid-chunk? (:effect mapped) :sync))
             (every? is-valid-chunk? (vals mapped)))))
     (= conditional? :async)
     (do (let [comp (completes? chunk 0)]
           (and comp (is-valid-chunk? chunk))))
     (= conditional? :sync)
     (is-valid-chunk? chunk))))

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
      (doseq [chunk chunks]
        (is (validate-chunk chunk) (str "invalid chunk at " chunk))))))
