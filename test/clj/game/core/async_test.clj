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
  [chunk depth]
  (some #(cond
           (string? %) (= % "eid")
           (and (vector? %) (= (second %) "make-eid")) true
           (and (vector? %) (= (second %) "assoc")) (contains-eid? % (inc depth))
           (and (vector? %) (zero? depth)) (contains-eid? % 1)
           :else nil)
        chunk))

(defn completes?
  "does a chunk complete an eid (probably)?"
  [chunk depth]
  (cond
    ;; TODO - see if we can actually map these maybe?
    ;; if we're just deffering to another def, that map should be able to check for itself
    (and (string? chunk) (zero? depth)) :maybe
    ;; special case for fns which defer the def elsewhere
    (and (vector? chunk) (= 2 (count chunk)) (zero? depth)) :maybe
    ;; both sides of the ifn should complete
    (and (vector? chunk) (= (first chunk) :FN)
         (contains? #{"if" "if-not" "if-let"} (second chunk)))
    (and (completes? (nth chunk 3 nil) (inc depth))
         (completes? (nth chunk 4 nil) (inc depth)))
    ;; `when ... complete` is a bad pattern, we should avoid it
    (and (vector? chunk) (= (first chunk) :FN)
         (contains? #{"when" "when-not" "when-let"} (second chunk)))
    nil
    ;; cond - every RHS pair should complete
    (and (vector? chunk) (= (first chunk) :FN) (or (= (second chunk) "cond")))
    (let [assignments (take-nth 2 (nthrest chunk 3))]
      (every? #(completes? % (inc depth)) assignments))
    ;; condp - same as above, just shifted over 1 more
    (and (vector? chunk) (= (first chunk) :FN) (= (second chunk) "condp"))
    (let [assignments (take-nth 2 (nthrest chunk 4))]
      (every? #(completes? % (inc depth)) assignments))
    ;; case - every RHS pair should complete, and the terminal (last element) should too
    (and (vector? chunk) (= (first chunk) :FN) (= (second chunk) "case"))
    (let [assignments (concat (take-nth 2 (nthrest chunk 4)) [(last chunk)])]
      (every? #(completes? % (inc depth)) assignments))
    ;; cond+ - the RHS of every child vec should complete
    (and (vector? chunk) (= (first chunk) :FN) (= (second chunk) "cond+"))
    (let [children (nthrest chunk 2)]
      (every? #(or (string? %) (completes? (last %) (inc depth))) children))
    ;; regular fn, or continue-abi
    (and (vector? chunk) (= (first chunk) :FN)
         (or (= (second chunk) "continue-ability")
             (contains-eid? chunk 0)
             (and (> (count chunk) 2) (completes? (last chunk) (inc depth)))))
    :maybe
    ;; other fns - see if the rightmost member completes
    (and (vector? chunk) (= (first chunk) :FN))
    (completes? (last chunk) (inc depth))
    :else nil))

;; TODO - can add a few more to these as errors get picked up down the line
(def terminal-fns #{"checkpoint" "complete-with-result" "continue-ability" "corp-install" "damage" "draw" "effect-completed"
                    "gain-credits" "gain-tags" "make-run" "reveal" "rez" "resolve-ability" "runner-install"
                    "trash" "trash-cards" "trigger-event-simult" "trigger-event-sync" "wait-for"})
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
             ;; note that cancel effects must always complete eids,
             ;; as there is no provision for async-checking them baked into the engine
             ;; this comment is valid as of Jan '25 -nbk
             (if (:cancel-effect mapped)
               (is-valid-chunk? (:cancel-effect mapped) :async)
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

(deftest async-test-if-block-is-correct?
  (let [c1 "{:async true :effect (req (if (some corp-installable-type? (:hand corp)) (continue-ability state side select-ability card nil) (damage state)))}"
        c2 "{:async true :effect (req (if-not (some corp-installable-type? (:hand corp)) (damage 2) (damage state side eid 1)))}"
        c3 "{:async true :effect (req (if-let (some corp-installable-type? (:hand corp)) (continue-ability state side select-ability card nil) (damage state side eid 1)))}"]
    (is (not (validate-chunk c1)) "If block C1 is picked up as being wrong (RHS does not complete)")
    (is (not (validate-chunk c2)) "If block C2 is picked up as being wrong (LHS does not complete)")
    (is (validate-chunk c3)       "If block C3 is picked up as being right (LHS and RHS both complete)")))

(deftest async-test-when-block-is-correct?
  (let [c1 "{:async true :effect (req (when x (effect-completed state side eid)))}"
        c2 "{:async true :effect (req (do (when x y) (effect-completed state side eid)))}"]
    (is (not (validate-chunk c1)) "When block C1 is picked up as being wrong (conditional may not complete)")
    (is (validate-chunk c2)       "When block C2 is picked up as being right (conditional does not block completion)")))

(deftest async-test-case-block-is-correct?
  (let [c1 "{:async true :effect (req (case x a (effect-completed state side eid) (system-msg state side \"whoops\")))}"
        c2 "{:async true :effect (req (case x a (system-msg state side \"whoops\") (effect-completed state side eid)))}"
        c3 "{:async true :effect (req (case x a (effect-completed state side eid) (effect-completed state side eid)))}"]
    (is (not (validate-chunk c1)) "Case block C1 is picked up as being wrong (terminal does not complete)")
    (is (not (validate-chunk c2)) "Case block C2 is picked up as being wrong (LHS does not complete)")
    (is (validate-chunk c3)       "Case block C3 is picked up as being right (LHS and terminal both complete)")))x

(deftest async-test-cond+-is-correct?
  (let [c1 "{:async true :effect (req (cond+ [a (damage state :runner)] [:else (effect-completed state side eid)]))}"
        c2 "{:async true :effect (req (cond+ [a (effect-completed state :runner eid)] [:else (damage state side)]))}"
        c3 "{:async true :effect (req (cond+ [a (effect-completed state :runner eid)] [:else (effect-completed state side eid)]))}"]
    (is (not (validate-chunk c1)) "Cond+ block C1 is picked up as being wrong (RHS does not complete)")
    (is (not (validate-chunk c2)) "Cond+ block C2 is picked up as being wrong (LHS does not complete)")
    (is (validate-chunk c3)       "Cond+ block C3 is picked up as being right (LHS and RHS both complete)")))
