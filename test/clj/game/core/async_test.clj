(ns game.core.async-test
  (:require
   [clojure.test :refer :all]
   [clojure.java.io :as io]
   [instaparse.core :as insta]
   [clojure.string :as str]))

;; This is intended to be a (mostly) exhaustive test for if cards that are
;; marked as async to in-fact complete eids, and if cards that are not
;; are also correct. I'm aiming to be accurate, and I think it's pretty much there.
;;
;; I currently don't have support for macros (mainly, just the tokens).
;;
;; If something is incorrect, you can prevent it being evaluated by adding
;; the following metadata to a card-def: ^:ignore-async-check
;; like: (defcard "Fall Guy" ^:ignore-async-check {:effect (req (do-something-cool))})
;;
;; There's a list of 'safe fns' (right-most rns that can contain an eid) and
;; terminal fns. If something gets caught out at some point in the future, it probably
;; means that one of these needs to be updated.
;;
;; --nbk, Jan 2025

(def card-base-str "src/clj/game/cards/")
(def relevant-cards-files ["basic.clj" "agendas.clj" "assets.clj" "events.clj" "hardware.clj"
                           "ice.clj" "identities.clj" "operations.clj" "programs.clj"
                           "resources.clj" "upgrades.clj"])

(def clojure-grammar
  (insta/parser
    "clojure  = form+
    <form>      = <ws> (anon-fn | fn | list | vector | map | set | symbol | literal | metadata | comment) <ws>
    fn          = <'(' ws> form* <ws ')'>
    <anon-fn>   = <ws '#'> fn
    list        = <ws '\\''> fn
    vector      = <'[' ws> form* <ws ']'>
    map         = <'{' ws> (form form)* <'}'>
    set         = <'#{' ws> form* <'}'>
    comment     = <'#_' form>
    <symbol>    = '@'? identifier
    <identifier>  = #'[\\'&%a-zA-Z_+\\-*/<>=?!\\.][%a-zA-Z0-9_+\\-*/<>=?!\\.]*'
    (* throw away the content of everything except for keywords and strings *)
    <literal>   = number | string | truthy | keyword | character
    number      = <#'-?[0-9]+'>
    string      = <'#'?> #'\"([^\\\"\\\\]|\\\\.)*\"'
    truthy      = <'true' | 'false' | 'nil'>
    keyword     = <':'> identifier
    (* I doubt we will ever use any of these, but it can't hurt *)
    character   = <#'\\\\[a-zA-Z0-9]'
                 | #'\\\\newline'
                 | #'\\\\space'
                 | #'\\\\tab'
                 | #'\\\\backspace'
                 | #'\\\\formfeed'
                 | #'\\\\return'>
    (*TODO: allow specifying 'ignore-async-check' in metadata*)
    metadata    = <'^'> form form
    (* handle whitespace*)
    <ws>        = <#'[\\s,\\n]+'?> "))

(defn- stitch-and-split-card-files
  [file]
  (let [split-file (str/split file #"(?=\(def)")
        restitch-fn (fn [chunk]
                      (let [lines (str/split-lines chunk)
                            ;; special case specifically for the hydra subs, which have semicolons,
                            ;; and TLDR, which contains a semicolon in the title
                            sans-comments (map first (map #(str/split % #"(?<!(tagged)|(TL));" 2) lines))]
                        (str/join "\n" sans-comments)))]
    (map restitch-fn split-file)))

(defn- get-fn-name
  "extracts a function name from a parsed segment of code"
  [parsed]
  (let [[t s :as ide] (nth (if (= :clojure (first parsed)) (second parsed) parsed) 2)]
    (if (= :string t) s
        ide)))

(defn- assemble-keywords
  "convert chunks into keywords where appropriate"
  [[sig kw :as chunk]]
  (if (= :keyword sig) (keyword kw) chunk))

(defn- bank-fn!
  "if the func is a let, or letfn, bank it in memory to refer to later down the line"
  [[_ sig bindings :as chunk] memory]
  (cond
    (= sig "let")
    (doseq [[k rhs] (partition 2 (rest bindings))]
      (when (string? k) (swap! memory assoc k rhs)))
    (= sig "letfn")
    (doseq [[_ k _ rhs] (rest bindings)]
      (when (string? k) (swap! memory assoc k rhs)))))

;; TODO - add more things as needed, if issues arise
(def terminal-fns
  "functions which should complete an eid, or indicate one needs to be completed"
  #{"checkpoint" "complete-with-result" "continue-ability" "corp-install"
    "damage" "draw" "effect-completed" "gain-credits" "gain-tags" "make-run"
    "reveal" "rez" "resolve-ability" "runner-install"
    "trash" "trash-cards" "trigger-event-simult" "trigger-event-sync" "wait-for"})

(def safe-fns
  "functions which probably contain an eid, but do not complete it"
  #{"can-pay?" "cost-value"})

(defn- contains-eid?
  [chunk depth]
  (some #(cond
           (string? %) (= % "eid")
           (not (vector? %)) nil
           (= (second %) "make-eid") true
           (contains? safe-fns (second %)) nil
           (contains? #{"assoc" "assoc-in"} (second %)) (contains-eid? % (inc depth))
           (zero? depth) (contains-eid? % 1)
           :else nil)
        chunk))

(defn- completes-eid?
  [[sig ide :as chunk] memory depth]
  (cond
    ;; if we're referring to a banked function or map, check that completes
    (and (string? chunk) (contains? @memory chunk) (< depth 15))
    (completes-eid? (get @memory chunk) memory (inc depth))
    ;; if it's not a function, it doesn't complete
    (not= sig :fn) nil
    ;; referring to a pre-deffed fn
    (and ide (contains? @memory ide) (< depth 15))
    (completes-eid? (get @memory ide) memory (inc depth))
    ;; if it's a safe function, it does not complete
    (contains? safe-fns ide) nil
    ;; both sides of the ifn should complete
    (contains? #{"if" "if-not" "if-let"} ide)
    (let [[_ _ body lhs rhs] chunk]
      (and (completes-eid? lhs memory (inc depth)) (completes-eid? rhs memory (inc depth))))
    ;; `when ... complete` is a bad pattern, and leaves us open to unclosed eids
    (contains? #{"when" "when-not" "when-let"} ide) nil
    ;; cond - every RHS element completes
    (= ide "cond")
    (let [assignments (take-nth 2 (drop 3 chunk))]
      (every? #(completes-eid? % memory (inc depth)) assignments))
    ;; condp - every RHS, and the terminal element, complete
    (= ide "condp")
    (let [assignments (concat (take-nth 2 (drop 5 chunk)) [(last chunk)])]
      (every? #(completes-eid? % memory (inc depth)) assignments))
    ;; case - every RHS, and the terminal element, complete
    (= ide "case")
    (let [assignments (concat (take-nth 2 (drop 4 chunk)) [(last chunk)])]
      (every? #(completes-eid? % memory (inc depth)) assignments))
    ;; cond+ - every RHS element of the leaves completes
    (= ide "cond+")
    (let [assignments (map last (drop 2 chunk))]
      (every? #(completes-eid? % memory (inc depth)) assignments))
    ;; regular fn which contains an eid, or continue-abi
    (or (= ide "continue-ability") (contains-eid? chunk 0))
    :maybe
    ;; leftover fn - check the RHS member completes
    :else (and (> (count chunk) 2) (completes-eid? (last chunk) memory (inc depth)))))

(defn- should-be-async?
  "should a chunk (probably) complete an eid?"
  [[sig ide :as chunk] memory depth]
  (cond
    (>= depth 15) nil
    (contains? @memory chunk) (should-be-async? (get @memory chunk) memory (inc depth))
    (and ide (contains? @memory ide)) (should-be-async? (get @memory ide) memory (inc depth))
    (= :fn sig) (or (contains? terminal-fns ide)
                    (some #(should-be-async? % memory (inc depth)) (drop 2 chunk)))))

(defn- read-metadata
  [[_ meta body :as metadata]]
  (when-not (= meta [:keyword "ignore-async-check"])
    body))

(defn- is-valid-chunk?
  "checks if a chunk of code is 'valid' in terms of sync/async classification.
   This is intended to be at least 99% accurate, but if something is getting missed,
   you can tag it with the metadata ^{:ignore-async-check true} and it will be ignored!"
  ([chunk] (is-valid-chunk? chunk (atom {})))
  ([[sig :as chunk] memory]
   (when (= :fn sig) (bank-fn! chunk memory))
   (cond
     (contains? #{:string :keyword :number :character :truthy :list :comment} sig) :fine
     (string? chunk) :fine
     (contains? #{:fn :vector :set} sig) (every? #(is-valid-chunk? % memory) (rest chunk))
     ;; maps -> require more complicated logic
     (= :map sig) (is-valid-chunk? (rest chunk) memory :map)
     ;; note, metadata can signify that a function does not need to be checked
     (= :metadata sig)
     (if-let [next-chunk (read-metadata chunk)] (is-valid-chunk? next-chunk memory) :fine)
     :else nil))
  ([chunk memory sig]
   (cond
     (= sig :map)
     (let [keypairs (partition 2 chunk)
           mapped (zipmap (map (comp assemble-keywords first) keypairs) (map second keypairs))]
       (and
         (if (:effect mapped)
           (if (:async mapped)
             (is-valid-chunk? (:effect mapped) memory :async)
             (is-valid-chunk? (:effect mapped) memory :sync))
           true)
         (if (:cancel-effect mapped)
           (is-valid-chunk? (:cancel-effect mapped) memory :async)
           true)
         (if-not (or (:effect mapped) (:cancel-effect mapped))
           (every? #(is-valid-chunk? % memory) (map second keypairs))
           true)))
     ;; things that are async should be completing eids
     (= sig :async) (and (completes-eid? chunk memory 0)
                         (is-valid-chunk? chunk memory))
     (= sig :sync) (and (not (should-be-async? chunk memory 0))
                        (not (completes-eid? chunk memory 0))
                        (is-valid-chunk? chunk memory))
     :else true)))

(defn- invalid-chunk?
  [chunk]
  (let [[sig body :as parsed] (clojure-grammar chunk)]
    (if-not (is-valid-chunk? body)
      (get-fn-name body)
      nil)))

(deftest cards-are-async-test
  (doseq [fname (take 15 relevant-cards-files)]
    (let [f (slurp (str card-base-str fname))
          chunks (rest (stitch-and-split-card-files f))]
      (let [invalids (->> chunks (map invalid-chunk?) (filterv identity))]
        (is (empty? invalids)
            (str "the following definitions in " fname
                 " may have sync/async issues: " (str/join ", " invalids)))))))

(deftest metadata-ignore-works
  (let [c1 "(defcard \\a ^:ignore-async-check {:async true :effect (req nil)})"
        c2 "(defcard \\b {:async true :effect (req nil)})"]
    (is (not (invalid-chunk? c1)) "c1 is valid because we ignore the async check")
    (is (invalid-chunk? c2) "c2 is invalid because we do not ignore the async check")))

(deftest async-test-defferred-fns-are-correct
  (let [c1 "(defcard \"c1\" (let [x (req (do-something state side eid))] {:async true :effect x}))"
        c2 "(defcard \"c2\" (let [x (req (do-something state side nil))] {:async true :effect x}))"
        c3 "(defcard \"c3\" (let [x (req (do-something state side eid))] {:effect x}))"
        c4 "(defcard \"c4\" (let [x (req (do-something state side nil))] {:effect x}))"
        c5 "(defcard \"c5\" (letfn [(x [] (req (do-something state side eid)))] {:async true :effect (x)}))"
        c6 "(defcard \"c6\" (letfn [(x [] (req (do-something state side nil)))] {:async true :effect (x)}))"
        c7 "(defcard \"c7\" (letfn [(x [] (req (do-something state side eid)))] {:effect (x)}))"
        c8 "(defcard \"c8\" (letfn [(x [] (req (do-something state side nil)))] {:effect (x)}))"]
    (is (not (invalid-chunk? c1)) "deffered block c1 is picked up as being correct (x completes)")
    (is (invalid-chunk? c2) "deffered block c2 is picked up as being wrong (x should complete)")
    (is (invalid-chunk? c3) "deffered block c3 is picked up as being wrong (x should complete)")
    (is (not (invalid-chunk? c4)) "deffered block c4 is picked up as being correct (x should not complete)")
    (is (not (invalid-chunk? c5)) "deffered block c5 is picked up as being correct (x completes)")
    (is (invalid-chunk? c6) "deffered block c6 is picked up as being wrong (x should complete)")
    (is (invalid-chunk? c7) "deffered block c7 is picked up as being wrong (x is not async, but should be)")
    (is (not (invalid-chunk? c8)) "deffered block c8 is picked up as being correct (x should not complete)")))

(deftest async-test-if-block-is-correct?
  (let [c1 (invalid-chunk? "(defcard \"c1\" {:async true :effect (req (if (some corp-installable-type? (:hand corp)) (continue-ability state side select-ability card nil) (damage state)))})")
        c2 (invalid-chunk? "(defcard \"c2\" {:async true :effect (req (if-not (some corp-installable-type? (:hand corp)) (damage 2) (damage state side eid 1)))})")
        c3 (invalid-chunk? "(defcard \"c3\" {:async true :effect (req (if-let (some corp-installable-type? (:hand corp)) (continue-ability state side select-ability card nil) (damage state side eid 1)))})")]
    (is c1       "If block C1 is picked up as being wrong (RHS does not complete)")
    (is c2       "If block C2 is picked up as being wrong (LHS does not complete)")
    (is (not c3) "If block C3 is picked up as being right (LHS and RHS both complete)")))

(deftest async-test-when-block-is-correct?
  (let [c1 "(defcard \"c1\" {:async true :effect (req (when x (do-something state side eid)))})"
        c2 "(defcard \"c2\" {:async true :effect (req (do (when x y) (do-something state side eid)))})"]
    (is (invalid-chunk? c1)       "When block C1 is picked up as being wrong (conditional may not complete)")
    (is (not (invalid-chunk? c2)) "When block C2 is picked up as being right (conditional does not block completion)")))

(deftest async-test-case-block-is-correct?
  (let [c1 "(defcard \"c1\" {:async true :effect (req (case x a (do-something state side eid) (system-msg state side \"whoops\")))})"
        c2 "(defcard \"c2\" {:async true :effect (req (case x a (system-msg state side \"whoops\") (do-something state side eid)))})"
        c3 "(defcard \"c3\" {:async true :effect (req (case x a (do-thing state side eid) (do-something state side eid)))})"]
    (is (invalid-chunk? c1)       "Case block C1 is picked up as being wrong (terminal does not complete)")
    (is (invalid-chunk? c2)       "Case block C2 is picked up as being wrong (LHS does not complete)")
    (is (not (invalid-chunk? c3)) "Case block C3 is picked up as being right (LHS and terminal both complete)")))

(deftest async-test-cond+-is-correct?
  (let [c1 "(defcard \"c1\" {:async true :effect (req (cond+ [a (damage state :runner)] [:else (do-something state side eid)]))})"
        c2 "(defcard \"c2\" {:async true :effect (req (cond+ [a (do-something state :runner eid)] [:else (damage state side)]))})"
        c3 "(defcard \"c3\" {:async true :effect (req (cond+ [a (do-something state :runner eid)] [:else (do-something state side eid)]))})"]
    (is (invalid-chunk? c1)       "Cond+ block C1 is picked up as being wrong (RHS does not complete)")
    (is (invalid-chunk? c2)       "Cond+ block C2 is picked up as being wrong (LHS does not complete)")
    (is (not (invalid-chunk? c3)) "Cond+ block C3 is picked up as being right (LHS and RHS both complete)")))
