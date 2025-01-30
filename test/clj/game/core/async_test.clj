(ns game.core.async-test
  (:require
   [clojure.test :refer :all]
   [clojure.java.io :as io]
   [instaparse.core :as insta]
   [clojure.string :as str]))

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
    metadata    = <'^'> map form
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
    (mapv restitch-fn split-file)))

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

(defn- contains-eid?
  [chunk depth]
  (some #(cond
           (string? %) (= % "eid")
           (and (vector? %) (= (second %) "make-eid")) true
           (and (vector? %) (= (second %) "assoc")) (contains-eid? % (inc depth))
           (and (vector? %) (zero? depth)) (contains-eid? % 1)
           :else nil)
        chunk))

(defn- completes-eid?
  [[sig ide :as chunk] memory depth]
  (cond
    ;; if we're referring to a banked function or map, check that completes
    (and (string? chunk) (contains? @memory chunk) (< depth 15))
    (completes-eid? (get @memory chunk) memory (inc depth))
    ;; if it's not a function, it doesn't complete
    (not= sig :fn) (println "does not complete: " chunk)
    (and ide (contains? @memory ide) (< depth 15))
    (completes-eid? (get @memory ide) memory (inc depth))
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
    :else (and (> (count chunk) 2) (completes-eid? (last chunk) memory (inc depth)))
    ))

(defn- is-valid-chunk?
  "checks if a chunk of code is 'valid' in terms of sync/async classification.
   This is intended to be at least 99% accurate, but if something is getting missed,
   you can tag it with the metadata ^{:ignore-async-check true} and it will be ignored!"
  ([chunk] (is-valid-chunk? chunk (atom {})))
  ([[sig :as chunk] memory]
   (when (= :fn sig) (bank-fn! chunk memory))
   (println "chunk: " chunk)
   ;; (println "memory: " memory)
   (cond
     (contains? #{:string :keyword :number :character :truthy :list :comment} sig) :fine
     (string? chunk) :fine
     (contains? #{:fn :vector :set} sig) (every? #(is-valid-chunk? % memory) (rest chunk))
     ;; maps -> require more complicated logic
     (= :map sig) (is-valid-chunk? (rest chunk) memory :map)
     :else (do (println "invalid chunk: " chunk)
               nil)))
  ([chunk memory sig]
   (println "sig: " sig " - mchunk: " chunk)
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
     (= sig :async) (and (completes-eid? chunk memory 0) (is-valid-chunk? chunk memory))
     (= sig :sync)
     true)))

(deftest cards-are-async-test
  (doseq [fname (take 15 relevant-cards-files)]
    (let [f (slurp (str card-base-str fname))
          chunks (rest (stitch-and-split-card-files f))
          name-if-invalid (fn [parsed] (when-not (is-valid-chunk? parsed) (get-fn-name parsed)))]
      ;; final form should look like this
      ;; (doseq [c (rest chunks)]
      ;;   ;;(println c)
      ;;   (let [parsed (clojure-grammar c)]
      ;;     (println parsed)
      ;;     (is (not ((comp name-if-invalid second) parsed)) (str "wrong: " parsed)))))))
      (let [invalids (->> chunks
                          (map (comp name-if-invalid second clojure-grammar))
                          (filterv identity))]
        (is (empty? invalids)
            (str "the following definitions in " fname
                 " may have sync/async issues: " (str/join ", " invalids)))))))

;; ;; TODO - can add a few more to these as errors get picked up down the line
;; (def terminal-fns #{"checkpoint" "complete-with-result" "continue-ability" "corp-install" "damage" "draw" "effect-completed"
;;                     "gain-credits" "gain-tags" "make-run" "reveal" "rez" "resolve-ability" "runner-install"
;;                     "trash" "trash-cards" "trigger-event-simult" "trigger-event-sync" "wait-for"})
;; (defn should-complete?
;;   "Should a chunk (probably) complete an eid?"
;;   [chunk depth]
;;   (cond
;;     (and (string? chunk) (zero? depth)) nil
;;     (and (vector? chunk) (= 2 (count chunk)) (zero? depth)) nil
;;     (and (vector? chunk) (= (first chunk) :FN))
;;     (let [func-name (second chunk)]
;;       (if (contains? terminal-fns func-name)
;;         true
;;         (some #(should-complete? % (inc depth)) (rest (rest chunk)))))
;;     :else nil))

;; (defn is-valid-chunk?
;;   ([chunk]
;;    (cond
;;      (not (sequential? chunk)) true
;;      (= :FN (first chunk)) (every? is-valid-chunk? (rest chunk))
;;      (= :VEC (first chunk)) (every? is-valid-chunk? (rest chunk))
;;      (= :SET (first chunk)) (every? is-valid-chunk? (rest chunk))
;;      (= [:STR] chunk) true
;;      (= :MAP (first chunk)) (is-valid-chunk? (rest chunk) :MAP)
;;      (sequential? chunk) (every? is-valid-chunk? chunk)
;;      :else true))
;;   ([chunk conditional?]
;;    (cond
;;      (= conditional? :MAP)
;;      (do (let [keypairs (partition 2 chunk)
;;                mapped (zipmap (map first keypairs) (map second keypairs))]
;;            ;; if it contains an :effect, then:
;;            ;;   see if it contains :async true. If it does, effect must complete eid
;;            ;;   if it does not, effect should not complete the eid
;;            (and
;;              (if (:effect mapped)
;;                (if (:async mapped)
;;                  (is-valid-chunk? (:effect mapped) :async)
;;                  (is-valid-chunk? (:effect mapped) :sync))
;;                (every? is-valid-chunk? (vals mapped)))
;;              ;; note that cancel effects must always complete eids,
;;              ;; as there is no provision for async-checking them baked into the engine
;;              ;; this comment is valid as of Jan '25 -nbk
;;              (if (:cancel-effect mapped)
;;                (is-valid-chunk? (:cancel-effect mapped) :async)
;;                true))))
;;      (= conditional? :async)
;;      (and (completes? chunk 0) (is-valid-chunk? chunk))
;;      (= conditional? :sync)
;;      (and (not (should-complete? chunk 0)) (is-valid-chunk? chunk)))))

;; (defn clean-chunks
;;   "remove the empties and nils, and swaps keywords in"
;;   [chunks]
;;   (cond
;;     (= "" chunks) nil
;;     (= "true" chunks) true
;;     (= "nil" chunks) :nil
;;     (and (string? chunks) (str/starts-with? chunks ":")) (keyword (subs chunks 1))
;;     (sequential? chunks) (filterv identity (map clean-chunks chunks))
;;     :else chunks))

;; (defn validate-chunk [chunk] (->> chunk scuffed-grammar clean-chunks is-valid-chunk?))

;; (deftest cards-are-async-test
;;   (doseq [fname relevant-cards-files]
;;     (let [f (slurp (str card-base-str fname))
;;           chunks (stitch-and-split-card-files f)]
;;       (let [invalid-chunks (filter (complement validate-chunk) chunks)
;;             titles (map #(re-find #" \".+?\"" %) invalid-chunks)]
;;         (when (seq titles)
;;           (is nil (str "The following cards/fns in file '" fname "' may be invalid (async/sync): " (str/join ", " titles))))))))

;; (deftest async-test-if-block-is-correct?
;;   (let [c1 "{:async true :effect (req (if (some corp-installable-type? (:hand corp)) (continue-ability state side select-ability card nil) (damage state)))}"
;;         c2 "{:async true :effect (req (if-not (some corp-installable-type? (:hand corp)) (damage 2) (damage state side eid 1)))}"
;;         c3 "{:async true :effect (req (if-let (some corp-installable-type? (:hand corp)) (continue-ability state side select-ability card nil) (damage state side eid 1)))}"]
;;     (is (not (validate-chunk c1)) "If block C1 is picked up as being wrong (RHS does not complete)")
;;     (is (not (validate-chunk c2)) "If block C2 is picked up as being wrong (LHS does not complete)")
;;     (is (validate-chunk c3)       "If block C3 is picked up as being right (LHS and RHS both complete)")))

;; (deftest async-test-when-block-is-correct?
;;   (let [c1 "{:async true :effect (req (when x (effect-completed state side eid)))}"
;;         c2 "{:async true :effect (req (do (when x y) (effect-completed state side eid)))}"]
;;     (is (not (validate-chunk c1)) "When block C1 is picked up as being wrong (conditional may not complete)")
;;     (is (validate-chunk c2)       "When block C2 is picked up as being right (conditional does not block completion)")))

;; (deftest async-test-case-block-is-correct?
;;   (let [c1 "{:async true :effect (req (case x a (effect-completed state side eid) (system-msg state side \"whoops\")))}"
;;         c2 "{:async true :effect (req (case x a (system-msg state side \"whoops\") (effect-completed state side eid)))}"
;;         c3 "{:async true :effect (req (case x a (effect-completed state side eid) (effect-completed state side eid)))}"]
;;     (is (not (validate-chunk c1)) "Case block C1 is picked up as being wrong (terminal does not complete)")
;;     (is (not (validate-chunk c2)) "Case block C2 is picked up as being wrong (LHS does not complete)")
;;     (is (validate-chunk c3)       "Case block C3 is picked up as being right (LHS and terminal both complete)")))

;; (deftest async-test-cond+-is-correct?
;;   (let [c1 "{:async true :effect (req (cond+ [a (damage state :runner)] [:else (effect-completed state side eid)]))}"
;;         c2 "{:async true :effect (req (cond+ [a (effect-completed state :runner eid)] [:else (damage state side)]))}"
;;         c3 "{:async true :effect (req (cond+ [a (effect-completed state :runner eid)] [:else (effect-completed state side eid)]))}"]
;;     (is (not (validate-chunk c1)) "Cond+ block C1 is picked up as being wrong (RHS does not complete)")
;;     (is (not (validate-chunk c2)) "Cond+ block C2 is picked up as being wrong (LHS does not complete)")
;;     (is (validate-chunk c3)       "Cond+ block C3 is picked up as being right (LHS and RHS both complete)")))




;; [:fn defcard
;;  [:string "Deuces Wild"]
;;  [:fn let
;;   [:vector
;;    all [:vector
;;         [:map
;;          [:keyword effect] [:fn effect [:fn gain-credits eid [:number]]]
;;          [:keyword async] [:truthy]
;;          [:keyword msg] [:string "gain 3 [Credits]"]]
;;         [:map
;;          [:keyword async] [:truthy]
;;          [:keyword effect] [:fn effect [:fn draw eid [:number]]]
;;          [:keyword msg] [:string "draw 2 cards"]]
;;         [:map
;;          [:keyword async] [:truthy]
;;          [:keyword effect] [:fn effect [:fn lose-tags eid [:number]]]
;;          [:keyword msg] [:string "remove 1 tag"]]
;;         [:map
;;          [:keyword prompt] [:string "Choose 1 piece of ice to expose"]
;;          [:keyword msg] [:string "expose 1 ice and make a run"]
;;          [:keyword choices] [:map
;;                              [:keyword card] [:fn and [:fn installed? %] [:fn ice? %]]]
;;          [:keyword async] [:truthy]
;;          [:keyword effect] [:fn req
;;                             [:fn wait-for
;;                              [:fn expose state side target]
;;                              [:fn continue-ability
;;                               state side
;;                               [:map
;;                                [:keyword prompt] [:string "Choose a server"]
;;                                [:keyword choices] [:fn req runnable-servers]
;;                                [:keyword async] [:truthy]
;;                                [:keyword effect] [:fn effect [:fn make-run eid target]]]
;;                               card [:truthy]]]]
;;          [:keyword cancel-effect] [:fn effect
;;                                    [:fn continue-ability
;;                                     [:map
;;                                      [:keyword prompt] [:string "Choose a server"]
;;                                      [:keyword choices] [:fn req runnable-servers]
;;                                      [:keyword async] [:truthy]
;;                                      [:keyword effect] [:fn effect [:fn make-run eid target]]]
;;                                     card [:truthy]]]]]
;;    choice [:fn fn
;;            choice [:vector abis] [:map
;;                                   [:keyword prompt] [:string "Choose an ability to resolve"]
;;                                   [:keyword choices] [:fn map [:fn capitalize [:fn [:keyword msg] %]] abis]
;;                                   [:keyword async] [:truthy]
;;                                   [:keyword effect] [:fn req [:fn let [:vector chosen [:fn some [:fn when [:fn = target [:fn capitalize [:fn [:keyword msg] %]]] %] abis]]
;;                                                               [:fn wait-for
;;                                                                [:fn resolve-ability state side chosen card [:truthy]]
;;                                                                [:fn if
;;                                                                 [:fn = [:fn count abis] [:number]]
;;                                                                 [:fn continue-ability state side [:fn choice [:fn remove-once [:fn = % chosen] abis]] card [:truthy]]
;;                                                                 [:fn effect-completed state side eid]]]]]]]]
;;   [:map
;;    [:keyword on-play] [:map
;;                        [:keyword async] [:truthy]
;;                        [:keyword effect] [:fn effect [:fn continue-ability [:fn choice all] card [:truthy]]]]]]]
