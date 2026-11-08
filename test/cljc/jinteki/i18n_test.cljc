(ns jinteki.i18n-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [clojure.test.check.clojure-test :refer [defspec *report-completion*]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [game.core.schemas :as schemas]
   [jinteki.i18n :as sut]
   [malli.generator :as mg]))

(use-fixtures :each (fn [f]
                      (binding [*report-completion* false]
                        (sut/load-dictionary! "public/i18n")
                        (f))))

(deftest check-translations-test
  (is (empty? (sut/load-dictionary! "public/i18n"))))

(deftest msg-map-validates-keywords-test
  (is (= {:msg/type :msg_tie} (sut/->msg-map :msg_tie)))
  (is (thrown? #?(:clj Exception :cljs :default) (sut/->msg-map :not-a-registered-msg))))

(defn- ftl-ids
  [content]
  (map second (re-seq #"(?m)^([a-zA-Z][a-zA-Z0-9_-]*)\s*=" content)))

(deftest msg-registry-translations-test
  (let [ids (set (ftl-ids (sut/get-content "en")))]
    (doseq [k (keys @schemas/msg-registry)]
      (is (contains? ids (name k)) (str k " has no en translation")))))

(deftest no-numeric-variant-keys-test
  ;; the jvm fluent never matches numeric variant keys like [0], but the browser does.
  ;; e.g. with `{$n -> [0] no cards *[other] {$n} cards}` and n = 0,
  ;; the browser shows "no cards" but clj tests render "0 cards"
  (is (empty? (re-seq #"(?m)^\s+\*?\[-?\d+\]" (sut/get-content "en")))))

(defspec messages-can-be-translated-test 100
  (let [lang (delay nil)]
    (prop/for-all [msg (gen/fmap #(assoc % :username "Test") (mg/generator sut/UseCard))]
      (try (sut/format lang (:msg/type msg) (sut/prep-build-msg msg))
        (catch #?(:clj Throwable :cljs :default) t
          (prn t)
          nil)))))
