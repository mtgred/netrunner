(ns jinteki.i18n-test
  (:require
   [clojure.test :refer [deftest is]]
   [game.core.schemas :as schemas]
   [jinteki.i18n :as sut]))

(deftest check-translations-test
  (is (empty? (sut/load-dictionary! "public/i18n"))))

(deftest msg-map-validates-keywords-test
  (is (= {:msg/type :tie} (sut/->msg-map :tie)))
  (is (thrown? Exception (sut/->msg-map :not-a-registered-msg))))

(defn- ftl-ids
  [content]
  (map second (re-seq #"(?m)^([a-zA-Z][a-zA-Z0-9_-]*)\s*=" content)))

(deftest msg-registry-translations-test
  (sut/load-dictionary! "public/i18n")
  (let [ids (set (ftl-ids (sut/get-content "en")))]
    (doseq [k (keys @schemas/msg-registry)]
      (is (contains? ids (name k)) (str k " has no en translation")))))

(deftest no-numeric-variant-keys-test
  ;; the jvm fluent never matches numeric variant keys like [0], but the browser does.
  ;; e.g. with `{$n -> [0] no cards *[other] {$n} cards}` and n = 0,
  ;; the browser shows "no cards" but clj tests render "0 cards"
  (sut/load-dictionary! "public/i18n")
  (is (empty? (re-seq #"(?m)^\s+\*?\[-?\d+\]" (sut/get-content "en")))))
