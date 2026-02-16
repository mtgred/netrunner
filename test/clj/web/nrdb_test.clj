(ns web.nrdb-test
  (:require
   [clojure.test :refer :all]
   [web.nrdb :refer :all]))

(deftest parse-input-test
  (let [parse-input @#'web.nrdb/parse-input]
    (testing "public decklist URL with slug"
      (is (= [:public "d8888676-f12e-4fa1-a531-9284a06a9ad0"]
             (parse-input "https://netrunnerdb.com/en/decklist/d8888676-f12e-4fa1-a531-9284a06a9ad0/tread-loudly-5th-at-worlds-2025"))))
    (testing "public decklist URL without slug"
      (is (= [:public "d8888676-f12e-4fa1-a531-9284a06a9ad0"]
             (parse-input "https://netrunnerdb.com/en/decklist/d8888676-f12e-4fa1-a531-9284a06a9ad0"))))
    (testing "public decklist URL with numeric id"
      (is (= [:public "92324"]
             (parse-input "https://netrunnerdb.com/en/decklist/92324"))))
    (testing "private deck URL"
      (is (= [:private "95db310b-79e7-4bf0-8e4e-e643cb7f6c95"]
             (parse-input "https://netrunnerdb.com/en/deck/view/95db310b-79e7-4bf0-8e4e-e643cb7f6c95"))))
    (testing "bare id"
      (is (= [:unknown "12345"]
             (parse-input "12345"))))))

(deftest readable-url-test
  (let [readable-url @#'web.nrdb/readable-url]
    (testing "public endpoint generates decklist URL"
      (is (= "https://netrunnerdb.com/en/decklist/92324"
             (readable-url :public "92324"))))
    (testing "private endpoint generates deck/view URL"
      (is (= "https://netrunnerdb.com/en/deck/view/95db310b-79e7-4bf0-8e4e-e643cb7f6c95"
             (readable-url :private "95db310b-79e7-4bf0-8e4e-e643cb7f6c95"))))
    (testing "unknown endpoint defaults to decklist URL"
      (is (= "https://netrunnerdb.com/en/decklist/12345"
             (readable-url :unknown "12345"))))))
