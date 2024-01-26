(ns web.user-test
  (:require
   [clojure.test :refer [deftest is]]
   [web.user :refer :all]))

(deftest valid-username?-test
  (is (valid-username? "test"))
  (is (not (valid-username? "abcdefghijklmnopqrstu")) "Username is invalid when more than 20 characters")
  (is (valid-username? "abcdefghijklmnopqrst") "Username is valid when less than or equal to 20 characters")
  (is (valid-username? "🐸") "Emoji are valid in usernames")
  (is (valid-username? "🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸🐸") "Make sure 20 glyphs is still valid")
  (is (valid-username? "<script src=\"t.js\"") "HTML-like text is valid")
  (is (not (valid-username? "<h1>hello</h1>")) "HTML is not valid")
  (is (not (valid-username? "http://example.org")) "URLs are not valid"))

