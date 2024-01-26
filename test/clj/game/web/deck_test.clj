(ns game.web.deck-test
  (:require
   [clojure.test :refer :all]
   [web.decks :refer :all]))

(deftest hash-salt
  (testing "Generate salt for hash from unicode string"
    (let [unicode-string "Hüsker Dü Аквариум"
          all-unicode-string "😀😀😀"
          ascii-string "Test Deck"]
      (is (java.util.Arrays/equals (make-salt unicode-string) (make-salt unicode-string)) "Unicode string salts match")
      (is (java.util.Arrays/equals (make-salt all-unicode-string) (make-salt all-unicode-string)) "Unicode string salts match")
      (is (java.util.Arrays/equals (make-salt ascii-string) (make-salt ascii-string)) "Ascii string salts match")
      (is (not (java.util.Arrays/equals (make-salt ascii-string) (make-salt unicode-string))) "Ascii and unicode salts don't match"))))
