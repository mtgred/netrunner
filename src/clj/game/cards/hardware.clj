(ns game.cards.hardware
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [jinteki.utils :refer [str->int other-side]]
            [jinteki.cards :refer [all-cards]]))
