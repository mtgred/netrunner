(ns styles.chat
  (:require [garden.def :refer [defstyles defkeyframes]]
            [garden.selectors :as gs :refer [& nth-child]]
            [garden.units :refer [px]]))

(defn get-delay
  "Takes a time interval and generates it as a string"
  [i]
  (str (float (* 0.1 i)) "s"))

(defkeyframes blink
              [:0% {:opacity 0.2}]
              [:20% {:opacity 1}]
              [:100% {:opacity 0.2}])

(def animate-typing
  (for [i (range 1 11)]
    [(-> i str nth-child &) {:animation-delay (get-delay i)}]))

(defstyles chat
           [:.typing {:position "absolute"
                      :margin 0
                      :padding 0
                      :overflow "hidden"
                      :bottom 0
                      :left (px 1)}
            [:span {:animation-name "blink"
                    :animation-duration "1.5s"
                    :animation-iteration-count "infinite"
                    :animation-fill-mode "both"}
             animate-typing]]
           blink)
