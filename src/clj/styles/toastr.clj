(ns styles.toastr
  (:require [garden.def :refer [defstyles]]
            [garden.units :refer [px]]))

(defstyles toastr
           [:.toast-hand {:bottom (px 100)
                          :left (px 16)}]
           [:.toast-left-center {:bottom (px 320)
                                 :left (px 5)
                                 :width (px 60)}]
           [:.toast-card {:top (px 36)
                          :right (px 5)}])