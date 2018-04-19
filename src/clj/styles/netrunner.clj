(ns styles.netrunner
  (:require [garden.def :refer [defstyles]]
            [styles.toastr :refer [toastr]]
            [styles.fonts :refer [fonts]]
            [styles.chat :refer [chat]]
            [styles.variables :as v]))

(defstyles netrunner
           fonts
           chat
           toastr)