(ns ^:figwheel-no-load dev.nr
  (:require
    [nr.main :as main]
    [devtools.core :as devtools]))

(enable-console-print!)

(devtools/install!)

(main/init!)