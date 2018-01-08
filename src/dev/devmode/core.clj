(ns devmode.core
  (:require [devtools.core :as devtools]))

(devtools/install!)
(enable-console-print!)
(println "dev mode")