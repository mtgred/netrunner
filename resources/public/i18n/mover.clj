(ns public.i18n.mover
  (:require
   [babashka.fs :as fs]))

(doseq [f (fs/glob "." "**.ftl" {:recursive false})
        :let [d (fs/strip-ext f)]]
  (fs/move f (fs/file d "ui.ftl") {:replace-existing true})
  )
