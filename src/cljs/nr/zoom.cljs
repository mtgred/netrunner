(ns nr.zoom
  (:require
   [jinteki.settings :refer [zoom-default zoom-step zoom-min zoom-max]]
   [nr.appstate :refer [app-state]]
   [nr.local-storage :as ls]
   [nr.translations :refer [tr]]
   [reagent.core :as r]))

(def zoom-cursor (r/cursor app-state [:options :zoom]))

(defn- round2 [n] (/ (js/Math.round (* n 100)) 100))
(defn- clamp [n lo hi] (max lo (min hi n)))

(defn apply-zoom!
  "Emulate browser zoom by setting CSS `zoom` on <html>, plus an `--app-zoom` custom
   property and an `app-zoomed` class the stylesheets use to grow fixed `vh` heights
   by 1/zoom, since CSS `zoom` doesn't scale `vh` like native browser zoom does.
   At zoom-default all three are cleared so there's no trace left."
  [factor]
  (let [el (.-documentElement js/document)
        style (.-style el)
        classes (.-classList el)
        f (or factor zoom-default)]
    (if (== f zoom-default)
      (do
        (set! (.-zoom style) "")
        (.removeProperty style "--app-zoom")
        (.remove classes "app-zoomed"))
      (let [s (str f)]
        (set! (.-zoom style) s)
        (.setProperty style "--app-zoom" s)
        (.add classes "app-zoomed")))))

(defn set-zoom!
  [factor]
  (let [factor (-> factor round2 (clamp zoom-min zoom-max))]
    (swap! app-state assoc-in [:options :zoom] factor)
    (ls/save! "zoom" factor)
    (apply-zoom! factor)))

(defn current-zoom [] (or @zoom-cursor zoom-default))
(defn zoom-in!   [] (set-zoom! (+ (current-zoom) zoom-step)))
(defn zoom-out!  [] (set-zoom! (- (current-zoom) zoom-step)))
(defn reset-zoom! [] (set-zoom! zoom-default))

(defn zoom-controls
  []
  (let [factor (current-zoom)
        pct (str (js/Math.round (* factor 100)) "%")
        at-min? (<= factor zoom-min)
        at-max? (>= factor zoom-max)]
    [:div#zoom-controls.float-right
     [:a (merge {:title (tr [:nav_zoom-out "Zoom out"])}
                (if at-min? {:class "disabled"} {:on-click zoom-out!}))
      "−"] ;; U+2212 minus sign, slightly bigger than normal -, matches + width
     [:a {:title (tr [:nav_zoom-reset "Reset zoom"])
          :on-click reset-zoom!} pct]
     [:a (merge {:title (tr [:nav_zoom-in "Zoom in"])}
                (if at-max? {:class "disabled"} {:on-click zoom-in!}))
      "+"]]))
