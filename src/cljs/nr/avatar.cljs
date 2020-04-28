(ns nr.avatar)

(defn avatar-impl [{:keys [emailhash username]} opts]
  (when emailhash
    [:img.avatar
     {:src (str "https://www.gravatar.com/avatar/" emailhash "?d=retro&s=" (get-in opts [:opts :size]))
      :alt username}]))

(def avatar (memoize avatar-impl))
