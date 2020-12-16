(ns tasks.temp
  (:require [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]))

(defn choices-map-handler
  [zloc choices]
  (let [effect (z/get zloc :effect)
        req (z/get zloc :req)
        msg (z/get zloc :msg)
        async (z/get zloc :msg)
        prompt (z/get zloc :prompt)
        choices-map (cond-> choices
                      effect (z/assoc :effect (z/node effect))
                      req (z/assoc :req (z/node req))
                      msg (z/assoc :msg (z/node msg))
                      async (z/assoc :async true)
                      prompt (z/assoc :prompt (z/node prompt)))
        ability-map (z/edit zloc dissoc :effect :req :msg :async :prompt :choices)]
    (z/replace zloc (z/node (z/assoc ability-map :select (z/node choices-map))))))

(defn choices-list-handler
  [zloc choices]
  (let [effect (z/get zloc :effect)
        req (z/get zloc :req)
        msg (z/get zloc :msg)
        async (z/get zloc :msg)
        prompt (z/get zloc :prompt)
        _ (println (n/map-node nil))
        choices-map (cond-> (n/map-node nil)
                      choices (z/assoc :buttons (z/node choices))
                      effect (z/assoc :effect (z/node effect))
                      req (z/assoc :req (z/node req))
                      msg (z/assoc :msg (z/node msg))
                      async (z/assoc :async true)
                      prompt (z/assoc :prompt (z/node prompt)))
        ability-map (z/edit zloc dissoc :effect :req :msg :async :prompt)]
    (z/replace zloc (z/node (z/assoc zloc :choices (z/node choices-map)))))
  )

(defn parser
  [zloc]
  (loop [zloc zloc]
    (if (z/end? zloc)
      (z/root zloc)
      (if (z/map? zloc)
        (let [choices (z/get zloc :choices)]
          (cond
            (z/map? choices)
            (recur (z/next (choices-map-handler zloc choices)))
            (or (z/list? choices)
                (z/vector? choices))
            (recur (z/next (choices-list-handler zloc choices)))
            :else
            (recur (z/next zloc))))
        (recur (z/next zloc))))))
