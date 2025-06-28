(ns game.core.prompt-state)

(defn set-prompt-state
  ([state side]
   (let [current-prompt (first (get-in @state [side :prompt]))]
     (set-prompt-state state side current-prompt)))
  ([state side prompt]
   (swap! state assoc-in [side :prompt-state] prompt)))

(defn remove-from-prompt-queue
  [state side prompt]
  (swap! state update-in [side :prompt] (fn [pr] (remove #(= % prompt) pr)))
  (swap! state update-in [side :prompt] vec)
  (set-prompt-state state side))

(defn add-to-prompt-queue
  "Adds a newly created prompt to the current prompt queue"
  [state side prompt]
  (swap! state update-in [side :prompt] #(cons prompt %))
  (swap! state update-in [side :prompt] vec)
  (set-prompt-state state side))
