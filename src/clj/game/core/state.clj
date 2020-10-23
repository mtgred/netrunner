(ns game.core.state)

(defrecord State
  [active-player
   bonus
   click-state
   corp
   effects
   effect-completed
   eid
   end-run
   end-time
   end-turn
   events
   gameid
   log
   loser
   losing-deck-id
   losing-user
   options
   per-run
   per-turn
   psi
   queued-events
   reason
   rid
   room
   run
   runner
   sfc-current-id
   sfx
   sfx-current-id
   stack
   stats
   trace
   trash
   turn
   turn-events
   turn-state
   typing
   winner
   winning-deck-id
   winning-user])

(defn make-rid
  "Returns a progressively-increasing integer to identify a new remote server."
  [state]
  (get-in (swap! state update-in [:rid] inc) [:rid]))

(defn new-state
  [gameid room now spectatorhands corp runner]
  (map->State
    {:gameid gameid :log [] :active-player :runner :end-turn true
     :room room
     :rid 0 :turn 0 :eid 0
     :sfx [] :sfx-current-id 0
     :stats {:time {:started now}}
     :options {:spectatorhands spectatorhands}
     :corp corp
     :runner runner}))
