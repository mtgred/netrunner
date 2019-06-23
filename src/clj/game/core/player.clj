(ns game.core.player)

(defrecord HandSize
  [base mod])

(defrecord Corp
  [user
   identity
   options
   deck
   deck-id
   hand
   discard
   scored
   rfg
   play-area
   servers
   click
   click-per-turn
   credit
   bad-publicity
   toast
   hand-size
   agenda-point
   agenda-point-req
   keep
   quote])

(defrecord Servers
  [hq rd archives])

(defrecord BadPublicity
  [base additional])

(defrecord Runner
  [user
   identity
   options
   deck
   deck-id
   hand
   discard
   scored
   rfg
   play-area
   rig
   toast
   click
   click-per-turn
   credit
   run-credit
   link
   tag
   memory
   hand-size
   agenda-point
   agenda-point-req
   hq-access
   rd-access
   rd-access-fn
   brain-damage
   keep
   quote])

(defrecord Rig
  [program resource hardware])

(defrecord Tags
  [base additional is-tagged])

(defrecord Memory
  [base mod used])
