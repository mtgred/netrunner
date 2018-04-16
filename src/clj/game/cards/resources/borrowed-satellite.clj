(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-borrowed-satellite
  {"Borrowed Satellite"
   {:in-play [:hand-size-modification 1 :link 1]}})