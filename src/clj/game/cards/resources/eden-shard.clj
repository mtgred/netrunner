(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-eden-shard
  {"Eden Shard"
   (shard-constructor :rd "force the Corp to draw 2 cards" (req (draw state :corp 2)))})