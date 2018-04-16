(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-utopia-shard
  {"Utopia Shard"
   (shard-constructor :hq "force the Corp to discard 2 cards from HQ at random"
                      (effect (trash-cards :corp (take 2 (shuffle (:hand corp))))))})
