## Profiling Database Queries

This document gives a short introduction on profiling database queries and
creating appropriate indexes. It is *purposely superficial*, for a deeper
understanding of the topic it is recommended to consult the [official
documentation](https://docs.mongodb.com/manual/core/crud/).

We'll combine two different approaches to identity slow and/or inefficient
queries:

1. Identifying slow queries by measuring their execution times on large
   collections, and
2. Analyzing these queries in a Mongo shell using `explain()`.


### Populating your database with sample data

The following command will create approx. 50k users, 550k decks, 250k game-logs
and 250k messages (taking a couple of minutes and using around 600MB of disk
space):

    $ lein create-sample-data


### Starting `mongod` with profiling enabled

    $ mongod --profile 1 --slowms 20


### Identifying slow queries

You can use `$ lein drop-indexes` to drop all created indexes to test logging
of slow queries. If you then open your local instance in a browser, `mongod`
should output messages like this one:

    2021-02-05T05:51:08.796+0100 I COMMAND  [conn5] command netrunner.messages command: find { find: "messages", filter: { channel: "general" }, sort: { date: -1 }, limit: 100, batchSize: 256 } planSummary: COLLSCAN keysExamined:0 docsExamined:250490 hasSortStage:1 cursorExhausted:1 numYields:1958 nreturned:100 reslen:16007 locks:{ Global: { acquireCount: { r: 3918 } }, Database: { acquireCount: { r: 1959 }, acquireWaitCount: { r: 1 }, timeAcquiringMicros: { r: 10475 } }, Collection: { acquireCount: { r: 1959 } } } protocol:op_query 718ms

`mongod` tells you the command that was slow,

    find {
        find: "messages",
        filter: { channel: "general" },
        sort: { date: -1 },
        limit: 100,
        batchSize: 256
    }

and some details about its plan and execution:

    planSummary: COLLSCAN
    keysExamined:0
    docsExamined:250490
    hasSortStage:1
    cursorExhausted:1
    numYields:1958
    nreturned:100
    reslen:16007

You'll want to look out for `COLLSCAN` and very large numbers in `docsExamined`
compared to the actual number of results (`nreturned`). Both point to a missing
index to support the query. Further details on reading the output can be found
at [Database Profiler
Output](https://docs.mongodb.com/manual/reference/database-profiler/).


### Analyzing the query and creating appropriate indexes

Hop in your nearest `mongo` shell and explain any query you suspect to be
inefficient:

    $ mongo netrunner
    > db.messages.find({ channel: "general" }).sort({ date: -1 }).limit(100).explain()
    {
        "queryPlanner" : {
            [...]
            "winningPlan" : {
                "stage" : "SORT",
                "sortPattern" : {
                    "date" : -1
                },
                "limitAmount" : 100,
                "inputStage" : {
                    "stage" : "SORT_KEY_GENERATOR",
                    "inputStage" : {
                        "stage" : "COLLSCAN",
                        "filter" : {
                            "channel" : {
                                "$eq" : "general"
                            }
                        },
                        "direction" : "forward"
                    }
                }
            },
            [...]
        },
        [...]
    }

This will print a detailed plan of how Mongo will execute this query. In
general, seeing `IXSCAN` is good, `COLLSCAN` is bad. See [Explain
Results](https://docs.mongodb.com/manual/reference/explain-results/) for
details.


### Creating appropriate indexes

Queries should usually have *one* index so support them. While Mongo can use
multiple indexes for one query, it is not recommended to rely on it (see [Index
Intersection](https://docs.mongodb.com/manual/core/index-intersection/) for
details).

In our example above, we can try if an index for `{ channel: 1 }` is sufficient:

    > db.messages.createIndex({ channel: 1 })
    > db.messages.find({ channel: "general" }).sort({ date: -1 }).limit(100).explain()
    {
        "queryPlanner" : {
            [...]
            "winningPlan" : {
                "stage" : "SORT",
                "sortPattern" : {
                        "date" : -1
                },
                "limitAmount" : 100,
                "inputStage" : {
                    "stage" : "SORT_KEY_GENERATOR",
                    "inputStage" : {
                        "stage" : "FETCH",
                        "inputStage" : {
                            "stage" : "IXSCAN",
                            [...]
                        }
                    }
                }
            },
            "rejectedPlans" : [ ]
        },
        [...]
    }

That looks better. The `COLLSCAN` is gone, meaning Mongo can use our index to
fetch matching documens. However, including the `"executionStats"` we will see
that this is still inefficient:

    > db.messages.find({ channel: "general" }).sort({ date: -1 }).limit(100).explain("executionStats")
    {
        [...]
        "executionStats" : {
            "executionSuccess" : true,
            "nReturned" : 100,
            "executionTimeMillis" : 442,
            "totalKeysExamined" : 250490,
            "totalDocsExamined" : 250490,
            [...]
        }
        [...]
    }

Even with our new index, Mongo still has to sort more than 250k documents in
memory, returning only 100 of them. We'll refine our index to `{ channel: 1,
date: -1 }`:

    > db.messages.dropIndex({ channel: 1 })
    > db.messages.createIndex({ channel: 1, date: -1 })
    > db.messages.find({ channel: "general" }).sort({ date: -1 }).limit(100).explain("executionStats")
    {
        [...]
        "executionStats" : {
            "executionSuccess" : true,
            "nReturned" : 100,
            "executionTimeMillis" : 3,
            "totalKeysExamined" : 100,
            "totalDocsExamined" : 100,
            [...]
        }
        [...]
    }

Perfect. The execution time dropped from 442ms to 3ms and we're furthermore not
wasting CPU cycles and memory by sorting huge amounts of data.

You can now add this index to `indexes` in `src/clj/tasks/db.clj`. If there is
already an index for `{ channel: 1 }`, you can replace it with `{ channel: 1,
date: -1 }`, as the latter can also be used for all queries the former can
support. See [Compound
Indexes](https://docs.mongodb.com/manual/core/index-compound/) for details.


## Stress Testing

`bin/stress-test` reproduces server load from many simultaneous games so that
resource usage can be measured and compared across runs. It connects headless
bot players to a running server over real websockets (the same HTTP login,
lobby events, game actions, and state diffs a browser goes through), plays
full games using the built-in preconstructed Worlds decks, and samples the
server's CPU and memory over time.

Start the server stack first (`bin/up`), then:

    $ bin/stress-test

The defaults are sized for a local dev run (10 concurrent games for 3
minutes at a 1s think time). To reproduce tournament-scale load:

    $ bin/stress-test --concurrent-games 45 --duration-seconds 300

Inputs: `-n`/`--concurrent-games`, `-d`/`--duration-seconds`, `--delay`
per-bot think time in ms (lower = more load), `--matchups` to pick specific
preconstructed matchups, `--spectators` per game, `--save-replays` to make
games save replays at the end like tournament games do, `--chat-chance` to
have bots chat in-game, and `--max-blocks` to give players block lists naming
players of other games (exercising the lobby list filtering). See `--help`
for all options.

Outputs land in `stress-runs/<timestamp>/` (or `--out`): `samples.csv` with
per-second server CPU/memory, action throughput, and action-to-diff latency
percentiles; `summary.edn` with aggregates; `config.edn` with the run
configuration.

With `--profile`, the run also captures CPU profiles of the server JVM via
its nREPL (port 44867 in the docker setup, `--nrepl-port` to change) using
the clj-async-profiler already in the dev profile. Two phases are profiled
separately, since their signatures differ completely: `profile-ramp.*`
covers logins, lobby creation, and game starts, and `profile-steady.*`
covers the loaded steady state once all games are up (profiling stops before
teardown). Each phase produces three files in the output directory:

- `<phase>.html`: interactive flamegraph, best for humans (self-contained,
  regenerate any time from the collapsed file).
- `<phase>-collapsed.txt`: raw collapsed stacks, one `frame;frame;... count`
  line per unique stack. Greppable, diffable, loads into
  [speedscope](https://www.speedscope.app), and the best input for agents.
- `<phase>-summary.txt`: top frames by self and total time, for a quick look.

If the run ends before all games came up (short duration, or the server
buckling under the load), only the ramp profile exists and it covers the run
up to that point.

Inside docker, perf events are unavailable and the profiler falls back to
itimer sampling automatically; the event used is printed and recorded in the
summary file. To evaluate a change, run the same configuration on both
versions of the server and compare the summaries; bot decisions are seeded
per game slot, but engine randomness (shuffles, accesses) still varies, so
prefer several runs or longer durations over single short runs.

The bots decide from the same client-visible state a browser sees and follow
the same action locking, so they exercise the full production path per action:
engine execution, per-perspective state projections and diffs, JSON encoding,
and websocket sends, plus lobby churn as games finish and restart. Games write
normal rows to `game-logs`, so stress runs against a database you care about
will leave records.

