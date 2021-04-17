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
