mongoskin = require('mongoskin')
async = require('async')

mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}").open( (err, _) -> throw err if err )

add_art = (callback) ->
  db.collection('altarts').find().toArray (err, results) ->
    throw(err) if err
    console.log("Found " + results.length + " cards with art")
    async.each results, (result, inner_callback) ->
      if result
        db.collection('cards').update {code: result.code}, {'$set': {alt_art: result.versions}},
          (err, inner_result) ->
            throw(err) if err
            console.log("Updating " + result.code + " : ")
            console.log(inner_result.result)
            inner_callback()
    , (inner_err) ->
      throw(inner_err) if inner_err
    callback(null, 0)

async.series [add_art, () -> db.close()]
