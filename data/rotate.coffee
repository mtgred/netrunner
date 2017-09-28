mongoskin = require('mongoskin')
async = require('async')

mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}").open( (err, _) -> throw err if err )

rotate_cycle = process.argv[2]
rotate_value = process.argv[3] || true

rotateCycle = (callback) ->
  if (!rotate_cycle?)
    console.log("Usage: npm run rotate -- \"cycle_code\" [false]")
    console.log("Will set the rotated value of all packs and cards belonging to the cycle")
    console.log("Defaults to setting the rotated value to true, can be specified to set the value to false")
    callback(null, 0)
  else
    rotate_value = (rotate_value != 'false')
    console.log('Rotation Cycle:', rotate_cycle)
    console.log('Rotation Value:', rotate_value)
    db.collection('cycles').update({code: rotate_cycle}, {'$set': {rotated: rotate_value}}, (err, result) ->
      throw(err) if err
      console.log('Cycle Update:', result.result)
      db.collection('sets').update({cycle_code: rotate_cycle}, {'$set': {rotated: rotate_value}}, {multi: true},
        (err, result) ->
          throw(err) if err
          console.log('Sets Update:', result.result)
          db.collection('cards').update({cycle_code: rotate_cycle}, {'$set': {rotated: rotate_value}}, {multi: true},
            (err, result) ->
              throw(err) if err
              console.log('Cards Update:', result.result)
              callback(null, 0))
      )
    )

async.series [rotateCycle, () -> db.close()]
