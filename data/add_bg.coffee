mongoskin = require('mongoskin')
async = require('async')
fs = require('fs')
path = require('path')

mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}").open( (err, _) -> throw err if err )

remove_bg = (callback) ->
  console.log("Removing old backgrounds")
  db.collection("backgrounds").remove (err, result) ->
    throw(err) if err
    console.log(result.result)
    callback(null)

list_backgrounds = () ->
  imgDir = path.join(__dirname, "..", "resources", "public", "img", "bg")
  fs.readdirSync(imgDir)

add_bg = (callback) ->
  console.log("Adding backgrounds")
  bgs = list_backgrounds()
  async.eachSeries bgs, (bg, bg_callback) ->
    db.collection("backgrounds").insert {path: "/img/bg/#{bg}"}, (err, result) ->
      throw(err) if err
      console.log("Added #{bg}")
      bg_callback()
  , (bg_err) ->
    throw(bg_err) if bg_err
    callback(null)

async.series [remove_bg, add_bg, () -> db.close()]
