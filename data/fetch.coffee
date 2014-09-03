fs = require('fs')
exec = require('child_process').exec
request = require('request')
mongoskin = require('mongoskin')

mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}")

setFields = [
  "name",
  "code",
  "total",
  "known",
  "available"
]

cardFields = [
  "code",
  "title",
  "type",
  "subtype",
  "text",
  "cost",
  "advancementcost",
  "agendapoints",
  "baselink",
  "influencelimit",
  "minimumdecksize",
  "faction",
  "factioncost", # influence
  "number",
  "quantity",
  "setname",
  "number",
  "cyclenumber",
  "side",
  "uniqueness",
  "memoryunits",
  "strength",
  "trash",
  "imagesrc"
]

baseurl = "http://netrunnerdb.com/api/"

restricted = ["Philotic Entanglement", "Eden Shard", "Eden Fragment"]

selectFields = (fields, objectList) ->
  ((fields.reduce ((newObj, key) -> newObj[key] = obj[key]; newObj["limit"] = 1 if obj.title in restricted; newObj), {}) for obj in objectList)

request.get baseurl + "sets", (error, response, body) ->
  if !error and response.statusCode is 200
    sets = selectFields(setFields, JSON.parse(body))
    # db.collection("sets").remove ->
    # db.collection('sets').insert sets, (err, result) ->
    fs.writeFile "sets.json", JSON.stringify(sets), ->
      # exec("mongoimport --db netrunner --upsert --upsertFields code --collection sets --jsonArray --file sets.json")
      exec("mongoimport --host $OPENSHIFT_MONGODB_DB_HOST --port $OPENSHIFT_MONGODB_DB_PORT --username $OPENSHIFT_MONGODB_DB_USERNAME --password $OPENSHIFT_MONGODB_DB_PASSWORD --db netrunner --upsert --upsertFields code --collection sets --jsonArray --file sets.json")

request.get baseurl + "cards", (error, response, body) ->
  if !error and response.statusCode is 200
    cards = selectFields(cardFields, JSON.parse(body))
    # db.collection("cards").remove ->
    # db.collection("cards").insert cards, (err, result) ->
    fs.writeFile "cards.json", JSON.stringify(cards), ->
      # exec("mongoimport --db netrunner --upsert --upsertFields code --collection cards --jsonArray --file cards.json")
      exec("mongoimport --host $OPENSHIFT_MONGODB_DB_HOST --port $OPENSHIFT_MONGODB_DB_PORT --username $OPENSHIFT_MONGODB_DB_USERNAME --password $OPENSHIFT_MONGODB_DB_PASSWORD --db netrunner --upsert --upsertFields code --collection cards --jsonArray --file cards.json")
