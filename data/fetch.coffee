fs = require('fs')
exec = require('child_process').exec
request = require('request')
mongoskin = require('mongoskin')
async = require('async')

mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}")

setFields = [
  "name",
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
  "setname",
  "side",
  "uniqueness",
  "memoryunits",
  "strength",
  "trash",
  "imagesrc"
]

baseurl = "http://netrunnerdb.com/api/"

restricted = ["Director Haas' Pet Project", "Philotic Entanglement", "Eden Shard", "Eden Fragment", "Hades Shard", "Hades Fragment", "Utopia Shard", "Uptopia Fragment", "Government Takeover"]

selectFields = (fields, objectList) ->
  ((fields.reduce ((newObj, key) -> newObj[key] = obj[key]; newObj["limit"] = 1 if obj.title in restricted; newObj), {}) for obj in objectList)

fetchSets = (callback) ->
  request.get baseurl + "sets", (error, response, body) ->
    if !error and response.statusCode is 200
      sets = selectFields(setFields, JSON.parse(body))
      db.collection("sets").remove ->
      db.collection("sets").insert sets, (err, result) ->
        fs.writeFile "andb-sets.json", JSON.stringify(sets), ->
          console.log("#{sets.length} sets fetched")
        callback(null, sets.length)

fetchImg = (code, t) ->
  setTimeout ->
    console.log code
    url = "http://netrunnerdb.com/web/bundles/netrunnerdbcards/images/cards/en/#{code}.png"
    imgDir = if mongoUser then "#{process.env.OPENSHIFT_DATA_DIR}/img" else "img"
    request(url).pipe(fs.createWriteStream("#{imgDir}/#{code}.png"))
  , t

fetchCards = (callback) ->
  request.get baseurl + "cards", (error, response, body) ->
    if !error and response.statusCode is 200
      cards = selectFields(cardFields, JSON.parse(body))
      i = 0
      for card in cards
        if card.imagesrc and !fs.existsSync("img/#{card.code}.png")
          fetchImg(card.code, i++ * 200)

      db.collection("cards").remove ->
      db.collection("cards").insert cards, (err, result) ->
        fs.writeFile "andb-cards.json", JSON.stringify(cards), ->
          console.log("#{cards.length} cards fetched")
        callback(null, cards.length)

async.parallel [fetchSets, fetchCards], (error, results) ->
  db.close()
