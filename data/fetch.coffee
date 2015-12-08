fs = require('fs')
exec = require('child_process').exec
request = require('request')
mongoskin = require('mongoskin')
mkdirp = require('mkdirp')
path = require('path')
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
  "limited"
]

baseurl = "http://netrunnerdb.com/api/"

selectFields = (fields, objectList) ->
  ((fields.reduce ((newObj, key) -> newObj[key] = obj[key] if typeof(obj[key]) isnt "undefined"; newObj), {}) for obj in objectList)

fetchSets = (callback) ->
  request.get baseurl + "sets", (error, response, body) ->
    if !error and response.statusCode is 200
      sets = selectFields(setFields, JSON.parse(body))
      db.collection("sets").remove ->
      db.collection("sets").insert sets, (err, result) ->
        fs.writeFile "andb-sets.json", JSON.stringify(sets), ->
          console.log("#{sets.length} sets fetched")
        callback(null, sets.length)

fetchImg = (code, imgPath, t) ->
  setTimeout ->
    console.log code
    url = "http://netrunnerdb.com/bundles/netrunnerdbcards/images/cards/en/#{code}.png"
    request(url).pipe(fs.createWriteStream(imgPath))
  , t

fetchCards = (callback) ->
  request.get baseurl + "cards", (error, response, body) ->
    if !error and response.statusCode is 200
      cards = selectFields(cardFields, JSON.parse(body))
      imgDir = path.join(__dirname, "..", "resources", "public", "img", "cards")
      mkdirp imgDir, (err) ->
        if err
          console.error("Failed to create card image resource directory #{imgDir}")
      i = 0
      for card in cards
        imgPath = path.join(imgDir, "#{card.code}.png")
        if !fs.existsSync(imgPath)
          fetchImg(card.code, imgPath, i++ * 200)

      db.collection("cards").remove ->
      db.collection("cards").insert cards, (err, result) ->
        fs.writeFile "andb-cards.json", JSON.stringify(cards), ->
          console.log("#{cards.length} cards fetched")
        callback(null, cards.length)

async.parallel [fetchSets, fetchCards], (error, results) ->
  db.close()
