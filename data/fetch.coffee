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

same = (key, t) ->
  return [key, t]

rename = (newkey) ->
  return (key, t) -> [newkey, t]

capitalize = (s) ->
  return s.charAt(0).toUpperCase() + s.substr(1)

setFields = {
  "name" : same
  "date_release" : rename("available")
  "cycle_code" : (k, t) -> ["cycle", capitalize(t)]
}

mapFactions = {
  "haas-bioroid" : "Haas-Bioroid",
  "jinteki" : "Jinteki",
  "nbn" : "NBN",
  "weyland-consortium" : "Weyland Consortium",
  "anarch" : "Anarch",
  "criminal" : "Criminal",
  "shaper" : "Shaper",
  "adam" : "Adam",
  "sunny-lebeau" : "Sunny Lebeau",
  "apex" : "Apex",
  "neutral-runner" : "Neutral",
  "neutral-corp" : "Neutral"
}

mapSets = {}

cardFields = {
  "code" : same,
  "title" : same,
  "type_code" : (k, t) -> ["type", if t is "ice" then "ICE" else capitalize(t)],
  "keywords": rename("subtype"),
  "text" : same,
  "cost" : (k, t) -> ["cost", if t is null then 0 else t],
  "advancement_cost" : rename("advancementcost"),
  "agenda_points" : rename("agendapoints"),
  "base_link" : rename("baselink"),
  "influence_limit" : rename("influencelimit"),
  "minimum_deck_size" : rename("minimumdecksize"),
  "faction_code" : (k, t) -> ["faction", mapFactions[t]],
  "faction_cost" : rename("factioncost"), # influence
  "position" : rename("number"),
  "pack_code": (k, t) -> ["setname", mapSets[t]]
  "side_code" : (k, t) -> ["side", capitalize(t)],
  "uniqueness" : same,
  "memory_cost" : rename("memoryunits"),
  "strength" : same,
  "trash_cost" : rename("trash"),
  "deck_limit" : rename("limited")
}

baseurl = "http://netrunnerdb.com/api/2.0/public/"

selectFields = (fields, objectList) ->
  ((Object.keys(fields).reduce ((newObj, key) ->
                    newKey = fields[key](key, obj[key]) if typeof(obj[key]) isnt "undefined"
                    newObj[newKey[0]] = newKey[1] if newKey and newKey[1] isnt null
                    return newObj), {}) \
   for obj in objectList)

fetchSets = (callback) ->
  request.get baseurl + "packs", (error, response, body) ->
    if !error and response.statusCode is 200
      data = JSON.parse(body).data
      for set in data
        mapSets[set.code] = set.name
      sets = selectFields(setFields, data)
      db.collection("sets").remove ->
      db.collection("sets").insert sets, (err, result) ->
        fs.writeFile "andb-sets.json", JSON.stringify(sets), ->
          console.log("#{sets.length} sets fetched")
        callback(null, sets.length)

fetchImg = (urlPath, code, imgPath, t) ->
  setTimeout ->
    console.log("Downloading image for " + code)
    url = urlPath.replace("{code}", code)
    request(url).pipe(fs.createWriteStream(imgPath))
  , t

fetchCards = (callback) ->
  request.get baseurl + "cards", (error, response, body) ->
    if !error and response.statusCode is 200
      res = JSON.parse(body)
      cards = selectFields(cardFields, res.data)
      imgDir = path.join(__dirname, "..", "resources", "public", "img", "cards")
      mkdirp imgDir, (err) ->
        if err
          console.error("Failed to create card image resource directory #{imgDir}")
      i = 0
      for card in cards
        imgPath = path.join(imgDir, "#{card.code}.png")
        if !fs.existsSync(imgPath)
          fetchImg(res.imageUrlTemplate, card.code, imgPath, i++ * 200)

      db.collection("cards").remove ->
      db.collection("cards").insert cards, (err, result) ->
        fs.writeFile "andb-cards.json", JSON.stringify(cards), ->
          console.log("#{cards.length} cards fetched")
        callback(null, cards.length)
    db.close()

async.series [fetchSets, fetchCards]
