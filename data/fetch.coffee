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

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}").open( (err, _) -> throw err if err )

same = (key, t) ->
  return [key, t]

rename = (newkey) ->
  return (key, t) -> [newkey, t]

capitalize = (s) ->
  return s.charAt(0).toUpperCase() + s.substr(1)

setFields = {
  "name" : same
  "code" : same
  "format" : same
  "position" : same
}

mapSets = {}

mapAlignments = {
  "hero" : "Hero",
  "minion" : "Minion",
  "RW" : "Ringwraith",
  "sauron" : "Sauron",
  "balrog" : "Balrog",
  "FW" : "Fallen-wizard",
  "EL" : "Elf-lord",
  "DL" : "Dwarf-lord",
  "AL" : "Atani-Lord",
  "all" : "All",
  "dual" : "Dual",
  "neutral" : "Neutral"
  "black" : "Black"
  "grey" : "Grey"
  "white" : "White"
}

mapPrimaries = {
  "character" : "Character",
  "hazard" : "Hazard",
  "resource" : "Resource",
  "region" : "Region",
  "site" : "Site"
}

mapSecondaries = {
  "ally" : "Ally",
  "avatar" : "Avatar",
  "character" : "character",
  "creature" : "Creature",
  "creaPerm" : "Creature/Permanent-event",
  "creaShort" : "Creature/Short-event",
  "faction" : "Faction",
  "goldRingItem" : "Gold Ring Item",
  "greaterItem" : "Greater Item",
  "longEvent" : "Long-event",
  "majorItem" : "Major Item",
  "minorItem" : "Minor Item",
  "permanent" : "Permanent-event",
  "permShort" : "Permanent-event/Short-event",
  "region" : "region",
  "short" : "Short-event",
  "site" : "site",
  "specialItem" : "Special Item",
  "stage" : "Stage"
}

banFields = {
  "NameEN" : same
  "code" : same
  "playableAlignment" : same
  "unplayableAlignment" : same
  "effectedAlignment" : same
  "uneffectedAlignment" : same
  "swapableAlignment" : same
  "Set" : same #
}

mapBans = {}

cardFields = {
  "Set": same,
  "Primary": same,
  "Alignment": same,
  "MEID": same,
  "Artist": same,
  "Rarity": same,
  "Precise": same,
  "NameEN": same,
  "NameFR": same,
  "NameGR": same,
  "NameSP": same,
  "NameJP": same,
  "ImageName": same,
  "Text": same,
  "Skill": same,
  "MPs": same,
  "Mind": same,
  "Direct": same,
  "General": same,
  "Prowess": same,
  "Body": same,
  "Corruption": same,
  "Home": same,
  "Unique": same,
  "Secondary": same,
  "Race": same,
  "RWMPs": same,
  "Site": same,
  "Path": same,
  "Region": same,
  "RPath": same,
  "Playable": same,
  "GoldRing": same,
  "GreaterItem": same,
  "MajorItem": same,
  "MinorItem": same,
  "Information": same,
  "Palantiri": same,
  "Scroll": same,
  "Haven": same,
  "Stage": same,
  "Strikes": same,
  "code": same,
  "codeFR": same,
  "codeGR": same,
  "codeSP": same,
  "codeJP": same
}

baseurl = "http://192.168.1.180:8080/rez/"

selectFields = (fields, objectList) ->
  ((Object.keys(fields).reduce ((newObj, key) ->
    newKey = fields[key](key, obj[key]) if typeof(obj[key]) isnt "undefined"
    newObj[newKey[0]] = newKey[1] if newKey and newKey[1] isnt null
    return newObj), {}) \
  for obj in objectList)

fetchSets = (callback) ->
  request.get baseurl + "sets", (error, response, body) ->
    if !error and response.statusCode is 200
      data = JSON.parse(body)
      sets = selectFields(setFields, data)
      for set in sets
        mapSets[set.code] = set
      db.collection("sets").remove ->
        db.collection("sets").insert sets, (err, result) ->
          fs.writeFile "meccg-sets.json", JSON.stringify(sets), ->
            console.log("#{sets.length} sets fetched")
          callback(null, sets.length)

fetchImg = (urlPath, code, imgPath, t) ->
  setTimeout ->
    console.log("Downloading image for " + code)
    url = urlPath + code
    request(url).pipe(fs.createWriteStream(imgPath))
  , t

fetchCards = (callback) ->
  request.get baseurl + "cards", (error, response, body) ->
    if !error and response.statusCode is 200
      res = JSON.parse(body)
      cards = selectFields(cardFields, res)
      imgDir = path.join(__dirname, "..", "resources", "public", "img", "cards")
      mkdirp imgDir, (err) ->
        if err
          console.error("Failed to create card image resource directory #{imgDir}")
      i = 0
      for card in cards
        imgPath = path.join(imgDir, "#{card.ImageName}")
        if !fs.existsSync(imgPath)
          fetchImg(baseurl, card.ImageName, imgPath, i++ * 200)
      db.collection("cards").remove ->
        db.collection("cards").insert cards, (err, result) ->
          fs.writeFile "meccg-cards.json", JSON.stringify(cards), ->
            console.log("#{cards.length} cards fetched")
          callback(null, cards.length)

fetchBans = (callback) ->
  request.get baseurl + "bans", (error, response, body) ->
    if !error and response.statusCode is 200
      data = JSON.parse(body)
      bans = selectFields(setFields, data)
      for ban in bans
        mapBans[ban.code] = ban
      db.collection("bans").remove ->
        db.collection("bans").insert bans, (err, result) ->
          fs.writeFile "meccg-bans.json", JSON.stringify(bans), ->
            console.log("#{bans.length} bans fetched")
          callback(null, bans.length)

async.series [fetchSets, fetchCards, fetchBans, () -> db.close()]
