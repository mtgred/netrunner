fs = require('fs')
exec = require('child_process').exec
request = require('request')
mongoskin = require('mongoskin')
mkdirp = require('mkdirp')
path = require('path')
async = require('async')
removeDiacritics = require('diacritics').remove

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
  "date_release" : (k, t) -> ["available", if t is null then "4096-01-01" else t]
  "cycle_code" : (k, t) -> ["cycle", mapCycles[t].name]
  "cyc_code" : rename("cycle_code")
  "size" : (k, t) -> ["bigbox", t > 20]
  "code" : same
  "position" : same
}

cycleFields = {
  "code" : same
  "name" : same
  "position" : same
  "size" : same
  "rotated" : same
}

mwlFields = {
  "name" : same
  "code" : same
  "date_start" : same
  "cards" : same
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

mapCycles = {}

cardFields = {
  "code" : same,
  "title" : same,
  "normalized_title": (k, t) -> ["normalizedtitle", removeDiacritics(t).toLowerCase()],
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
  "pack_code": (k, t) -> ["setname", mapSets[t].name]
  "set_code" : same,
  "cycle_code" : same,
  "side_code" : (k, t) -> ["side", capitalize(t)],
  "uniqueness" : same,
  "memory_cost" : rename("memoryunits"),
  "strength" : same,
  "trash_cost" : rename("trash"),
  "deck_limit" : rename("limited"),
  "quantity" : rename("packquantity"),
  "rotated" : same
}

baseurl = "http://netrunnerdb.com/api/2.0/public/"

selectFields = (fields, objectList) ->
  ((Object.keys(fields).reduce ((newObj, key) ->
                    newKey = fields[key](key, obj[key]) if typeof(obj[key]) isnt "undefined"
                    newObj[newKey[0]] = newKey[1] if newKey and newKey[1] isnt null
                    return newObj), {}) \
   for obj in objectList)

fetchCycles = (callback) ->
  request.get baseurl + "cycles", (error, response, body) ->
    if !error and response.statusCode is 200
      data = JSON.parse(body).data
      cycles = selectFields(cycleFields, data)
      for cycle in cycles
        mapCycles[cycle.code] = cycle
      db.collection("cycles").remove ->
        db.collection("cycles").insert cycles, (err, result) ->
          fs.writeFile "andb-cycles.json", JSON.stringify(cycles), ->
            console.log("#{cycles.length} cycles fetched")
          callback(null, cycles.length)

fetchSets = (callback) ->
  request.get baseurl + "packs", (error, response, body) ->
    if !error and response.statusCode is 200
      data = JSON.parse(body).data
      data = data.map (d) ->
        d.cyc_code = d.cycle_code
        d
      sets = selectFields(setFields, data)
      sets = sets.map (s) ->
        s.rotated = mapCycles[s.cycle_code].rotated
        s.cycle_position = mapCycles[s.cycle_code].position
        s
      for set in sets
        mapSets[set.code] = set
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
      data = res.data.map (d) ->
        d.set_code = d.pack_code
        d.cycle_code = mapSets[d.set_code].cycle_code
        d.rotated = mapSets[d.set_code].rotated
        d.normalized_title = d.title
        d
      cards = selectFields(cardFields, data)
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

fetchMWL = (callback) ->
  request.get baseurl + "mwl", (error, response, body) ->
    throw(error) if error
    if !error and response.statusCode is 200
      data = JSON.parse(body).data
      mwl = selectFields(mwlFields, data)
      db.collection("mwl").remove ->
        db.collection("mwl").insert mwl, (err, result) ->
          throw(err) if err
          fs.writeFile "andb-mwl.json", JSON.stringify(mwl), ->
            console.log("#{mwl.length} MWL lists fetched")
            callback(null, mwl.length)

async.series [fetchCycles, fetchSets, fetchCards, fetchMWL, () -> db.close()]
