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

remove_art = (callback) ->
  console.log("Removing old alt_arts")
  db.collection('cards').update {}, {'$unset': {"alt_art": 1}}, {multi: true},
    (err, result) ->
      throw(err) if err
      console.log(result.result)
      callback(null)

cards_with_suffix = (suffix) ->
  imgDir = path.join(__dirname, "..", "resources", "public", "img", "cards")
  files = fs.readdirSync(imgDir)
  suffix_files = (file for file in files when path.basename(file).match(///.*-#{suffix}\.png///))
  (file.split('-')[0] for file in suffix_files)

add_cards = (callback) ->
  db.collection('altarts').find().toArray (err, packs) ->
    throw(err) if err
    console.log("Found " + packs.length + " alt art packs")
    async.eachSeries packs, (pack, pack_callback) ->
      if pack
        console.log("Pack: " + pack.name)
        cards = cards_with_suffix(pack.version)
        console.log("\tFound #{cards.length} alt art cards with suffix #{pack.version}")
        db.collection('altarts').update {version: pack.version}, {'$set': {cards: cards}},
          (alt_error, alt_result) ->
            throw(alt_error) if alt_error
            console.log(alt_result.result)
            pack_callback()
    , (pack_err) ->
      throw(pack_err) if pack_err
      callback(null)

add_art = (callback) ->
  db.collection('altarts').find().toArray (err, packs) ->
    throw(err) if err
    async.eachSeries packs, (pack, pack_callback) ->
      if pack
        console.log("\nPack: " + pack.name)
        async.each pack.cards, (card, card_callback) ->
          db.collection('cards').update {code: card}, {'$set': {"alt_art.#{pack.version}": "#{card}-#{pack.version}"}},
            (card_err, card_result) ->
              throw(card_err) if card_err
              console.log("Updating " + pack.name + " " + card + " : ")
              console.log(card_result.result)
              card_callback()
        , (card_err) ->
          throw(card_err) if card_err
          pack_callback()
    , (pack_err) ->
      throw(pack_err) if pack_err
      callback(null)

add_replacement_art = (callback) ->
  db.collection('cards').find({replaces: {'$exists': true}}).toArray (err, cards) ->
    throw(err) if err
    console.log("\n\nFound " + cards.length + " replacement cards")
    async.each cards, (card, card_callback) ->
      if card
        console.log("Replacement: " + card.title)
        db.collection('cards').find({code: card.replaces}).toArray (err, replaced_card) ->
          throw(err) if err
          replaced = replaced_card[0]
          if replaced and replaced.alt_art
            db.collection('cards').update {code: card.code}, {'$set': {alt_art: replaced.alt_art}},
              (err, update_result) ->
                throw(err) if err
                console.log("Updated alt art: " + card.title)
                console.log(update_result.result)
                card_callback()
          else
            card_callback()
    , (err) ->
      throw(err) if err
      callback(null)

async.series [remove_art, add_cards, add_art, add_replacement_art, () -> db.close()]
