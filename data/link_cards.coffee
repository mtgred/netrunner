mongoskin = require('mongoskin')
async = require('async')

mongoUser = process.env.OPENSHIFT_MONGODB_DB_USERNAME
mongoPassword = process.env.OPENSHIFT_MONGODB_DB_PASSWORD
login = if process.env.OPENSHIFT_MONGODB_DB_PASSWORD then "#{mongoUser}:#{mongoPassword}@" else ""
mongoHost = process.env.OPENSHIFT_MONGODB_DB_HOST || '127.0.0.1'
mongoPort = process.env.OPENSHIFT_MONGODB_DB_PORT || '27017'
appName = process.env.OPENSHIFT_APP_NAME || 'netrunner'

db = mongoskin.db("mongodb://#{login}#{mongoHost}:#{mongoPort}/#{appName}").open( (err, _) -> throw err if err )

cycleMap = {}
duplicate_titles = []

get_cycles = (callback) ->
  db.collection('cycles').find().toArray (err, cycles) ->
    throw(err) if err
    async.each cycles, (cycle, inner_callback) ->
      if cycle
        cycleMap[cycle.code] = cycle
      inner_callback()
    , (inner_err) ->
      throw(inner_err) if inner_err
      callback(null, 0)
  

get_cards = (callback) ->
  db.collection('cards').find().toArray (err, cards) ->
    throw(err) if err
    card_count = cards.reduce (acc, card) ->
      if acc[card.title]
        acc[card.title] += 1
      else
        acc[card.title] = 1
      acc
    , {}
    names = Object.keys(card_count)
    duplicate_titles = names.reduce (acc, card) ->
      if card_count[card] > 1
        acc.push card
      acc
    , []
    callback(null, 0)

zip = () ->
  lengthArray = (arr.length for arr in arguments)
  length = Math.min(lengthArray...)
  for i in [0...length]
    arr[i] for arr in arguments

link_cards = (callback) ->
  console.log("Found " + duplicate_titles.length + " duplicated cards")
  async.each duplicate_titles, (title, inner_callback) ->
    db.collection('cards').find({title: title}).toArray (err, matches) ->
      throw(err) if err
      matches = matches.sort (a, b) ->
        cycleMap[b.cycle_code].position - cycleMap[a.cycle_code].position

      sorted_matches = matches.slice 0
      matches.shift()
      tuples = zip(sorted_matches, matches)

      async.each tuples, ([new_card, old_card], tuple_callback) ->
        console.log("Card: " + new_card.title + ": " + new_card.setname + ", " + old_card.setname)
        db.collection('cards').update {code: new_card.code}, {'$set': {replaces: old_card.code}},
          (err, result) ->
            throw(err) if err
            db.collection('cards').update {code: old_card.code}, {'$set': {replaced_by: new_card.code}},
              (err, result) ->
                throw(err) if err
                tuple_callback()
      , (tuple_err) ->
        throw(tuple_err) if tuple_err
        inner_callback()
  , (err) ->
    throw(err) if err
    callback(null, 0)

async.series [get_cycles, get_cards, link_cards, () -> db.close()]
