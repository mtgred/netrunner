express = require('express')
app = express()
server = require('http').createServer(app)
stylus = require('stylus')
coffeeMiddleware = require('coffee-middleware')

# db = require('mongoskin').db('localhost:27017/netrunner')
# io = require('socket.io').listen(server) #
# io.set('log level', 2)

app.configure ->
  app.set 'port', process.env.PORT || 3000
  app.use express.favicon()
  app.use express.methodOverride() # provide PUT DELETE
  app.use express.cookieParser()
  app.use express.bodyParser()
  app.use stylus.middleware({src: __dirname + '/src', dest: __dirname + '/resources'})
  app.use coffeeMiddleware({src: __dirname + '/src', dest: __dirname + '/resources'})
  app.use '/cljs',  express.static(__dirname + '/out')
  app.use express.static(__dirname + '/resources')
  app.use app.router

app.get '/*', (req, res) ->
  res.render('index.jade', {})

server.listen app.get('port'), ->
  console.log("Express server listening on port " + app.get('port'))
