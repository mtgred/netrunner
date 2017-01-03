mongoose = require('mongoose')
Schema = mongoose.Schema

postSchema = new Schema({
  title: String,
  date: Date
},
{
    timestamps: true
})

Post = mongoose.model('Post', postSchema);

module.exports = Post