const port = 6789
const express = require('express')
const fs = require('fs')
const path = require('path')
const expressStaticGzip = require("express-static-gzip")
var app = express()

app.use("/", expressStaticGzip('dist'))

app.get('*', (request, response, next) =>{
  response.sendFile(path.resolve('dist', 'index.html'))
})


app.listen(port)

console.log(`Listen to requests on ${port}`)
