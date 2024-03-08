import express from 'express'
import R from 'r-script'

const app = express()
const port = process.env.PORT || 3000

app.get('/bl_ci.json', (req, res) => {
  let x = req.query.x.split(',').map((x) => parseInt(x, 10))
  let y = req.query.y.split(',').map((x) => parseFloat(x, 10))
  let h = parseInt(req.query.h, 10)

  const result = R('bl_ci.R').data({ x: x, y: y, h: h }).callSync()

  const transformedData = result.reduce((acc, obj) => {
    Object.keys(obj).forEach((key) => {
      if (acc[key]) {
        acc[key].push(obj[key])
      } else {
        acc[key] = [obj[key]]
      }
    })
    return acc
  }, {})

  res.setHeader('Content-Type', 'application/json')
  res.end(JSON.stringify(transformedData, null, 2))
})

app.listen(port, () => {
  console.log(`FableTools R wrapper app listening at http://localhost:${port}`)
})
