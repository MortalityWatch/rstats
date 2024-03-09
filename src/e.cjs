var R = require('r-script')

R('/opt/rstats/bl_ci.r')
  .data({ x: [2017, 2018, 2019], y: [100, 110, 120], h: 3 })
  .callSync()

console.log(out)
