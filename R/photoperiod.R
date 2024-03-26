
photoperiod=function(dayofyear,latitude,longitude,zone=0,twilight="civil") {
  # twilight: "rise/set" or "civil" or "nautic")
  # source: http://lexikon.astronomie.info/zeitgleichung/
  T=dayofyear
  truediff = -0.171*sin(0.0337 * T + 0.465) - 0.1299*sin(0.01787 * T - 0.168)
  declin = 0.4095*sin(0.016906*(T-80.086))
  B = pi *latitude / 180
  
  if (twilight=="rise/set") h=-50/60
  if (twilight=="civil")  h=-6
  if (twilight=="nautic") h=-12
  
  h=h/(360/(2*pi))
  timedev = 12*acos((sin(h) - sin(B)*sin(declin)) / (cos(B)*cos(declin)))/pi
  localrise = 12 - timedev - truediff
  localset = 12 + timedev - truediff
  sunrise= localrise - longitude /15 + zone
  sunset = localset -longitude /15 + zone
  return(list(rise=sunrise,set=sunset))
}
