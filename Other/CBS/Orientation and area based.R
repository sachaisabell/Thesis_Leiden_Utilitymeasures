
library(sf)
library(sp)
library(dplyr)
library(scales)

### ODI function
# INPUT: dataframe with x and y coordinates in that order
# OUTPUT: single value for ODI

ellipse_angle_func = function(points) {
  
  # define the SDE ellipse
  sde = aspace::calc_sde(points = points, id= 1)
  
  # find the point farthest away from the center of the ellipse (on the major axis (ma))
  anglepoint = which.max(sqrt((sde$LOCATIONS$x - sde$ATTRIBUTES$CENTRE.x)^2 +  (sde$LOCATIONS$y - sde$ATTRIBUTES$CENTRE.y)^2))
  anglepointloc = sde$LOCATIONS[anglepoint,]
  
  # plotting the points, ellipse centre, point on the ma, and the ellipse itself, respectively
  plot(points, axes = TRUE, asp = 1)
  points(sde$ATTRIBUTES$CENTRE.x,sde$ATTRIBUTES$CENTRE.y, col = "blue", pch = 20, lwd = 5)
  points(anglepointloc[,2], anglepointloc[,3], lwd = 5, col = "green")
  lines(sde$FORPLOTTING$coordsSDE[,1], sde$FORPLOTTING$coordsSDE[,2], col = "red")
  
  # calculate the angle of the  ellipse based on the centre and the point on the ma
  angleSDE = atan(anglepointloc$y/anglepointloc$x)* 180/pi
  
  # standardize angles
  if(angleSDE < 0) return(angleSDE + 180)
  
  return(angleSDE)
}


ODI_func = function(safe, original){
  # calculate the angle of the SDE for both safe and original data
  orient.safe = ellipse_angle_func(safe)
  orient.og = ellipse_angle_func(original)
  
  # calculate the ODI measure and return
  ODI = abs(orient.og - orient.safe) / 180
  
  print(orient.safe);print(orient.og)
  return(ODI)
  
}



### Minimum convex polygon preservation
# INPUT: data frame with x and y in that order
# OUTPUT: single value for the MCP ratio

MCP_func = function(data){
  # must declare an identity ("a") and coordintes
  data$ID = rep('a',nrow(data))
  sp::coordinates(data) <- data[,c("x","y")]
  MCP = adehabitatHR::mcp(data[,3], percent = 100) # 100 to include all points regardless of outliers
  
  # obtain coordinates from the mcp function ...
  MCP.coor = as.data.frame(MCP@polygons[[1]]@Polygons[[1]]@coords)
  MCP.coor$ID = rep("a", nrow(MCP.coor))
  
  ### ... and turn into an sf object POLYGON
  MCP.poly = sf::st_as_sf(as.data.frame(MCP.coor), coords = c("x","y")) %>% group_by(ID) %>% summarise(geometry = sf::st_combine(geometry)) %>% sf::st_cast("POLYGON")
  
  return(MCP.poly)
}

convex_poly_func = function(safe, original){
  # obtain mcp for safe and original
  MCP.safe = MCP_func(safe)
  MCP.og = MCP_func(original)
  
  # calculate overlap of mcp's and the ratio of excluded to original mcp for the original data
  overlap = sf::st_intersection(MCP.safe, MCP.og)
  MCP.ratio = (sf::st_area(MCP.og) - sf::st_area(overlap)) / sf::st_area(MCP.og)
  
  # plot both mcp's
  plot(MCP.og$geometry, axes  = TRUE, asp = 1, col = alpha("blue",.4))
  plot(MCP.safe$geometry, add= TRUE, col = alpha("red",.4))

  return(MCP.ratio)
}



### standard deviation ellipse preservation
# INPUT: data frame with x and y in that order
# OUTPUT: single value for the MCP ratio

ellipse_func = function(points) {
  # calculate the SDE
  sde = calc_sde(points = points, id= 1)
  
  # extract locations on the border of the sde and turn into sf object POLYGON
  locs = sde$LOCATIONS
  ellpol = st_as_sf(locs, coords = c("x","y")) %>% group_by(id) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")
  
  return(ellpol)
}

ellipse_ratio_func = function(safe, original){
  # calculate SDE for both original and safe
  ell.safe = ellipse_func(safe)
  ell.og = ellipse_func(original)
  
  # calculate overlap of sde's and the ratio of excluded to original sde for the original data
  overlap = st_intersection(ell.safe, ell.og)
  ell.ratio = (st_area(ell.og) - st_area(overlap)) / st_area(ell.og)
  
  # plot both sde's
  plot(ell.og$geometry, axes  = TRUE, asp = 1, col = alpha("blue",.4))
  plot(ell.safe$geometry, add= TRUE, col = alpha("red",.4))

  return(ell.ratio)
}


ellipse_ratio_func(df.safe, df.original)





### Examples
set.seed(1)
df.original <- base::data.frame(x = c(abs(rnorm(200)), abs(rnorm(200,4))), y = c(abs(rnorm(200)), abs(rnorm(200,4))))
df.safe =sdcSpatial:: mask_random(df.original, r = 0.5)

ODI_func(df.safe, df.original)

convex_poly_func(df.safe,df.original)




































