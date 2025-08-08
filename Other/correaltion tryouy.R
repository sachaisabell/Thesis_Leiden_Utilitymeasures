

local_MI_func = function(original, safe, style = c("W","C","B"), type = c("grid", "area")){
  
  if(type == "grid"){
    og = original#original$value$mean %>% raster::as.matrix(); 
    #og[is.na(og)] = 0
    sf = safe#safe$value$mean %>% raster::as.matrix(); 
    sf[is.na(sf)] = 0
    
    original.weights = weight.matrix(original, style = style, map = type, output = "list")
    safe.weights = weight.matrix(safe, style = style, map = type, output = "list")
    
    MI.original = spdep::localmoran(raster::values(og), original.weights)
    MI.safe = spdep::localmoran(raster::values(sf), safe.weights)
  }
  
  if(type == "area"){
    
    original.weights = weight.matrix(original, style = style, map = type, output = "list")
    safe.weights = weight.matrix(safe, style = style, map = type, output = "list")
    
    MI.original = spdep::localmoran(original$values, original.weights)
    MI.safe = spdep::localmoran(safe$values, safe.weights)
  }
  
  
  return(c(MI.original, MI.safe))  
}


weight.matrix = function(input, style = c("B", "C", "W"), output = c("matrix","list"), map = c("grid", "area")){
  
  # first get the neighbors
  if(map == "grid"){
    # input = input$value$mean
    # input = raster::as.matrix(input); 
    #input[is.na(input)] = 0
    input.weights = spdep::cell2nb(nrow(input),ncol(input)) 
    
  } 
  else if(map == "area"){
    input = input$geometry
    input.weights = spdep::poly2nb(input) 
  } 
  
  # then actually calculate the correct weights matrix
  if(output == "matrix"){
    weight.matrix =spdep:: nb2mat(input.weights, style = style, zero.policy = TRUE)
  }
  else if(output == "list"){
    weight.matrix = spdep::nb2listw(input.weights, style = style, zero.policy = TRUE)
  }
  
  
  return(weight.matrix)
}

# turn data back into a raster
nro = raster(nrow = 35, ncol = 35)
raster::extent(nro) = rep(c(0,1),2)
values(nro) = normal_maps$raster_data$original$count
plot(nro)

### to test this raster, should be about -1 in rooks case
moran1 = raster(nrow = 5, ncol = 5); extent(moran1) = rep(c(0,1),2)
values(moran1) = c(1, matrix(rep(c(0,1),12)))
plot(moran1)

neighs = spdep::cell2nb(nrow(moran1),ncol(moran1)) 
weight.matrix = spdep::nb2listw(neighs, style = "B", zero.policy = TRUE)
out = spdep::localmoran(values(moran1), weight.matrix)
out[,1]

out.global = spdep::moran.test(values(moran1), weight.matrix)
out.global$estimate

nrow(moran1)

local_MI_func(moran1,moran1, "B", "grid")

output.m1 = raster(nrow = 5, ncol = 5); extent(output.m1) = rep(c(0,1),2)
values(output.m1) = out[,1]
plot(output.m1)



###############
moransI_grid_func = function(input, version = c("local", "global"), type = c("rook", "queen"), style = "B") {

  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE)  # Weight matrix
  
  if (version == "global") {
    return(spdep::moran.test(values(input), weight.matrix)$estimate)
  } else {
    return(spdep::localmoran(values(input), weight.matrix)[,1])
  }
}

moransI_grid_func(moran1, "local", "queen")


########### now test for areas
moran1_poly = rasterToPolygons(moran1, dissolve = FALSE) %>% st_as_sf()
plot(moran1_poly$geometry)


neighs = spdep::poly2nb(moran1_poly$geometry)   # Neighbors list
weight.matrix = spdep:: nb2listw(neighs, style = "B", zero.policy = TRUE) 
spdep::localmoran(moran1_poly$values, weight.matrix)[,1]

moran1_poly$values = moran1_poly$layer




moransI_area_func = function(input, version = c("local", "global"), type = c("rook", "queen"), style = c("B","C","W")) {
 
  neighs = spdep::poly2nb(input$geometry)   # Neighbors list
  weight.matrix = spdep:: nb2listw(neighs, style = style, zero.policy = TRUE)  # Weight matrix

  if (version == "global") {
    return(spdep::moran.test(input$values, weight.matrix)$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(spdep::localmoran(input$values, weight.matrix)[,1])
  }
}

moransI_area_func(moran1_poly, "global", "rook", "B")

#####################
Gi_function = function(data, d, star = FALSE) {
  n = length(data)
  GIs = rep(NA,n)
  
  for (i in 1:n) {
    num = den = 0
    
    for (j in 1:n) {
      if (star == FALSE & i == j) next
      
      else {
        num = num + d[i,j] * data[j]
        den = den + data[j]
      }
      
    }
    
    GIs[i] = num / den
  }
  
  return(GIs)
}

Gi_grid_func = function(input, version = c("local", "global"), type = c("rook", "queen"),  style = "B", star = FALSE) {
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2mat(neighs, style = style, zero.policy = TRUE) 
  diag(weight.matrix) = 1 # function automatically sets to 0 which is not good
  
  if (version == "global") {
    return(spdep::globalG.test(values(input), spdep::nb2listw(neighs, style = style, zero.policy = TRUE))$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(Gi_function(values(input), weight.matrix, star = star))
  }
  

}

Gi_area_func = function(input, version = c("local", "global"),  style = "B", star = FALSE) {
  
  neighs = spdep::poly2nb(input$geometry)  # Neighbors list
  weight.matrix = spdep::nb2mat(neighs, style = style, zero.policy = TRUE) 
  diag(weight.matrix) = 1
  
  if (version == "global") {
    return(spdep::globalG.test(input$values, spdep::nb2listw(neighs, style = style, zero.policy = TRUE))$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(Gi_function(input$values, weight.matrix, star = star))
  }
  
}


moran1 = raster(nrow = 5, ncol = 5); extent(moran1) = rep(c(0,1),2)
values(moran1) = c(1, matrix(rep(c(0,1),12)))
plot(moran1)

neighs = spdep::cell2nb(nrow(moran1),ncol(moran1)) 
weight.matrix = spdep::nb2mat(neighs, style = "B", zero.policy = TRUE)
out = Gi_core(values(moran1), weight.matrix, star = FALSE)

empty = raster(nrow = 5, ncol = 5); extent(empty) = rep(c(0,1),2)
values(empty) = out
plot(empty)

Gi_grid_func(moran1, "rook", star = TRUE)


Gi_area_func(sf_obj)
Gi_area_func(sf_obj, star = TRUE)


moransI_area_func(sf_obj, "local", style = "W")
moransI_area_func(sf_obj, "global", style = "W")



sf_obj$value
neighs = spdep::poly2nb(sf_obj$geometry)
weight.matrix = spdep::nb2mat(neighs, style = "B", zero.policy = TRUE); diag(weight.matrix ) = 1

Gi_function(sf_obj$value, weight.matrix, star = TRUE)


ras.new = raster(nrow = 2, ncol = 4)
values(ras.new) = c(8, 6, 3, 2, 
                    0,6,0,0)
plot(ras.new)


area.new = rasterToPolygons(ras.new, dissolve = FALSE) %>% st_as_sf()
area.new$values = area.new$layer
plot(area.new)

Gi_grid_func(ras.new, "queen","W")
Gi_area_func(area.new, "W")

ras.out = ras.new
values(ras.out) = Gi_grid_func(ras.new, "queen","W")
plot(ras.out)

neighs = spdep::cell2nb(moran1)   # Neighbors list
weight.matrix = spdep:: nb2listw(neighs, style = "B", zero.policy = TRUE) 
spdep::localG(moran1_poly$values, weight.matrix)

Gi_area_func(moran1_poly,"local", "B")
Gi_area_func(moran1_poly,"global", "B")


####### now test random values
neighs = spdep::cell2nb(nrow(moran1),ncol(moran1), type = "rook") 
weight.matrix = spdep:: nb2listw(neighs, style = "B", zero.policy = TRUE) 

spdep::localC(abs(values(moran1)-1), weight.matrix)

neighs = spdep::poly2nb(sf_obj$geometry)
weight.matrix = spdep::nb2listw(neighs, style = "W", zero.policy = TRUE)#; diag(weight.matrix ) = 1
output = spdep::localC(sf_obj$values, weight.matrix)
sum(output^2)

spdep::geary.test(sf_obj$value, weight.matrix)


Geary_grid_func = function(input, version = c("local", "global"), type = c("rook", "queen"),  style = "B") {
  
  neighs = spdep::cell2nb(nrow(input), ncol(input), type = type)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 

  if (version == "global") {
    return(spdep::geary.test(values(input), weight.matrix)$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(spdep::localC(values(input), weight.matrix, star = star))
  }
  
  
}

Geary_area_func = function(input, version = c("local", "global"),  style = "B") {
  
  neighs = spdep::poly2nb(input$geometry)  # Neighbors list
  weight.matrix = spdep::nb2listw(neighs, style = style, zero.policy = TRUE) 

  if (version == "global") {
    return(spdep::geary.test(input$values, weight.matrix)$estimate) # values is not inherent, name yourself if necessary
  } else {
    return(spdep::localC(input$values, weight.matrix, star = star))
  }
  
}


Geary_grid_func(moran1, "global", "rook", "B")


moran2 = raster(nrow = 5, ncol = 5)
raster::extent(moran2) = rep(c(0,1),2)
values(moran2) = c(matrix(rep(1:5,5), byrow = TRUE))
plot(moran2)

Geary_grid_func(moran2, "global", "rook", "B")

moran2_poly = rasterToPolygons(moran2) %>% st_as_sf()
plot(moran2_poly)
moran2_poly$values = moran2_poly$layer

moran1_poly

Geary_grid_func(moran2, "global", "queen")
Geary_area_func(moran2_poly, "global", "B")
moransI_area_func(moran1_poly, "global", "B")

moran2_poly$geometry

############ now to turn the output into a local map
### for grids
local_grid_map = function(output, original){
  
  empty.raster= raster(ncol = ncol(original), nrow = nrow(original))
  extent(empty.raster) = (extent(original)[1:4])
  values(empty.raster) = output
  
  return(empty.raster)
}

outmap = local_grid_map(Geary_grid_func(moran1, "local", "queen", "B"), moran1)
plot(outmap)


local_area_map = function(output, original){
  
  output.map = original
  output.map$values = output
  
  return(output.map)
  
}

out1 = local_area_map(Geary_area_func(moran1_poly, "local", "B"), moran1_poly)
plot(out1)






# Helper to make rectangles by bottom-left and top-right corners
make_rect <- function(xmin, ymin, xmax, ymax) {
  st_polygon(list(matrix(
    c(xmin, ymin,
      xmin, ymax,
      xmax, ymax,
      xmax, ymin,
      xmin, ymin),
    ncol = 2, byrow = TRUE)))
}

# Create individual polygons
poly1 <- make_rect(0, 1, 1, 2)
poly2 <- make_rect(1, 1, 2, 2)
poly3 <- make_rect(1, 0, 2, 1)
poly4 <- make_rect(2, 1, 3, 2)
poly5 <- make_rect(3, 1, 4, 2)

# Combine into sf object
polys <- st_sfc(poly1, poly2, poly3, poly4, poly5)
sf_obj <- st_sf(id = 1:5, value = c(8, 6, 6, 3, 2), geometry = polys)
sf_obj$values = sf_obj$value

# View
plot(sf_obj["value"], main = "Custom Polygon SF Object")



