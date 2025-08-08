

### L-function
l.func_func = function(original, safe){
  
  win.original = spatstat.geom::as.ppp(original, c(min(original$x), max(original$x), min(original$y), max(original$y)))
  win.safe = spatstat.geom::as.ppp(safe, c(min(safe$x), max(safe$x), min(safe$y), max(safe$y)))
  
  L.original = spatstat.explore::Lest(win.original, correction = "isotropic")
  L.safe = spatstat.explore::Lest(win.safe, correction = "isotropic")
  
  
  par(mfrow = c(1,2))
  plot(L.original)
  plot(L.safe)
  
}


l.func_func(df[1:100,],dfsafe[1:100,])



### D-function
d.func_func = function(original, safe){
  
  dataset = rbind(original, safe)
  input = spatstat.geom::as.ppp(dataset,  c(min(dataset$x), max(dataset$x), min(dataset$y), max(dataset$y)))
  input$origin = c(rep("original",nrow(original)), rep("safe", nrow(safe)))
  input$origin = as.factor(input$origin)
  spatstat.geom::marks(input) <- input$origin
  
  Ddat = dbmss::as.wmppp(input)
  RipD = dbmss::Dhat(Ddat, Cases = "original", Controls = "safe")
  
  plot(RipD)
}


d.func_func(df[1:100,],dfsafe[1:100,])



### K-cross function
# make sure that both have only columns x and y
k.cross_func = function(original, safe){
  
  dataset = rbind(original, safe)
  input = spatstat.geom::as.ppp(dataset,  c(min(dataset$x), max(dataset$x), min(dataset$y), max(dataset$y)))
  input$origin = c(rep("original",nrow(original)), rep("safe", nrow(safe)))
  input$origin = as.factor(input$origin)
  spatstat.geom::marks(input) <- input$origin
  
  crossk = spatstat.explore::Kcross(input, "original", "safe", correction = "isotropic")
  plot(crossk)
}


k.cross_func(df[1:100,],dfsafe[1:100,])









