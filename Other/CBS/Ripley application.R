

library(spatstat.explore)
library(dbmss)
library(spatstat.geom)

# all safe v original separately
all_ripleys_func_sep = function(original, safe, maplim){
  df = original
  dfsafe = safe
  
  dwell_limits = maplim$coordinates$limits
  win = as.ppp(df, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  win2 = as.ppp(dfsafe, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  
  # original
  RipL = Lest(win ,correction="isotropic")
  RipK = Kest(win ,correction="isotropic")
  
  # random
  RipL2 = Lest(win2 ,correction="isotropic", r = RipL$r)
  RipK2 = Kest(win2 ,correction="isotropic", r = RipK$r)
  
  ################ Ripleys K
  par(mfrow = c(1,1))
  plot(RipK$r, RipK$iso, col = "blue", type = "l", xlab = "r", main = names(safe))
  lines(RipK2$r, RipK2$iso, col = "red")
  lines(RipK2$r, RipK2$theo, col = "black")
  
  ################ L and other transformations
  par(mfrow = c(1,4))
  plot(RipK$r, RipK$iso, col = "blue", type = "l", xlab = "r", main = "K function")
  lines(RipK2$r, RipK2$iso, col = "red")
  lines(RipK2$r, RipK2$theo, col = "black")
  
  plot(RipL$r, RipL$iso, col = "blue", type = "l", xlab = "r", main = "L function")
  lines(RipL2$r, RipL2$iso, col = "red")
  lines(RipL2$r, RipL2$theo, col = "black")
  
  plot(RipL$r, RipL$iso - RipL$r, col = "blue", type = "l", xlab = "r", main = "L(r) - r")
  lines(RipL2$r, RipL2$iso - RipL2$r, col = "red")
  lines(RipL2$r, RipL2$theo - RipL2$r, col = "black")
  
  plot(RipK$r, (RipK$iso / (pi*RipK$r^2)), col = "blue", type = "l", xlab = "r", main = "K(r) / (pi*r^2)")
  lines(RipK2$r, RipK2$iso / (pi*RipK2$r^2), col = "red")
  lines(RipK2$r, RipK2$theo / (pi*RipK2$r^2), col = "black")
  
  #################### cross functions and d function
  cross = rbind(df, dfsafe)
  win.cross = as.ppp(cross, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  win.cross$origin = c(rep("a",nrow(df)), rep("b", nrow(dfsafe)))
  win.cross$origin = as.factor(win.cross$origin)
  marks(win.cross) <- win.cross$origin
  
  ### k-cross, mcross, d function
  kcross1 = Kcross(win.cross, "a", "b", correction = "isotropic")
  win.cross2 = as.wmppp(win.cross)
  RipD = Dhat(win.cross2, Cases = "a", Controls = "b")
  RipM = Mhat(win.cross2, ReferenceType = "a", NeighborType = "b")
  
  par(mfrow = c(1,3))
  plot(RipD$r, RipD$iso, col = "purple", type = "l", xlab = "r", main = "D function")
  lines(RipD$r, RipD$theo, col = "black")
  
  plot(kcross1$r, kcross1$iso, col = "purple", type = "l", xlab = "r", main = "K Intertype function")
  lines(kcross1$r, kcross1$theo, col = "black")
  
  plot(RipM$r, RipM$M, col = "purple", type = "l", xlab = "r", main = "M Intertype function")
  lines(RipM$r, RipM$theo, col = "black")
  
}

# all safe v original together
all_ripleys_func = function(original, safe, maplim){
  df = original
  dfrand = as.data.frame(safe[1])
  dfgrid = as.data.frame(safe[2])
  dfvono = as.data.frame(safe[3])
  dfweigh = as.data.frame(safe[4])
  
  names(df) = names(dfrand) = names(dfgrid) = names(dfvono) = names(dfweigh) <- c("x","y")
  
  dwell_limits = maplim$coordinates$limits
  win.o = as.ppp(df, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  win.r = as.ppp(dfrand, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  win.g = as.ppp(dfgrid, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  win.v = as.ppp(dfvono, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  win.w = as.ppp(dfweigh, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  
  # original
  RipL = Lest(win.o ,correction="isotropic")
  RipK = Kest(win.o ,correction="isotropic")
  # random
  RipL.r = Lest(win.r ,correction="isotropic", r = RipL$r)
  RipK.r = Kest(win.r ,correction="isotropic", r = RipK$r)
  # grid
  RipL.g = Lest(win.g ,correction="isotropic", r = RipL$r)
  RipK.g = Kest(win.g ,correction="isotropic", r = RipK$r)
  # vonoroi
  RipL.v = Lest(win.v ,correction="isotropic", r = RipL$r)
  RipK.v = Kest(win.v ,correction="isotropic", r = RipK$r)
  # weighted
  RipL.w = Lest(win.w ,correction="isotropic", r = RipL$r)
  RipK.w = Kest(win.w ,correction="isotropic", r = RipK$r)
  
  ################ K L and other transformations
  par(mfrow = c(1,3))
  plot(RipK$r, RipK$iso, col = "blue", type = "l", xlab = "r", main = "K function",ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(RipK.r$r, RipK.r$iso, col = "pink")
  lines(RipK.g$r, RipK.g$iso, col = "red")
  lines(RipK.v$r, RipK.v$iso, col = "orange")
  lines(RipK.w$r, RipK.w$iso, col = "purple")
  lines(RipK$r, RipK$theo, col = "black")
  
  plot(RipL$r, RipL$iso, col = "blue", type = "l", xlab = "r", main = "L function",ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(RipL.r$r, RipL.r$iso, col = "pink")
  lines(RipL.g$r, RipL.g$iso, col = "red")
  lines(RipL.v$r, RipL.v$iso, col = "orange")
  lines(RipL.w$r, RipL.w$iso, col = "purple")
  lines(RipL$r, RipL$theo, col = "black")
  
  plot(RipL$r, RipL$iso - RipL$r, col = "blue", type = "l", xlab = "r", main = "L(r) - r",ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(RipL.r$r, RipL.r$iso - RipL.r$r, col = "pink")
  lines(RipL.g$r, RipL.g$iso - RipL.g$r, col = "red")
  lines(RipL.v$r, RipL.v$iso - RipL.v$r, col = "orange")
  lines(RipL.w$r, RipL.w$iso - RipL.w$r, col = "purple")
  lines(RipL$r, RipL$theo - RipL$r, col = "black")
  
  # now the transformation both in detail and out
  par(mfrow = c(1,2))
  plot(RipK$r, (RipK$iso / (pi*RipK$r^2)), col = "blue", type = "l", xlab = "r", main = "K(r) / (pi*r^2)", ylim = c(0,max(c(c(RipK.r$iso / (pi*RipK.r$r^2))[2:512],
                                                                                                                          c(RipK.g$iso / (pi*RipK.g$r^2))[2:512],
                                                                                                                          c(RipK.v$iso / (pi*RipK.v$r^2))[2:512],
                                                                                                                          c(RipK.w$iso / (pi*RipK.w$r^2))[2:512]))),ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(RipK.r$r, RipK.r$iso / (pi*RipK.r$r^2), col = "pink")
  lines(RipK.g$r, RipK.g$iso / (pi*RipK.g$r^2), col = "red")
  lines(RipK.v$r, RipK.v$iso / (pi*RipK.v$r^2), col = "orange")
  lines(RipK.w$r, RipK.w$iso / (pi*RipK.w$r^2), col = "purple")
  lines(RipK$r, RipK$theo / (pi*RipK$r^2), col = "black")
  
  plot(RipK$r, (RipK$iso / (pi*RipK$r^2)), col = "blue", type = "l", xlab = "r", main = "K(r) / (pi*r^2)",ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(RipK.r$r, RipK.r$iso / (pi*RipK.r$r^2), col = "pink")
  lines(RipK.g$r, RipK.g$iso / (pi*RipK.g$r^2), col = "red")
  lines(RipK.v$r, RipK.v$iso / (pi*RipK.v$r^2), col = "orange")
  lines(RipK.w$r, RipK.w$iso / (pi*RipK.w$r^2), col = "purple")
  lines(RipK$r, RipK$theo / (pi*RipK$r^2), col = "black")
  
  #################### cross functions and d function
  cross = rbind(df, dfrand, dfgrid, dfvono, dfweigh)
  win.cross = as.ppp(cross, c(min(dwell_limits$x), max(dwell_limits$x), min(dwell_limits$y), max(dwell_limits$y)))
  win.cross$origin = c(rep("o",nrow(df)), rep("r", nrow(dfrand)),rep("g", nrow(dfgrid)),rep("v", nrow(dfvono)),rep("w", nrow(dfweigh)))
  win.cross$origin = as.factor(win.cross$origin)
  marks(win.cross) <- win.cross$origin
  
  ### k-cross, mcross, d function
  crossnD_func = function(win.cross, char.safe){
    kcross1 = Kcross(win.cross, "o", char.safe, correction = "isotropic")
    win.cross2 = as.wmppp(win.cross)
    RipD = Dhat(win.cross2, Cases = "o", Controls = char.safe)
    RipM = Mhat(win.cross2, ReferenceType = "o", NeighborType = (char.safe))
    
    return(c(kcross1, RipD, RipM))
    
  }
  
  
  intertypes = sapply(c("g","r","v","w"), function(x) crossnD_func(win.cross, (x)))
  
  par(mfrow = c(1,3))

  ### plot D function
  plot(intertypes[4,]$g, intertypes[6,]$g, col = "red", type = "l", xlab = "r", main = paste("D function"), ylim = c(min(unlist(intertypes[6,])),max(unlist(intertypes[6,]))),
       ,ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(intertypes[4,]$g, intertypes[5,]$g, col = "black")
  lines(intertypes[4,]$r, intertypes[6,]$r, col = "pink")
  lines(intertypes[4,]$v, intertypes[6,]$v, col = "orange")
  lines(intertypes[4,]$w, intertypes[6,]$w, col = "purple")
  
  ### plot crossK function
  plot(intertypes[1,]$g, intertypes[3,]$g, col = "red", type = "l", xlab = "r", main = paste("K intertype function"), ylim = c(min(unlist(intertypes[3,])),max(unlist(intertypes[3,]))),
       ,ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(intertypes[1,]$g, intertypes[2,]$g, col = "black")
  lines(intertypes[1,]$r, intertypes[3,]$r, col = "pink")
  lines(intertypes[1,]$v, intertypes[3,]$v, col = "orange")
  lines(intertypes[1,]$w, intertypes[3,]$w, col = "purple")
  
  ### plot crossM function
  plot(intertypes[7,]$g, intertypes[9,]$g, col = "red", type = "l", xlab = "r", main = paste("M intertype function"), ylim = c(min(unlist(intertypes[9,])[c(2:63, (2+63):(63+63),(2+63*2):(63+63*2),(2+63*3):(63+63*3))]),max(unlist(intertypes[9,])[2:63])),
       ,ylab = "", cex.lab=2.5, cex.axis=1.5, cex.main=2.5, cex.sub=1.5)
  lines(intertypes[7,]$g, intertypes[8,]$g, col = "black")
  lines(intertypes[7,]$r, intertypes[9,]$r, col = "pink")
  lines(intertypes[7,]$v, intertypes[9,]$v, col = "orange")
  lines(intertypes[7,]$w, intertypes[9,]$w, col = "purple")
  
  
  
}








## normal data
all_ripleys_func(normal_maps$point_data$original, normal_maps$point_data[2:5], normal_maps$point$original)

## clustered data
all_ripleys_func(clustered_maps$point_data$original,clustered_maps$point_data[2:5], clustered_maps$point$original)

## random data
all_ripleys_func(random_maps$point_data$original,random_maps$point_data[2:5], random_maps$point$original)

## small clusters data
all_ripleys_func(smallclusters_maps$point_data$original,smallclusters_maps$point_data[2:5], smallclusters_maps$point$original)

## consistent data
all_ripleys_func(consistent_maps$point_data$original,consistent_maps$point_data[2:5], consistent_maps$point$original)


























































