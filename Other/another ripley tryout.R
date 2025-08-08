





df = dwellings[,1:2]
dfsafe = mask_random(df, 100)
dfnorm = data.frame(x = rnorm(nrow(df),mean= mean(df$x), sd= sd(df$x)),
                    y = rnorm(nrow(df), mean(df$y), sd(df$y)))

win = as.ppp(df, c(min(df$x), max(df$x), min(df$y), max(df$y)))
RipL = Lest(win ,correction="isotropic")
RipK = Kest(win ,correction="isotropic")

win2 = as.ppp(dfsafe, c(min(dfsafe$x), max(dfsafe$x), min(dfsafe$y), max(dfsafe$y)))
RipL2 = Lest(win2 ,correction="isotropic")
RipK2 = Kest(win2 ,correction="isotropic", r = RipK$r)


### for the L function, separately
par(mfrow = c(1,2))
plot(RipL2, col = "red", type = "l")
plot(RipL, col = "red", type = "l")

## plotting lines in the same plot
par(mfrow = c(1,2))
plot(RipL$r, RipL$iso, col = "blue", type = "l")
lines(RipL2$r, RipL2$iso, col = "red")
lines(RipL2$r, RipL2$theo, col = "black")

plot(RipL$r, RipL$iso - RipL$r, col = "blue", type = "l")
lines(RipL2$r, RipL2$iso - RipL2$r, col = "red")
lines(RipL2$r, RipL2$theo - RipL2$r, col = "black")


# making sure that the theoretical overlap too
plot(RipL$r, RipL$theo, col = "blue", type = "l")
lines(RipL2$r, RipL2$theo, col = "red")


### for the K function, separately
par(mfrow = c(1,2))
plot(RipK2, col = "red", type = "l")
plot(RipK, col = "red", type = "l")

## plotting lines in the same plot
par(mfrow = c(1,1))
plot(RipK$r, (RipK$iso) - (RipK2$iso), col = "blue", type = "l")

plot(RipK$r, (RipK$iso / (pi*RipK$r^2)) - (RipK2$iso / (pi*RipK2$r^2)), col = "blue", type = "l")
lines(RipK2$r, RipK2$iso / (pi*RipK2$r^2), col = "red")
lines(RipK2$r, RipK2$theo / (pi*RipK2$r^2), col = "black")


# making sure that the theoretical overlap too
plot(RipK$r, RipK$theo, col = "blue", type = "l")
lines(RipK2$r, RipK2$theo, col = "red")



#######
Ddat = as.wmppp(win.cross.13)

RipD = Dhat(Ddat, Cases = "a", Controls = "b", r = RipK$r)
RipD2 = Dhat(Ddat, Cases = "c", Controls = "b")
RipD3 = Dhat(Ddat, Cases = "c", Controls = "a")

par(mfrow = c(1,2))
plot(RipD3, col = "red", type = "l")
plot(RipD2, col = "red", type = "l")
plot(RipD, col = "red", type = "l")

par(mfrow = c(1,2))
plot(RipK$r, RipK$iso - RipK2$iso, col = "blue", type = "l")

###
RipM = Mhat(Ddat, ReferenceType = "a", NeighborType = "b", r = RipK$r)
RipM2 = Mhat(Ddat, ReferenceType = "c", NeighborType = "b", r = RipK$r)
RipM3 = Mhat(Ddat, ReferenceType = "a", NeighborType = "c", r = RipK$r)

par(mfrow = c(1,3))
plot(RipM)
plot(RipM2)
plot(RipM3)

plot(RipM2$r, RipM2$M, col = "blue", type = "l")
lines(RipM3$r, RipM3$M, col = "red")

RipM$


### K-cross
dfsafesafe = mask_random(dfsafe, 10)

k13cross = rbind(df[1:100,], dfsafe[1:100,], dfsafesafe[1:100,])
win.cross.13 = as.ppp(k13cross, c(min(k13cross$x), max(k13cross$x), min(k13cross$y), max(k13cross$y)))
win.cross.13$origin = c(rep("a",100), rep("b", 100), rep("c", 100))
win.cross.13$origin = as.factor(win.cross.13$origin)
marks(win.cross.13) <- win.cross.13$origin

cross.13 = Kcross(win.cross.13, "a", "c", correction = "isotropic")
cross.23 = Kcross(win.cross.13, "c", "b", correction = "isotropic")

par(mfrow=c(1,2))
plot(cross.13, legend = FALSE)
plot(cross.23, legend = FALSE)

### m-criss
RipM = Dhat(Ddat, Cases = "a", Controls = "b", r = RipK$r)
RipD2 = Dhat(Ddat, Cases = "c", Controls = "b")
RipD3 = Dhat(Ddat, Cases = "c", Controls = "a")

par(mfrow = c(1,2))
plot(RipD3, col = "red", type = "l")
plot(RipD2, col = "red", type = "l")
plot(RipD, col = "red", type = "l")














































