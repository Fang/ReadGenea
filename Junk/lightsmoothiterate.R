 weights = 1/(0.05+  pmax((smooth.spline(lights[ind, 2], log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )

 points(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2, pch = ".")

##############



plot(times2(hiz[,1]), hiz[,5], type="l")
plot(times2(hiz[,1]), replace(log(hiz[,5]), hiz[,5] == 0, NA), type="l")
?smooth.spline
cbind( times2(hiz[,1]), replace(log(hiz[,5]), hiz[,5] == 0, NA))
lights = cbind( times2(hiz[,1]), replace(log(hiz[,5]), hiz[,5] == 0, NA))
smooth.spline(lights)
lights = cbind(times2(hiz[,1]), hiz[,5])
table(lights[,2])
smooth.spline(lights, w = rep(1, nrow(lights)) - 0.9*(lights[,2] < 3))
plot(.Last.value)
points(lights, pch = ".", col=2)
plot(lights, pch = ".", col=2)
lines(smooth.spline(lights, w = rep(1, nrow(lights)) - 0.99*(lights[,2] < 3)))
?smooth.spline
lines(smooth.spline(lights, w = rep(1, nrow(lights)) - 0.99*(lights[,2] < 3) , nknots = 100))
lights[,2]
is.na(lights[,2])
which(is.na(lights[,2]))
which((lights[,2])>3)
diff(which((lights[,2])>3))
length(which((lights[,2])>3))
nrow(lights)
1:595 * 10000
sapply(1:595 * 10000, function(t) which((lights[,2])>3)  
ind
ind = which((lights[,2])>3)
sapply(1:595 * 10000, function(t) min(ind[ind > t]))
unique(sapply(1:595 * 10000, function(t) min(ind[ind > t])))
knots = unique(sapply(1:595 * 10000, function(t) min(ind[ind > t])))
is.na(lights[,2])?smooth.spline
?smooth.spline
library(pspline)
install.packages(pspline)
install.packages("pspline")
install.packages("splines")
install.packages("spline")
install.packages("splines")
?psline
library(pspline)
?pspline::
?pspline
?pspline::sm.spline
smooth.spline
smooth.spline
debug(smooth.spline)
lines(smooth.spline(lights, w = rep(1, nrow(lights)) - 0.99*(lights[,2] < 3)), col=2)
knots
?knots
?knots
unique(sapply(1:595 * 10000, function(t) min(ind[ind > t])))
knot
knots
knots[1]
ls()
knot
xbar
xbar[knots]
xbar[unique(sapply(1:595 * 10000, function(t) min(ind[ind > t])))]
length(xbar)
dim(lights)
Q
ns
library(splines)
ns
?ns
ns(lights[,2], knots = knots)
knots
ls()
objsort()
hiz
rm(hiz)
ns(lights[,2], knots = knots)
lines(ns(lights[,2], knots = knots), col=2)
dev.cur()
points(lights, pch = ".", col=2)
lights
dim(lights)
plot(lights, pch = "."
)
dev.new()
plot(lights, pch = ".")
ind
history(4000)
lines(lights[,1], predict(smooth.spline( x = lights[ind,1], y = log(lights[ind,2]), spar = 0.5), x = lights[,1])
)
Q
undebug(smooth.spline)
lines(lights[,1], predict(smooth.spline( x = lights[ind,1], y = log(lights[ind,2]), spar = 0.5), x = lights[,1]))
lines(lights[,1], predict(smooth.spline( x = lights[ind,1], y = log(lights[ind,2]), spar = 0.5), x = lights[,1])$y)
plot(lights[,1], log(lights[,2]), pch = ".")
lines(lights[,1], predict(smooth.spline( x = lights[ind,1], y = log(lights[ind,2]), spar = 0.5), x = lights[,1])$y)
 weights = 1/(0.05+  pmax((smooth.spline(lights[ind, 2], log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
 points(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2, pch = ".")
weights
weights = rep(1, length(ind))
 weights = 1/(0.05+  pmax((smooth.spline(lights[ind, 2], log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
 points(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2, pch = ".")
weights = rep(1, length(ind))
 weights = 1/(0.05+  pmax((smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
weights = rep(1, length(ind))
length(weights)
length(lights[ind,1])
length(lights[ind,2])
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2)
plot(lights[,1], log(lights[,2]), pch = ".")
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=2)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.5, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1]), col=3)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.7, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.7, w = weights), x = lights[,1]), col=4)
 weights = 1/(0.05+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.7, w= weights)$y - log(lights[ind, 2])), 0) )
lines(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.7, w = weights), x = lights[,1]), col=4)
plot( times2(lights[,1]), exp(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.7, w = weights), x = lights[,1])$y), col=4, type= "l")
plot( chron2(lights[,1]), exp(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.7, w = weights), x = lights[,1])$y), col=4, type= "l")
plot( times2(lights[,1] * 60*60*24), exp(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.7, w = weights), x = lights[,1])$y), col=4, type= "l")
plot( times2(lights[,1] * 60*60*24), exp(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1])$y), col=4, type= "l")
points( times2(lights[,1] * 60*60*24), lights[,2], pch=".", cex = 2)
 weights = 1/(0.01+  pmax((smooth.spline(x=lights[ind, 1], y= log(lights[ind, 2]), spar = 0.7, w= weights)$y - log(lights[ind, 2])), 0) )
lines( times2(lights[,1] * 60*60*24), exp(predict(smooth.spline(lights[ind, 1], log(lights[ind, 2]), spar = 0.5, w = weights), x = lights[,1])$y), col=4, type= "l")
dim(lights)
#scotlandlights
history(22000)

