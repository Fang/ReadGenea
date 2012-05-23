#helper function for epoch aggregation - take every k items from X, and compute FUN, then put it all as one big vector
bapply.basic <- function(X, k, FUN) { res = rep(0, floor(length(X) / k)); for (i in 1:floor(length(X)/k)) res[i] = FUN(X[ (i-1)*k + 1:k]); return(res)}

#Function for 2D KDE plotting
#Arguments:
#x,y - two variable input
#h.relative - a smoothing parameter. 1 = lots of smoothing, 0 = no smoothing
#col - colour of plot
#add - add to an existing plot? If FALSE, create a new plot
#drawlabels - have labels on the contour?
kde2dplot = function(x, y, h, h.relative = 1, col = "red", add=F, drawlabels = F, ...){
require(MASS)

if (missing(h)) h = c(bandwidth.nrd(x), bandwidth.nrd(y))*h.relative

d <- kde2d(x,y,h=h,n=100)
dtmp = (sort(d$z))
colr = col2rgb(col)
breaks = dtmp[findInterval(c(0.1, 0.25, 0.5, 0.75, 0.9), cumsum(dtmp)/sum(dtmp))]
image(d, col = rgb(colr[1], colr[2], colr[3], alpha = c(0, 0.03, 0.05, 0.1, 0.2, 0.3)*255, maxColorValue = 255), breaks = c(0,breaks, max(dtmp)), add = add, ...)
contour(d,add=T,levels=  breaks , col=col, drawlabels = drawlabels,...)
invisible(d)
}
###################


#read in data
# CHANGE THESE PATHS
thigh = read.bin("C:/Users/zhou/Desktop/kl_left thigh_011352_2012-04-20 21-32-52.bin", cali = T)
wrist = read.bin("C:/Users/zhou/Desktop/kl_right wrist_011345_2012-04-20 21-33-34.bin", cali =T)

#get relevant segments
stand = get.intervals(wrist,"20:05","20:25")
sit = get.intervals(wrist,"16:10","16:30")

#epoch length in measurements
epoch = 1000
# x = Y-axis
 x = sit[,2]
# y = svm
 y = rowSums(sit^2)

 kde2dplot(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), add = F, col=2, xlim = c(-1, 1) * epoch, ylim = c(-5, 1.5), ylab = "log sd svm", xlab = "sum wrist y", h.relative = 0.5)
points(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), col=2)

 x = stand[,2]
 y = rowSums(stand^2)
 kde2dplot(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), add = T, col="blue", h.relative= 0.5)
 points(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), col="blue")

legend(500, -4, leg= c("sit", "stand"), fill = c("red", "blue"))

