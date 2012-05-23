#require(MASS)
#set.seed(125)
#
#x <- rnorm(150,mean=3*rbinom(150,prob=.5,size=1),sd=1)
#y <- rnorm(150,mean=2*rbinom(150,prob=.5,size=2),sd=1)

kde2dplot = function(x, y, h, h.relative = 1, col = "red", add=F, drawlabels = F,n = 100, shade = T, breaks = c(0.1, 0.25, 0.5, 0.75, 0.9),...){
require(MASS)

if (missing(h)) h = c(bandwidth.nrd(x), bandwidth.nrd(y))*h.relative

d <- kde2d(x,y,h=h,n=n)
dtmp = (sort(d$z))
colr = col2rgb(col)
breaksraw = dtmp[findInterval(breaks, cumsum(dtmp)/sum(dtmp))]
if (shade) image(d, col = rgb(colr[1], colr[2], colr[3], alpha = c(breaks^2,1) * 0.3*255, maxColorValue = 255), breaks = c(0,breaksraw, max(dtmp)), add = add, ...)
contour(d,add=T,levels=  breaksraw , col=col, drawlabels = drawlabels,...)
invisible(d)
}
###################
#
#x <- rnorm(150,mean=3*rbinom(150,prob=.5,size=1),sd=1)
#y <- rnorm(150,mean=2*rbinom(150,prob=.5,size=2),sd=1)
#kde2dplot(x,y, h.re = 1)#, ylim = c(-2, 10))
#
#
#x <- rnorm(150,mean=3*rbinom(150,prob=.5,size=1),sd=1)
#y <- rnorm(150,mean=2*rbinom(150,prob=.5,size=2),sd=1)
#kde2dplot(x,y+2, add=T, col="blue", h.re = 1)
#
#
#
#########################
#
#
#
#thigh = read.bin("C:/Users/zhou/Desktop/kl_left thigh_011352_2012-04-20 21-32-52.bin", cali = T)
#wrist = read.bin("C:/Users/zhou/Desktop/kl_right wrist_011345_2012-04-20 21-33-34.bin", cali =T)
#
#
#stand = get.intervals(wrist,"20:05","20:25")
#sit = get.intervals(wrist,"16:10","16:30")
#
#epoch = 1000
# x = sit[,2]
# y = rowSums(sit^2)
## points(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sum)), col=2)
# kde2dplot(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), add = F, col=2, xlim = c(-1, 1) * epoch, ylim = c(-5, 1.5), ylab = "log sd svm", xlab = "sum wrist y", h.relative = 0.9)
#points(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), col=2)
# x = stand[,2]
# y = rowSums(stand^2)
## points(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sum)), col=2)
# kde2dplot(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), add = T, col="blue", h.relative= 0.9)
# points(bapply.basic(x, epoch, sum), log(bapply.basic(y, epoch, sd)), col="blue")
#
#legend(500, -4, leg= c("sit", "stand"), fill = c("red", "blue"))
#
