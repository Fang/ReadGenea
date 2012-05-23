svm <- function(obj){
rowSums(obj$data.out[,2:4]^2)
}

library(ReadGenea)
source("C:/Users/zhou/PortableGit/ReadGenea/Junk/helper.R")
#jogetc = read.

par(mfrow = c(4,1))
par(oma = c(5, 0.5, 3,3))
par(mar = c(1, 4, 1,1))

tmpdat = sed1sleep1nonwear; bkpoints = 0:3 /3; titles = "sed|sleep|nonwear"
tmpdat = sed1walk1run1sports; bkpoints = 0:4 /4; titles = "sed|walk|run|sports"
tmpdat = drive1ride1bus1train1walk1sed; bkpoints = 0:6 /6; titles = "drive|ride|bus|train|walk|sed"


plot(bapply.basic(svm(tmpdat), 500, mean) -> tt, type="l", yaxt = "n", ylab = "mean"); abline(v = length(tt) * bkpoints)
title(titles)

plot(bapply.basic(svm(tmpdat), 500, median) -> tt, type="l", yaxt = "n", ylab = "median"); abline(v = length(tt) * bkpoints)
plot(bapply.basic(svm(tmpdat), 500, sd) -> tt, type="l", yaxt = "n", ylab = "sd"); abline(v = length(tt) * bkpoints)
plot(bapply.basic(svm(tmpdat), 500, mad) -> tt, type="l", yaxt = "n", ylab = "mad"); abline(v = length(tt) * bkpoints)

fillplots()

rmin = function(t, th = 0.1){
quantile(t, th)
}
rmax = function(t, th = 0.1){
quantile(t, 1-th)
}
plot(bapply.basic(svm(tmpdat), 500, max) -> tt, type="l", yaxt = "n", ylab = "max"); abline(v = length(tt) * bkpoints)
title(titles)

plot(bapply.basic(svm(tmpdat), 500, rmax) -> tt, type="l", yaxt = "n", ylab = "rmax"); abline(v = length(tt) * bkpoints)
plot(bapply.basic(svm(tmpdat), 500, min) -> tt, type="l", yaxt = "n", ylab = "min"); abline(v = length(tt) * bkpoints)

plot(bapply.basic(svm(tmpdat), 500, rmin) -> tt, type="l", yaxt = "n", ylab = "rmin"); abline(v = length(tt) * bkpoints)

fillplots()

acor = function(t){
cor(t[-1], head(t, -1))
}


plot(bapply.basic(svm(tmpdat), 500, acor) -> tt, type="l", yaxt = "n", ylab = "autocorrelation"); abline(v = length(tt) * bkpoints)
title(titles)
plot(bapply.basic( (tmpdat)$dat[,3]/sqrt(svm(tmpdat)), 500, sd) -> tt, type="l", yaxt = "n", ylab = "yaxis sd"); abline(v = length(tt) * bkpoints)
plot(bapply.basic( (tmpdat)$dat[,3]/sqrt(svm(tmpdat)), 500, median) -> tt, type="l", yaxt = "n", ylab = "yaxis median"); abline(v = length(tt) * bkpoints)
#plot(abs(shift(bapply.basic(svm(tmpdat), 500, mean), -1, fill = "edge") -(bapply.basic(svm(tmpdat), 500, mean)) >  bapply.basic(svm(tmpdat), 500, sd)) -> tt, type="l", yaxt = "n", ylab = "exceedences"); abline(v = length(tt) * bkpoints)
plot( exceed(tmpdat, 500) -> tt, type="l", yaxt = "n", ylab = "exceedences"); abline(v = length(tt) * bkpoints)
fillplots()


tmpstft = stft.mv(tmpdat, 0, 1, win = 3000, svm = T)
tmpstftmv = stft.mv(tmpdat, 0, 1, win = 3000)

plot(rowSums(tmpstft$value[,tmpstft$freq >3]^2)/ rowSums(tmpstft$value[,]^2) -> tt, type="l", yaxt = "n", ylab = ">3Hz"); abline(v = length(tt) * bkpoints)
title(titles)

plot(rowSums(tmpstftmv$value[,tmpstftmv$freq <0.5]^2)/ rowSums(tmpstftmv$value[,]^2) -> tt, type="l", yaxt = "n", ylab = "<0.5Hz MV"); abline(v = length(tt) * bkpoints)
plot(rowSums(tmpstft$value[,tmpstft$freq %bt% c(0.5, 3)]^2)/ rowSums(tmpstft$value[,]^2) -> tt, type="l", yaxt = "n", ylab = "0.5Hz - 3Hz"); abline(v = length(tt) * bkpoints)
plot(rowSums(tmpstft$value[,tmpstft$freq %bt% c(0.5, 3)]^2)/ rowSums(tmpstftmv$value[,tmpstftmv$freq %bt% c(0.5, 3)]^2) -> tt, type="l", yaxt = "n", ylab = "SVM/MV", log="y"); abline(v = length(tt) * bkpoints); abline(h=1)

fillplots()
coln = function(t){
 coef(lm.fit(matrix(log(tmpstft$freq[-1]), ncol=1), t[-1]))
}
centrn = function(t){
t = t[-1]
sum(t^2 * log(tmpstft$freq[-1])) / sum(t^2)
}

entropy = function(t){
t = t/sqrt(sum(t^2))
-sum(t*log(t))
}


plot(apply( log(tmpstft$values), 1, coln) -> tt, type="l", yaxt = "n", ylab = "Noise colour"); abline(v = length(tt) * bkpoints) 
title(titles)
plot((apply( tmpstft$values, 1, centrn)) -> tt, type="l", yaxt = "n", ylab = "Centroid", ylim = log(c(0.5, 20)) ); abline(v = length(tt) * bkpoints) 
plot(log(tmpstft$freq)[(apply( tmpstftmv$values, 1, which.max)) ]-> tt, type="l", yaxt = "n", ylab = "Peak", ylim = log(c(0.01, 20))); abline(v = length(tt) * bkpoints) 
plot((apply( tmpstft$values[,-1], 1, entropy)) -> tt, type="l", yaxt = "n", ylab = "Entropy"); abline(v = length(tt) * bkpoints) 



exceed <- function(dat, window = 500){
dat1 = dat$dat[,2:4]
dat2 = shift(dat$dat[,2:4], c(window,0), fill = "edge")
means = apply(dat2, 2, function(t) bapply.basic(t, window, mean))
sds = apply(dat2, 2, function(t) bapply.basic(t, window, sd))
sds2 = apply(dat1, 2, function(t) bapply.basic(t, window, sd))
rowSums((abs( apply(dat1,2, function(t) bapply.basic(t, window, mean)) - means) > sds+sds2))
}


runmean = function(t, size = 10){
res = (cumsum(t) - shift(cumsum(t), size, fill = "edge"))/size
res[1:size] =mean(res[1:size])
res
}

plot(svm(tmpdat) > runmean(svm(tmpdat), 3000))

shift(bapply.basic(svm(tmpdat), 500, mean), -1, fill = "edge") > (bapply.basic(svm(tmpdat), 500, mean)+ bapply.basic(svm(tmpdat), 500, sd)), type="l"
