#multi-accelerometer

upperarm = read.bin("C:/Users/zhou/Data/upperarm__011660_2012-01-17 15-48-06.bin", calibrate=T)
wrist = read.bin("C:/Users/zhou/Data/wrist__011661_2012-01-17 16-01-52.bin", calibrate=T)
waist = read.bin("C:/Users/zhou/Data/waist__011076_2012-01-17 15-38-37.bin", calibrate=T, start = 60000)


par(mfrow = c(2,1), mar = c(2,2,2,2))# mar =c(5, 4, 4, 2) + 0.5 )

 plot(wrist$data[-(1:8e6),3], type="l", ylim=c(-2,2))
 abline(v = which(wrist$dat[-(1:8e6), 6] != 0),col=2)
 lines(wrist$dat[  -(1:8e6),7]/2 - 15, col=2)
# lines(wrist$dat[  -(1:8e6),5]/5 - 5, col=2)
 #lines(wrist$dat[  -(1:8e6),5]/5 - 5, col=3)
 plot(upperarm$data[-(1:8e6),3], type="l", ylim = c(-2,2))

 unique(wrist$page.timestamps[floor(which(wrist$dat[,6]!= 0)/300)])
 which(wrist$page.timestamps %in% unique(wrist$page.timestamps[floor(which(wrist$dat[,6]!= 0)/300)]))

 plot(wrist$data[interval,3] -> tmp, type="l", ylim=c(-2,2))
 abline(v = which(wrist$dat[interval, 6] != 0),col=2)
 lines(wrist$dat[  interval,7]/2 - 15, col=2)
# lines(wrist$dat[  interval,5]/5 - 5, col=2)
 #lines(wrist$dat[  interval,5]/5 - 5, col=3)
 plot(upperarm$data[interval,3], type="l", ylim = c(-2,2))

tmp = rowSums(wrist$data[,2:4]^2)

library(signal)
library(e1071)
source("C:/Users/zhou/ReadGenea/R/read.bin.R")
#bf = butter(4, c(0.2, 15)/50, type="pass")
#tmp2 = filter(bf, tmp)
tmp2 = tmp

stftobj = (stft(tmp2, inc = 1024, win = 1024, coef = 512))
#z = t(scale(t(stftobj$value), center= F))

z = constrain(stftobj$value,0,100)

image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/512, z, col = gray (63:0/63), log="y")#stftobj$val)


z = t(scale(t(stftobj$value), center= F))


> curve = sin(1:length(tmp2) *2*pi/100)
> stftobj = (stft(tmp2+curve, inc = 1024, win = 1024, coef = 512))
> z = constrain(stftobj$value,0,100)
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/512, z, col = gray (63:0/63), log="y")#stftobj$val)
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/1024, z, col = gray (63:0/63), log="y")#stftobj$val)
>  which(wrist$page.timestamps %in% unique(wrist$page.timestamps[floor(which(wrist$dat[,6]!= 0)/300)]))*300
[1]  114300  114600  121500  779700 8276100 8446800 8478000 8478300 8698500
> interval = 8276100:8698500
> tmp2 = tmp[interval]
> stftobj = (stft(tmp2, inc = 1024, win = 1024, coef = 512))
> z = constrain(stftobj$value,0,100)
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/1024, z, col = gray (63:0/63), log="y")#stftobj$val)
> 
> par(mfrow = c(2,1), mar = c(2,2,2,2))# mar =c(5, 4, 4, 2) + 0.5 )
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/1024, z, col = gray (63:0/63), log="y")#stftobj$val)
> plot(tmp2)
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/1024, z, col = gray (63:0/63), log="y")#stftobj$val)
> plot(tmp2, type="l")
> z = constrain(stftobj$value,0,200)
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/1024, z, col = gray (63:0/63), log="y")#stftobj$val)
> z = constrain(stftobj$value,0,200)
> z = constrain(stftobj$value,0,50)
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/1024, z, col = gray (63:0/63), log="y")#stftobj$val)
> image(x = 0:nrow(stftobj$val) *1024/wrist$freq, y = (0:ncol(stftobj$val)+1) * wrist$freq/1024, z, col = gray (63:0/63), log="y")#stftobj$val)
> plot(tmp2, type="l")




####
plot(stft(subs(mag, 0.94,0.96), win = 1024, plot = F, coef = 512)-> stftobj, log.it = T)
plot(pmax(log(stftobj$va [181,]) , stftobj$null.logmean)/4, type="b", ylim = c(0.7, 3))
for (k in 1:10) lines(pmax(log(stftobj$va [175+k,]) , stftobj$null.logmean)/4+k/5)

 noquote(readLines("Rprofmem.out", n=5))
source("C:/Users/zhou/PortableGit/ReadGenea/ReadGenea/R/read.bin.R")

source("C:/Users/zhou/PortableGit/ReadGenea/ReadGenea/R/stft.R")

ankle10hz = read.bin("C:/Users/zhou/Data/ankle3__011660_2012-01-25 16-25-21.bin", blocksize=10000, downsample = 10, calibrate = T)
ankleend = read.bin("C:/Users/zhou/Data/ankle3__011660_2012-01-25 16-25-21.bin", start = 0.9975, end  = 0.999, calibrate = T)
wrist10hz = read.bin("C:/Users/zhou/Data/wrist3__011661_2012-01-25 16-14-57.bin", blocksize=10000, downsample = 10, calibrate = T)
wristend =  read.bin("C:/Users/zhou/Data/wrist3__011661_2012-01-25 16-14-57.bin", start = 0.9975, end  = 0.999, calibrate = T)



a = rnorm(3)
b = rnorm(3)

x = a + b*i

twist = atan( jogandstuff$dat[,4]/ jogandstuff$dat[,2])*180/pi
updown = acos(-jogandstuff$data.out[,3]/sqrt(rowSums( jogandstuff$data.out[,2:4]^2)))*180/pi

for (i in 6200:6999){
plot(jogandstuff$dat[6200:7000,3],jogandstuff$dat[6200:7000,2] , type="b")
lines(jogandstuff$dat[ i:(i+1),3],jogandstuff$dat[i:(i+1),2], col=2, lwd = 3)
}

 plot(stft( princomp( jogandstuff$dat[1:20000,2:4])$scores[,1], coef = 500, inc = 250, start.time=jogandstuff$dat[1,1], plot= F)-> stfobj, showmax = 2, log="y", xaxt="n"); axis(1, at = stfobj$time[21+0:2 * 24], label = hms (stfobj$time[21+0:2 * 24], "HMS"))

# minor.tick   ?


library(rgl)

tmp = jogandstuff[1:4000,2:4]
plot3d(tmp, xlim = c(-2,2), ylim = c(-2,2), zlim = c(-2,2)); lines3d(tmp, col=2)


