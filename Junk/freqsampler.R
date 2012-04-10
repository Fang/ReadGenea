 gainrecurse<-function(ratio,n,startgain=1.1){
 
 gain<-NULL
 
 for(i in 1:n){
         gain<-c(gain,(ratio[i]+9*ifelse(i==1,startgain,gain[i-1]))/10)
 }
 
 gain
 
 }
 
library(ReadGenea)
mum = read.bin("C:/Users/zhou/Data/mumdata__011661_2012-03-16 10-54-54.bin",start = 0.55,end = 0.79, test=F, cali=T, block  = 1000)
mum = read.bin("C:/Users/zhou/Data/mumdata__011661_2012-03-16 10-54-54.bin",start = 0.55,end = 0.79, test=F, cali=T, block  = 1000)
mum
positionals(mum, "07:00", "09:00")
positionals(mum, "16:00", "19:00")
positionals(mum, "17:30", "19:00", filter= 0)
stft.mv(mum, "17:30", "19:00", win = 3000)
plot(stft.mv(mum, "17:30", "19:00", win = 3000))
plot(stft.mv(mum, "17:30", "19:00", win = 3000), mode= "pval", top = 10)
plot(stft.mv(mum, "17:30", "19:00", win = 3000,svm = T), mode= "pval", top = 10)
plot(stft.mv(mum, "17:30", "19:00", win = 3000,svm = T), mode= "pval", top = 10, reassign = F)
plot(stft.mv(mum, "17:30", "19:00", win = 3000,svm = T), mode= "pval", top = 20, reassign = F)
plot(stft.mv(mum, "17:30", "19:00", win = 3000,svm = T), mode= "pval", reassign = F)
plot(stft.mv(mum, "17:30", "19:00", win = 3000,svm = T), mode= "pval", reassign = F)
history(200000)

#####################################################

data = read.bin("C:/Users/zhou/Data/weekandstart__011661_2012-03-22 12-20-15.bin", calibrate=T, start = 0.1,end = 0.4, blocksize=5000)
obj2 = stft.mv(data, "15:00", "16:20", svm = T, win = 3000)

res = NULL
for (thresh in 1:20 / 2){
res = rbind(res, cbind(thresh, log(rowSums(obj2$va[,obj2$freq > thresh]^2)), log(rowSums(obj2$va[,obj2$freq < thresh]^2))))
}
######


positionals(data, "15:00", "16:20", filter=0)
tmp = locator(2)$x * 60*60*24 ; a = which.max(obj2$times > tmp[1]) ; b = which.max(obj2$times > tmp[2]) ; ind = 1 + 1:length(obj2$times) %bt% c(a,b)

#####

colour = apply(obj2$va, 1, function(t) coef(lm(log(t[-1])~  log(obj2$freq[-1])  ))[2])
plot(times2(obj2$times), colour, col = ind) # noise colour
plot(times2(obj2$times),apply(obj2$va^2, 1, function(t) sum(t * obj2$frequen^2) /(sum(t))  - (sum(t * obj2$frequen) /(sum(t)))^2 ), col=ind)#sp
plot(times2(obj2$times), apply(obj2$va,1, function(t) -sum(t*log(t/sum(t)))/sum(t) /log(length(t))), col=ind) 



res = NULL
for (thresh in 1:20 / 2){
res = rbind(res, cbind(thresh, log(rowSums(obj2$va[,obj2$freq > thresh]^2)), log(rowSums(obj2$va[,obj2$freq < thresh]^2))))
}
colnames(res) = c("thresh", "greater", "less")

times2(obj2$times[c(a, b)]); plot3d(res[,1] , log(rowSums( exp (res[,2:3]))), res[,2] - res[,3]  ,col=ind , size=7)


beg = "7:00"
end = "11:20"

obj2 = stft.mv(data, beg, end, svm = F, win = 3000)
positionals(data, beg, end, filter=0)
res = NULL
for (thresh in seq.log(0.2, 15, 20)){
res = rbind(res, cbind(log(thresh), log(rowSums(obj2$va[,obj2$freq > thresh]^2)), log(rowSums(obj2$va[,obj2$freq < thresh]^2))))
}
colnames(res) = c("thresh", "greater", "less")

tmp = locator(2)$x * 60*60*24 ; a = which.max(obj2$times > tmp[1]) ; b = which.max(obj2$times > tmp[2]) ; ind = 1 + 1:length(obj2$times) %bt% c(a,b) + 2*(ind == 2)+ 3*(ind == 3); times2(obj2$times[c(a, b)]); plot3d(res[,1] , log(rowSums( exp (res[,2:3]))), res[,2] - res[,3]  ,col=ind , size=7)

bundleplot3d <- function( x, ind,...){
for (ii in which(ind == 2)){
indices = ii + (1: floor(nrow(x) / length(ind)) -1 ) * length(ind)
lines3d(x[indices,], col = ind[ii],...)
}
}

 tmp = locator(2)$x * 60*60*24 ; a = which.max(obj2$times > tmp[1]) ; b = which.max(obj2$times > tmp[2]) ; ind = 1 + 1:length(obj2$times) %bt% c(a,b) + 2*(ind == 2)+ 3*(ind == 3); times2(obj2$times[c(a, b)]); restmp = cbind(res[,1],log(rowSums( exp (res[,2:3]))), res[,2] - res[,3]); plot3d(restmp ,   ,col=ind , size=7, alpha = 0.1) ; bundleplot3d(restmp, ind, alpha = 0.5, lwd = 3)

res2 = NULL
tmp = rep( 0, nrow(obj2$va))
for (thresh in seq.log(0.2, 15, 20)){
res2 = rbind(res2, cbind(log(thresh), log(rowSums(obj2$va[,]^2)), log(rowSums(obj2$va[,obj2$freq < thresh]^2) - tmp) -log(rowSums(obj2$va[,]^2)) ))
tmp = rowSums(obj2$va[,obj2$freq < thresh]^2)
}
 tmp = locator(2)$x * 60*60*24 ; a = which.max(obj2$times > tmp[1]) ; b = which.max(obj2$times > tmp[2]) ; ind = 1 + 1:length(obj2$times) %bt% c(a,b) + 2*(ind == 2)+ 3*(ind == 3); times2(obj2$times[c(a, b)]); restmp = res2; plot3d(restmp ,   ,col=ind , size=7, alpha = 0.1) ; bundleplot3d(restmp, ind, alpha = 0.5, lwd = 3)

