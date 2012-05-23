prcomp(vibration[,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(drive[,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(walk[,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(motion[,-1])$x[,1] -> x; sum(abs(diff(x)) )/ sd(x)
prcomp(sleep[,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(sedentary[,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(sedentary[200:400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(sleep[200:400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(sedentary[200:400+100,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(sedentary[200:400+200,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(motion[200:400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(vibration[200:400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(vibration[200:400+200,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(vibration[200:400+400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(vibration[,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(drive[200:400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(drive[200:400+200,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(drive[200:400+400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(walk[200:400+400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)
prcomp(walk[200:400,-1])$x[,1] -> x; sum(abs(diff(x))) / sd(x)

svm(vibration[,-1]) -> x; sum(diff(x)^2) / var(x)
svm(drive[,-1]) -> x; sum(diff(x)^2) / var(x)
svm(walk[,-1]) -> x; sum(diff(x)^2) / var(x)
svm(motion[,-1]) -> x; sum(diff(x)^2) / var(x)
svm(sleep[,-1]) -> x; sum(diff(x)^2) / var(x)
svm(sedentary[,-1]) -> x; sum(diff(x)^2) / var(x)
svm(sedentary[200:400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(sleep[200:400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(sedentary[200:400+100,-1]) -> x; sum(diff(x)^2) / var(x)
svm(sedentary[200:400+200,-1]) -> x; sum(diff(x)^2) / var(x)
svm(motion[200:400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(vibration[200:400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(vibration[200:400+200,-1]) -> x; sum(diff(x)^2) / var(x)
svm(vibration[200:400+400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(vibration[,-1]) -> x; sum(diff(x)^2) / var(x)
svm(drive[200:400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(drive[200:400+200,-1]) -> x; sum(diff(x)^2) / var(x)
svm(drive[200:400+400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(walk[200:400+400,-1]) -> x; sum(diff(x)^2) / var(x)
svm(walk[200:400,-1]) -> x; sum(diff(x)^2) / var(x)




vibration = get.intervals(data, "15:50",30, incl = T)
drive = get.intervals(data2, "17:10",30, inc= T)
walk  = get.intervals(data, "17:04:11",30, inc= T)
#sedentary = get.intervals(data, "17:30:00",30, inc= T)
sedentary2 = get.intervals(data2, "16:30:00",60, inc= T)

sleep = get.intervals(data, "04:30:00",30, inc= T)
motion = get.intervals(data, "17:16:5",30, incl=T)

y = vibration

plot.tv <- function(y){
binsize = 100; res = NULL
for (i in 1:(nrow(y) -binsize)){
prcomp(y[i+ 1:binsize,-1])$x[,1] -> x
#svm(y[i+ 1:binsize,-1]) -> x
res = c(res, median((diff(x))^2) / var(x))
#res = c(res, acf(x,plot = F)$acf[2])
}
plot(res)
binsize = 200; res = NULL
for (i in 1:(nrow(y) -binsize)){
prcomp(y[i+ 1:binsize,-1])$x[,1] -> x
#svm(y[i+ 1:binsize,-1]) -> x
res = c(res, median((diff(x))^2) / var(x))
#res = c(res, acf(x,plot = F)$acf[2])
}
points(res, pch = 3, col = 2 )
x =  svm(y[,-1]); abline(h =  mean((diff(x))^2) / var(x), col=3)
}


