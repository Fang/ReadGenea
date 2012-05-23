plot3d(josswrist10hz$dat[1:0.5e4 + 1.5e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:0.5e4 + 1.5e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:0.5e4 + 1.6e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:1e4 + 1.6e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:5e4 + 1.6e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:5e4 + 1.6e6,2:4]-> tmp); #lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:5e2 + 1.6e6,2:4]-> tmp); #lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:5e2 + 1.6e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(cbind(1:500, tmp[,-2]) -> tmp2); lines3d(tmp2, col=2)
plot3d(josswrist10hz$dat[1:5e2 + 1.6e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:0.5e4 + 1.5e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(josswrist10hz$dat[1:0.5e4 + 1.5e6,2:4]-> tmp); lines3d(tmp[1:500,], col=2)
plot3d(josswrist10hz$dat[1:0.5e4 + 1.6e6,2:4]-> tmp); lines3d(tmp[1:500,], col=2)
plot3d(josswrist10hz$dat[1:0.5e4 + 1.6e6,2:4]-> tmp); lines3d(tmp[1:5000,], col=2)
plot3d(josswrist10hz$dat[1:0.5e4 + 1.6e6,2:4]-> tmp); lines3d(tmp[1:1000,], col=2)
plot3d(cbind(1:500, tmp[,-2]) -> tmp2); lines3d(tmp2, col=2)
plot3d(josswrist10hz$dat[1:5e2 + 1.6e6,2:4]-> tmp); lines3d(tmp, col=2)
plot3d(cbind(1:500, tmp[,-2]) -> tmp2); lines3d(tmp2, col=2)
?plot3d
plot3d(cbind(1:500, tmp[,-2]) -> tmp2, aspect  = T); lines3d(tmp2, col=2)
plot3d(cbind(1:500, tmp[,-2]) -> tmp2, aspect  = T); lines3d(tmp2, col=2, aspect=T)
plot3d(cbind(1:500, tmp[,-2]) -> tmp2, aspect  = F); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500, tmp[,-2]) -> tmp2, aspect  = F, ylim = c(-2,2), zlim = c(-2,2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500, scale(tmp[,-2]), scale=F) -> tmp2, aspect  = F, ylim = c(-2,2), zlim = c(-2,2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500, scale(tmp[,-2]), scale=F) -> tmp2, aspect  = F, xlim = c(0, 501), ylim = c(-2,2), zlim = c(-2,2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500, scale(tmp[,-2], scale=F)) -> tmp2, aspect  = F, xlim = c(0, 501), ylim = c(-2,2), zlim = c(-2,2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500, scale(tmp[,-2], scale=F)) -> tmp2, aspect  = F, xlim = c(0, 501), ylim = c(-0.2,0.2), zlim = c(-0.2,0.2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500, scale(tmp[,-2], scale=F))[,3:1] -> tmp2, aspect  = F, xlim = c(0, 501), ylim = c(-0.2,0.2), zlim = c(-0.2,0.2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500, scale(tmp[,-2], scale=F))[,3:1] -> tmp2, aspect  = T, xlim = c(0, 501), ylim = c(-0.2,0.2), zlim = c(-0.2,0.2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500/500, scale(tmp[,-2], scale=F))[,3:1] -> tmp2, aspect  = F, xlim = c(0, 501), ylim = c(-0.2,0.2), zlim = c(-0.2,0.2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500/500, scale(tmp[,-2], scale=F))[,3:1] -> tmp2, aspect  = F, xlim = c(0, 501), ylim = c(-0.2,0.2), zlim = c(-0.2,0.2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500/500, scale(tmp[,-2], scale=F))[,3:1] -> tmp2, aspect  = F,  ylim = c(-0.2,0.2), zlim = c(-0.2,0.2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500/500, scale(tmp[,-2], scale=F))[-(1:100),3:1] -> tmp2, aspect  = F,  ylim = c(-0.2,0.2), zlim = c(-0.2,0.2)); lines3d(tmp2, col=2, aspect=F)
plot3d(cbind(1:500/500, scale(tmp[,-2], scale=F))[-(1:100),3:1] -> tmp2, aspect  = F); lines3d(tmp2, col=2, aspect=F)
tmp = josswrist10hz$dat[1:1e4 + 1.6e6,4]
plot(tmp)
plot(stft(tmp, win = 600, coef = 300))
plot(stft(tmp, win = 600, coef = 300), mode="pval")
plot(stft(tmp, win = 600, coef = 300), type="pval")
plot.stft
source("C:/Users/zhou/PortableGit/ReadGenea/ReadGenea/R/stft.R")
plot(stft(tmp, win = 600, coef = 300), type="pval")
plot(stft(tmp, win = 600, coef = 300),mode="pval")
plot(stft(tmp, win = 600, coef = 300, freq = 10),mode="pval")
plot(stft(tmp, win = 600, coef = 300, freq = 10, inc = 100),mode="pval")
plot(stft(tmp, win = 600, coef = 300, freq = 10, inc = 300),mode="pval")
plot(stft(tmp, win = 600, coef = 300, freq = 10, inc = 300),mode="log")
plot(stft(tmp, win = 600, coef = 300, freq = 10, inc = 300),mode="pval")
tmp = josswrist10hz$dat[1:1e4 + 1.6e6,2:4]
prcomp(tmp)
prcomp(tmp)
str(prcomp(tmp))
prcomp(tmp)$x[,1]
plot(prcomp(tmp)$x[,1])
plot(prcomp(tmp)$x[,2])
plot(prcomp(tmp)$x)
plot(prcomp(tmp)$1)
plot(prcomp(tmp)$x[,1])
plot(stft(prcomp(tmp)$x[,1], win = 600, inc = 300, coef = 300), mode="pval")
plot(stft(prcomp(tmp)$x[,2], win = 600, inc = 300, coef = 300), mode="pval")
plot(stft(prcomp(tmp)$x[,3], win = 1200, inc = 300, coef = 300), mode="pval")
plot(stft(prcomp(tmp)$x[,3], win = 1200, inc =200, coef = 300), mode="pval")
plot(stft(prcomp(tmp)$x[,2], win = 600, inc = 300, coef = 300), mode="pval", log="y")
plot(stft(prcomp(tmp)$x[,2], win = 600, inc = 300, coef = 300, freq = 10), mode="pval", log="y")
dim(josswrist10hz$dat)
range((1:1e4 + 1.6e6) / 604800)
josswristslep= read.bin("C:/Users/zhou/Data/jl_left wrist_010703_2012-01-20 20-30-16.bin", calibrate=T, downsample=1, start = 0.26,end = 0.27, blocksize=10000)
josswristslep
plot(josswristslep$dat[,2])
plot(josswristslep$dat[,2] -> tmp)
plot(stft(tmp, win = 18000, inc = 6000, coef = 9000), mode="pval", log="y")
plot(stft(tmp, win = 6000, inc = 3000, coef = 3000), mode="pval", log="y")
plot(josswristslep$dat[,3] -> tmp)
plot(stft(tmp, win = 6000, inc = 3000, coef = 3000), mode="pval", log="y")
plot(stft(tmp, win = 6000, inc = 6000, coef = 3000), mode="pval", log="y")
plot(josswristslep$dat[,5] -> tmp)
plot(josswristslep$dat[,4] -> tmp, type="l")
plot(stft(tmp, win = 6000, inc = 6000, coef = 3000), mode="pval", log="y")
plot.stft
plot(stft(tmp, win = 6000, inc = 6000, coef = 3000), mode="modulus", log="y")
plot(stft(tmp, win = 6000, inc = 6000, coef = 3000), mode="pval", log="y")
plot(stft(tmp, win = 6000, inc = 6000, coef = 3000)-> stftobj, mode="pval", log="y")
stftobj$va
dim(stftobj$va)
apply(stftobj$va, 2, function(t) sum(t^2))
plot(apply(stftobj$va, 2, function(t) sum(t^2)))
plot(apply(stftobj$va, 1, function(t) sum(t^2)))
plot(apply(stftobj$va, 1, function(t) sum(t^2)), log="y")
which(apply(stftobj$va, 1, function(t) sum(t^2)) < 5e5)
ind = which(apply(stftobj$va, 1, function(t) sum(t^2)) < 5e5)
stftobj$va[ind,]
dim(stftobj$va[ind,])
colMeans(stftobj$va[ind,])
plot(colMeans(stftobj$va[ind,]))
plot(stftobj$freq, colMeans(stftobj$va[ind,]), log= "x")
plot(stftobj$freq, colMeans(stftobj$va[ind,]^2), log= "x")
plot(stftobj$freq, colMeans(stftobj$va[ind,]^2), log= "xy")
for (k in ind) lines (stftobj$freq, colMeans(stftobj$va[k,]^2), log= "xy")
for (k in ind) lines (stftobj$freq, (stftobj$va[k,]^2), log= "xy")
repeatplot(stftobj$freq, (stftobj$va[ind,]^2), log= "xy" , mar = 2, type="l")
repeatplot(stftobj$freq, (stftobj$va[ind,]^2), log= "xy" , mar = 1, type="l")
repeatplot(stftobj$freq, (stftobj$va[ind,]^2),  mar = 1, type="l")
dim(stftobj$va)
dim(stftobj$freq)
len](stftobj$freq)
length(stftobj$freq)
repeatplot(stftobj$freq, (stftobj$va[ind,]^2),  mar = 2, type="l")
repeatplot(stftobj$freq, (stftobj$va[ind,]^2),  mar = 1, type="l")
repeatplot(stftobj$freq, (stftobj$va[ind,]^2),  mar = 1, type="l", log="y")
repeatplot(stftobj$freq, (stftobj$va[ind,]^2),  mar = 1, type="l", log="x")
stftobj$freq
repeatplot(stftobj$freq+0.01, (stftobj$va[ind,]^2),  mar = 1, type="l", log="x")
order(apply(stftobj$va, 1, function(t) sum(t^2)))
ind = order(apply(stftobj$va, 1, function(t) sum(t^2)))
repeatplot(stftobj$freq, (stftobj$va[ind,]^2),  mar = 1, type="l", log="x")
repeatplot(stftobj$freq+0.01, (stftobj$va[ind,]^2),  mar = 1, type="l", log="x")
repeatplot(stftobj$freq+0.1, (stftobj$va[ind,]^2),  mar = 1, type="l", log="x")
repeatplot(stftobj$freq+0.1, (stftobj$va[ind[-(1:20),]^2),  mar = 1, type="l", log="x")
repeatplot(stftobj$freq+0.1, (stftobj$va[ind[-(1:20)],]^2),  mar = 1, type="l", log="x")
repeatplot(stftobj$freq+0.1, (stftobj$va[ind[(1:50)],]^2),  mar = 1, type="l", log="x")
repeatplot(stftobj$freq+0.1, (stftobj$va[ind[(1:80)],]^2),  mar = 1, type="l", log="x")
apply(stftobj$va, 2, median)
plot(apply(stftobj$va, 2, median))
plot(stftobj$freq + 0.1,apply(stftobj$va, 2, median, log="x")
)
plot(stftobj$freq + 0.1,apply(stftobj$va, 2, median), log="x", type="l")
plot(josswristslep$dat[,2:4] -> tmp)
plot(josswristslep$dat[,2:4] -> tmp, pch=".")
plot(stft(rowSums(tmp^2), win = 6000, inc = 6000, coef = 3000)-> stftobj, mode="pval", log="y")
plot(stftobj$freq + 0.1,apply(stftobj$va, 2, median, log="x")
)
plot(stftobj$freq + 0.1,apply(stftobj$va, 2, median), log="x")
plot(stft( (tmp[,3]), win = 6000, inc = 6000, coef = 3000)-> stftobj, mode="pval", log="y")
plot(stftobj$freq + 0.1,apply(stftobj$va, 2, median), log="x")
plot3d(tmp)
examples(sprites3d)
example(sprites3d)
dim(tmp)
dim(josswristslep$da)
plot3d(tmp)
aspect3d("iso")
grid3d(at = c(0,0,0))
grid3d(side = c("x","y","z"),at = c(0,0,0))
lines3d(tmp, col=2)
plot3d(tmp)
grid3d(side = c("x","y","z"),at = c(0,0,0))
play3d(f = function(t) plot3d(tmp[t*1000 +1:1000,]))
par3d(NULL)
par3d(par3d())
tempf = function(t){
 plot3d(tmp[t*1000 +1:1000,])
return(NULL)
}
play3d(f = tempf)
?points3d
tempf = function(t){
plot3d(tmp, size = 1)
points3d(tmp[t*1000 +1:1000,], size = 3, col=2)
return(NULL)
}
play3d(f = tempf)
aspect3d("iso")
play3d(f = tempf)
grid3d(side = c("x","y","z"),at = c(0,0,0))
tempf = function(t){
for (t in 1:floor(nrow(tmp)/100)){
grid3d(side = c("x","y","z"),at = c(0,0,0))
plot3d(tmp, size = 1)
points3d(tmp[t*100 +1:1000,], size = 3, col=2)
readLine("stdin", n=1)
}
return(NULL)
}
tempf(1)
readLines
tempf = function(t){
for (t in 1:floor(nrow(tmp)/100)){
grid3d(side = c("x","y","z"),at = c(0,0,0))
plot3d(tmp, size = 1)
points3d(tmp[t*100 +1:1000,], size = 3, col=2)
readLines("stdin", n=1)
}
return(NULL)
}
tempf(1)
plot3d(tmp)
plot3d(tmp)
plot3d(tmp)
aspect3d("iso")
utils:::menuInstallPkgs()
tempf = function(inc = 100, win = 1000){
for (t in 1:floor(nrow(tmp)/100)){
grid3d(side = c("x","y","z"),at = c(0,0,0))
plot3d(tmp, size = 1)
points3d(tmp[t*inc +1:win,], size = 3, col=2)
mywait()
}
return(NULL)
}
library(tcltk2)
mywait <- function() {
    tt <- tktoplevel()
    tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
        side='bottom')
    tkbind(tt,'<Key>', function()tkdestroy(tt) )
    tkwait.window(tt)
}
tempf()
tkdestroy(tt)
objsort()
tempf = function(inc = 100, win = 1000){
for (t in 1:floor(nrow(tmp)/100)){
grid3d(side = c("x","y","z"),at = c(0,0,0))
plot3d(tmp, size = 1)
points3d(tmp[t*inc +1:win,], size = 3, col=2)
browser()
}
return(NULL)
}
tempf()
c
c
c
Q
tempf(1000)
c
c
c
c
c
c
Q
tempf(6000)
c
c
c
c
c
c
c
c
c
Q
aspect3d("iso")
u  = rnorm(3)
v = rnorm(3)
x = u + v*i
x = u + vi
I
i
?i
complex
?complex
u  = rnorm(3)
v = rnorm(3)
x = u + v*i
u  = rnorm(3)
v = rnorm(3)
x = complex(u, v)
x
v
x = complex(real = u, imag= v)
x
u%*% u
t(u)%*% u
u %*% matrix(u,3)
u %*% matrix(u,1)
u %*% matrix(u,1) + u %*% matrix(u,1)
u %*% matrix(u,1) + v %*% matrix(v,1)
eigen(u %*% matrix(u,1) + v %*% matrix(v,1))
eigen(u %*% matrix(u,1) + v %*% matrix(v,1))[,3]
eigen(u %*% matrix(u,1) + v %*% matrix(v,1))$vec[,3]
x
Mod(x)
u
v
u %*% matrix(u,1)
0.3*0.88
eigen(u %*% matrix(u,1) + v %*% matrix(v,1))$vec[,3]
eigen(u %*% matrix(u,1) + v %*% matrix(v,1))$vec[3,]
eigen(u %*% matrix(u,1) + v %*% matrix(v,1))
eigen(u %*% matrix(u,1) + v %*% matrix(v,1) -> M)
m $*$  eigen(u %*% matrix(u,1) + v %*% matrix(v,1) -> M)
m $*$  eigen(u %*% matrix(u,1) + v %*% matrix(v,1) -> M)$vec[,3]
m %*%  eigen(u %*% matrix(u,1) + v %*% matrix(v,1) -> M)$vec[,3]
M %*%  eigen(u %*% matrix(u,1) + v %*% matrix(v,1) -> M)$vec[,3]
x
vec = eigen(u %*% matrix(u,1) + v %*% matrix(v,1) -> M)$vec[,3]
vec
sum(vec^2)
vec * complex
vec * x
(sum(vec * x))
x
v
vec = eigen(u %*% matrix(u,1) +0* v %*% matrix(v,1) -> M)$vec[,3]
vec
eigen(M)
spectralpow = function(a,u, v){
if (a^2+ b^2 > 1) return(Inf)
a = c(a,sqrt(1 - a[1]^2 - a[2]^2))
sum(a*u)^2 + sum(a*v)^2
}
optim((0.1,0.1),spectralpow,u=u,v=v))
optim(c(0.1,0.1),spectralpow,u=u,v=v))
optim(c(0.1,0.1),spectralpow,u=u,v=v)
spectralpow = function(a,u, v){
if (a[1]^2+ a[2]^2 > 1) return(Inf)
a = c(a,sqrt(1 - a[1]^2 - a[2]^2))
sum(a*u)^2 + sum(a*v)^2
}
optim(c(0.1,0.1),spectralpow,u=u,v=v)
spectralpow = function(a,u, v){
if (a[1]^2+ a[2]^2 > 1) return(-Inf)
a = c(a,sqrt(1 - a[1]^2 - a[2]^2))
sum(a*u)^2 + sum(a*v)^2
}
optim(c(0.1,0.1),spectralpow,u=u,v=v)
vec = optim(c(0.1,0.1),spectralpow,u=u,v=v)$par
sum(vec^2)
u
v
?optim
spectralpow = function(a,u, v){
if (a[1]^2+ a[2]^2 > 1) return(Inf)
a = c(a,sqrt(1 - a[1]^2 - a[2]^2))
-sum(a*u)^2 + sum(a*v)^2
}
optim(c(0.1,0.1),spectralpow,u=u,v=v)
spectralpow = function(a,u, v){
if (a[1]^2+ a[2]^2 > 1) return(Inf)
a = c(a,sqrt(1 - a[1]^2 - a[2]^2))
-sum(a*u)^2 - sum(a*v)^2
}
optim(c(0.1,0.1),spectralpow,u=u,v=v)
x
vec = optim(c(0.1,0.1),spectralpow,u=u,v=v)$par
a = optim(c(0.1,0.1),spectralpow,u=u,v=v)$par
a = c(a,sqrt(1 - a[1]^2 - a[2]^2))
a
a
a * x
sum(a * x)
Mod(sum(a * x))
Mod(sum(a * x))^2
Mod(x)^2
a = optim(c(0.1,0.1),spectralpow,u=u,v=v)$par'  c(a,sqrt(1 - a[1]^2 - a[2]^2))
a = optim(c(0.1,0.1),spectralpow,u=u,v=v)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))
a = optim(c(0.3,0.1),spectralpow,u=u,v=v)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))
a = optim(c(0.3,0.1),spectralpow,u=u,v=0)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))
a = optim(c(0.3,0.1),spectralpow,u=0,v=1)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))
a = optim(c(0.3,0.1),spectralpow,u=0,v=v)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))
a = optim(c(0.3,0.1),spectralpow,u=u,v=0)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))-> g
a = optim(c(0.3,0.1),spectralpow,u=0,v=v)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))-> h
g
h
a = optim(c(0.3,0.1),spectralpow,u=u,v=v)$par;  c(a,sqrt(1 - a[1]^2 - a[2]^2))-> q
q
h
z
g
h
print(q)  ;  print( (g + 0.1*h)/ sum((g+0.1*h)^2))
alph = 0.1; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 0.2; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 1; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 0.4; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 0.5; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 0.6; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 0.55; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 0.55; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2))
alph = 0.55; print(q)  ;  print( (h+alph*g)/ sum((h + alph*g)^2)-> p)
sum( p * x)
Mod(sum( p * x))
Mod(sum( p * x))^2
interval = c(0.1, 0.15); ind = subs(1:nrow(nadankle$dat), interval[1], interval[2]) ;# plot(times2(nadankle$dat[ind,1]),nadankle$dat[ind,2], type = "l"); 
library(HiddenMarkov)
install.packages("HiddenMarkov")
library(HiddenMarkov)
demo("beta", package = "HiddenMarkov")
demo("norm", package = "HiddenMarkov")
demo("norm", package = "HiddenMarkov")
 y <- BaumWelch(x)
x
 y
x
?BaumWelch(x)
?dthmm
x
str(x)
x2 = x
x2$x = c(rnorm(400), 2*rnorm(800)  + 0.1)
 y <- BaumWelch(x)
y
str(y)
str(x)
plot(y$y)
 y <- BaumWelch(x2)
warnings()
str(x2)
str(y)
x2$x = c(rnorm(400), 2*rnorm(600)  + 0.1)
 y <- BaumWelch(x2)
plot(y$y)
y
str(y)
 y <- BaumWelch(x2)
 y <- BaumWelch(x2)
 y <- BaumWelch(x2)
 y <- BaumWelch(y)
plot(y)
plot(y)
x2$x = c(rnorm(400), 2*rnorm(600)  + 2)
 y <- BaumWelch(x2)
plot(y)
plot(y$x)
x2$x = c(rnorm(400), rnorm(600)  + 2)
 y <- BaumWelch(x2)
plot(y$x)
plot(y$y)
x2$x = c(rnorm(400), rnorm(600)*10)
 y <- BaumWelch(x2)
 y <- BaumWelch(x2)
plot(y$y)
x2
x2$y = rep(1, 1000)
 y <- BaumWelch(x2)
plot(y$y)
x2$y = NULL
 y <- BaumWelch(x2)
plot(y$y)
x3 = dthmm(x$x)
x3 = dthmm(x$x, distn = "norm")
?simulate
?BaumWelch
x2
x2$x
plot(x2$x)#
plot(x2$x)
x2$y = abs(x2$x) < mean(abs(x2$x))  + 1
x2$y
 y <- BaumWelch(x2)
plot(y)
plot(y$y)
x2$x
str(x2)
str(y)
x2$y = (abs(x2$x) < mean(abs(x2$x)))  + 1
 y <- BaumWelch(x2)
str(y)
str(x2)
?dthmm
x2$pn = NULL
x2$pm = list(sd = c(1,3), mean = c(0,0))
 y <- BaumWelch(x2)
y
str(y)
x2$delta = c(0.5,0.5)
 y <- BaumWelch(x2)
str(y)
str(x2)
x2$y = abs(x2$x) > mean(abs(x2$x))  + 1
 y <- BaumWelch(x2)
str(y)
str(x2)
x2$y = (abs(x2$x) > mean(abs(x2$x)))  + 1
 y <- BaumWelch(x2)
str(x2)
str(y)
plot(y)
plot(y$y)
x2$x = c(rnorm(400), rnorm(600)+3)
 y <- BaumWelch(x2)
plot(y$y)
str(y)
plot(y$y, type = "l")
plot(cumsum(y$y-1), type = "l")
abline(0,1)
?BaumWelch
y$u
plot(y$u)
plot(y$u[1,])
plot(y$u[,1])
plot(y$v[,1])
plot(y$v[,1,1])
?dthmm
x2$y = NULL
 y <- BaumWelch(x2)
plot(y)
str(y)
simulate(y)
simulate(y, nsim = 10)
y$delta = c(0.99, 0.01)
simulate(y, nsim = 10)
plot(simulate(y, nsim = 1000))
plot(simulate(y, nsim = 1000))
plot(simulate(y, nsim = 1000)$y)
plot(simulate(y, nsim = 1000)$y)
plot(simulate(y, nsim = 1000)$y)
str(y)
?BaumWelch
library(help = HiddenMarkov)
?Estep
Viterbi(x2)
dev.new() ; plot(Viterbi(x2))
 plot(Viterbi(y))
Viterbi
?Viterbi
?Viterbihmm
history(340343)
ls()
history(340343)

