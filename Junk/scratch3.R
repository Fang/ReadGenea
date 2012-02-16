stftobj$val


#agglomerate  12  chunks

res = apply(matrix(t(stftobj$va), nrow = 12 * ncol(stftobj$va)),2, function(t) apply(matrix(t, ncol = 12 ),1, median))

image(y = stftobj2$freq + 0.1, constrain(t(apply(stftobj2$values, 1, function(t)  pexp(t^2, 1/mean(t^2)))), 0.9,1  ), log = "y")


image(y = stftobj$freq + 0.1, constrain(t(apply(res, 2, function(t)  pexp(t^2, 1/mean(t^2)))), 0.9,1  ), log = "y", col = gray (63:0/63))


 offset = 0.1; plot3d( tmps[subs(1:50200, 0+offset, 0.05+offset),] -> tmp3); lines3d(tmp3, col=heat.colors(nrow(tmp3))); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0)); abline(v = offset, col=2) ; print(nrow(tmp3) / 6000)



 offset = 0;leng = 0.05; plot3d( tmps[subs(1:nrow(tmps), 0+offset, 0.05+offset),] -> tmp3); lines3d(tmp3, col=heat.colors(nrow(tmp3))); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0)); plot(1:nrow(tmps)/nrow(tmps), tmps[,1]);  abline(v = offset+c(0, leng), col=2) ; print(nrow(tmp3) / 6000)

> offset = 0.8153;leng = 0.0005; plot3d( tmps[subs(1:nrow(tmps), 0+offset, leng+offset),] -> tmp3); lines3d(tmp3, col=heat.colors(nrow(tmp3))); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0)); plot(1:nrow(tmps)/nrow(tmps), tmps[,1], type="l");  abline(v = offset+c(0, leng), col=2) ; print(nrow(tmp3) / 100)

offset = 0.76;leng = 0.005; plot3d( tmps[subs(1:nrow(tmps), 0+offset, leng+offset),] -> tmp3); lines3d(tmp3, col=heat.colors(nrow(tmp3))); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0)); plot(1:nrow(tmps)/nrow(tmps), tmps[,3], type="l");  abline(v = offset+c(0, leng), col=2) ; print(nrow(tmp3) / 100)



offset = 0.81;leng = 0.01; plot3d( tmps[subs(1:nrow(tmps), 0+offset, leng+offset),] -> tmp3); lines3d(tmp3, col=heat.colors(nrow(tmp3))); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0)); plot(1:nrow(tmps)/nrow(tmps), tmps[,2], type="l");  abline(v = changepoints2, col=3);abline(v = offset+c(0, leng), col=2) ; print(nrow(tmp3) / 100)



 plot(stft(tmps[,1], win = 3000, inc = 1500), mode= "pval", median = T)

abline(v = 26730 * changepoints2, col=2)


stddevs = apply(tmps, 2, function(t) sd(matrix(t, nrow = 6000) ))
stddevout = GFLasso(stddevs,0.2)
abline(v = which(diff(stddevout[,1])!= 0)/ nrow(stddevs) -> changepoints2, col=5)

nadankle = read.bin ("C:/Users/zhou/Desktop/Running data/Nad_right ankle_011153_2011-10-09 11-24-20.bin", cali = T)
nadwrist = read.bin ("C:/Users/zhou/Desktop/Running data/Nad_right wrist_011152_2011-10-09 11-24-03.bin", cali = T)


library(rgl)
library(princurve)

interval = c(0.1, 0.15); ind = subs(1:nrow(nadwrist$dat), interval[1], interval[2]) ;# plot(times2(nadwrist$dat[ind,1]),nadwrist$dat[ind,2], type = "l"); 

subind = subs(ind, 0.1, 0.2); plot( times2(nadwrist$dat[ind,1]),  nadwrist$dat[ind, 2], type="l"); abline(v = range(times2(nadwrist$dat[subind,1])), col = 2); tmp3 = nadwrist$dat[subind,2:4] ; plot3d( tmp3);lines3d(tmp3, col=heat.colors(nrow(tmp3))); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0))


interval = c(0.1, 0.15); ind = subs(1:nrow(nadankle$dat), interval[1], interval[2]) ;# plot(times2(nadankle$dat[ind,1]),nadankle$dat[ind,2], type = "l"); 

subind = subs(ind, 0, 1);  tmp3 = nadankle$dat[subind,2:4] ; plot3d( tmp3);lines3d(tmp3, col=heat.colors(nrow(tmp3))); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0))

############## principal curve plotting

subind = subs(ind, 0.1, 0.2); plot( times2(nadwrist$dat[ind,1]),  nadwrist$dat[ind, 2], type="l"); abline(v = range(times2(nadwrist$dat[subind,1])), col = 2); tmp3 = nadwrist$dat[subind,2:4] ; plot3d( tmp3, col = 1+ (1:nrow(tmp3) < 160) );lines3d(tmp3, col=1); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0))

obj = principal.curve(tmp3, start = tmp3[1:100,], stretch = 0.3, smoother = "periodic.lowess")#stretch = 0.05, thresh = 0.001)
lines3d(obj$s, col=3)




dwr = 2; dan = 4;offset = 35;37*2-> a;plot3d(subind %% a,nadwrist$dat[subind+offset, dwr], nadankle$dat[subind+offset,dan]); for (k in 1:floor(length(subind)/a)) lines3d(1:a - 1, nadwrist$dat[subind[1:a + (k-1)*a]+offset, dwr],  nadankle$dat[subind[1:a + (k-1)*a]+offset, dan], col=2)


interval = c(0.1, 0.15); ind = subs(1:nrow(nadankle$dat), interval[1], interval[2]) ;# plot(times2(nadankle$dat[ind,1]),nadankle$dat[ind,2], type = "l"); 

subind = subs(ind, 0, 0.1)
plot3d(nadwrist$dat[subind,2:4], col = heat.colors(79)[  round((abs( nadankle$dat[subind,2])) * 10) + 1 ]); lines3d(nadwrist$dat[subind,2:4], col = heat.colors(79)[  round((abs( nadankle$dat[subind,2])) * 10) + 1 ]); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.1, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0))

persp3d(y = constrain(log(stftobj$frequency), -3, Inf) ,z = (pmax(tmp, stftobj$null.logmean * 10)), alpha =1, col=(cols), front = "line", back="line")



> colorlut = terrain.colors ( diff(range(tmp)) + 1)
> cols = colorlut[t(tmp) - min(tmp)]
> persp3d(y = constrain(log(stftobj$frequency), -3, Inf) ,z = log10(pmax(tmp, stftobj$null.logmean * 10)), alpha =1, col=(cols), front = "line", back="line")
> cols = colorlut[t(tmp) - min(tmp)+1]
> cols = colorlut[(tmp) - min(tmp)+1]
> persp3d(y = constrain(log(stftobj$frequency), -3, Inf) ,z = log10(pmax(tmp, stftobj$null.logmean * 10)), alpha =1, col=(cols), front = "line", back="line")
> colorlut = terrain.colors ( diff(range(log10(pmax(tmp, stftobj$null.logmean * 10)))) + 1)
> cols = colorlut[(log10(pmax(tmp, stftobj$null.logmean * 10))) - min(log10(pmax(tmp, stftobj$null.logmean * 10)))+1]
> persp3d(y = constrain(log(stftobj$frequency), -3, Inf) ,z = log10(pmax(tmp, stftobj$null.logmean * 10)), alpha =1, col=(cols), front = "line", back="line")
> colorlut = terrain.colors ( diff(range(log10(pmax(tmp, stftobj$null.logmean * 10)))) + 1)
> colorlut
[1] "#00A600FF" "#F2F2F2FF"
> diff(range(log10(pmax(tmp, stftobj$null.logmean * 10)))) + 1
[1] 2.49726430013
> colorlut = terrain.colors ( diff(range(log10(pmax(tmp, stftobj$null.logmean * 10))))*10 + 1)
> cols = colorlut[(log10(pmax(tmp, stftobj$null.logmean * 10)))*10 - min(log10(pmax(tmp, stftobj$null.logmean * 10)))*10+1]
> persp3d(y = constrain(log(stftobj$frequency), -3, Inf) ,z = log10(pmax(tmp, stftobj$null.logmean * 10)), alpha =1, col=(cols), front = "line", back="line")
> aspect3d(c(2,1,1))
> persp3d(y = constrain(log(stftobj$frequency), -3, Inf) ,z = (pmax(tmp, stftobj$null.logmean * 10)), alpha =1, col=(cols), front = "line", back="line")
> 

