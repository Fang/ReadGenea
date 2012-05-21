#
#plot(acos(tmp[,2] / sqrt(rowSums(tmp^2)) ) *180/pi -90, col = hcl(180 *acos(tmp[,1] / sqrt(rowSums(tmp[,-2]^2)))/pi + 180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp^2)) - 1), 0,1) ), pch=".")


#halfday2 = read.bin("C:/Users/zhou/Data/__011661_2012-02-29 14-05-12.bin", cali=T)



positionals = function(x, start=0, end= 1, length = NULL, filter=T,bw = F, col , legend = T, max.points = 1e6, density = F,...){ 
if (missing(col)){
if (bw){
col = 1:5
} else {
col = hcl(0:360)
}
}
tmp2 = get.intervals(x, start, end, length,, incl.date=T, size = max.points,...)

if (filter==0){
ind = rep(T, nrow(tmp2))
} else if (filter == 1) {
ind = expand((bapply.basic(1: nrow(tmp2), 100 , function(t) sum(cov(1:100, tmp2[t, 2:4])^2) / sum(apply(tmp2[t, 2:4], 2, sd)^2)) / var(1:100)) %bq% "[0, 0.5]", nrow(tmp2))
} else if (filter == 2){
ind = expand(  (bapplyc.basic(1: nrow(tmp2), 100 , function(t) sum(diag(cov( tmp2[t[1:99], 2:4], tmp2[t[2:100], 2:4]))^2) / sum(sd(tmp2[t, 2:4])^2)^2)) %bq% "[0, 0.5]", nrow(tmp2))

} else if (filter == 3){


tmp2 = apply(tmp2, 2, function(t) runmed(t, 501))

ind = rep(T, nrow(tmp2))
}

if (!bw){
plot(times2(tmp2[ind,1]) -> x, -acos(tmp2[ind,3] / sqrt(rowSums(tmp2[ind,-1]^2)) ) *180/pi +90 ->y, col =col[ floor( length(col)* (sign(-tmp2[ind,2]) * 180 *acos(-tmp2[ind,4] / sqrt(rowSums(tmp2[ind,-c(1,3)]^2)))/pi +180)/360   ) + 1 ], ylim = c(-90, 100), xlab = "Time", ylab="Up/Down", pch=".", cex= 2, yaxt = "n"); abline(h = c(-2:2) * 45, lty= 2); axis(2, at = 45 * -2:2)

if (legend){
points(times2(seq(tmp2[1,1] , quantile(tmp2[,1], 0.2), len = 361) )-> tmp, rep(95, 361), col =col[ floor( length(col)* seq(0.999, 0 , len = 361)) +1 ] , pch = "|")
text(tmp[1], 95, "CCW")
text(tmp[361], 95, "CW")
points(tmp[c( 90, 180, 270) +1], rep(95, 3), pch = "|")
}
} else {
plot(times2(tmp2[ind,1]) -> x, -acos(tmp2[ind,3] / sqrt(rowSums(tmp2[ind,-1]^2)) ) *180/pi +90 ->y, ylim = c(-90, 100), xlab = "Time", ylab="Up/Down", pch=".", cex= 2, yaxt = "n"); abline(h = c(-2:2) * 45, lty= 2); axis(2, at = 45 * -2:2)

degs = (floor( length(col)* (sign(-tmp2[ind,2]) * 180 *acos(-tmp2[ind,4] / sqrt(rowSums(tmp2[ind,-c(1,3)]^2)))/pi +180)/360   ) + 1 )
shade = bapply.basic(degs, floor(length(degs)/100), function(t) round(median(t))) / length(col)
beg =  bapply.basic(times2(tmp2[ind, 1]), floor(length(degs)/100), min)
end =  bapply.basic(times2(tmp2[ind, 1])[-1], floor(length(degs)/100), max)


for (i in 1:length(shade)){
rect(beg[i], -120, end[i], 120, border= NA,  col = rgb(0,0,0, alpha =0.5 * shade[i]))
}


if (legend){

tmp = times2(seq(tmp2[1,1] , quantile(tmp2[,1], 0.2), len = 361) )

#rect(tmp[1] - (tmp[361] - tmp[1])* 0.05 , 91, tmp[361]+(tmp[361] - tmp[1])* 0.05, 98, col="white")
points(tmp, rep(95, 361), col =grey( 1- 0.5* (floor( length(col)* seq(0.999, 0 , len = 361)) +1)/length(col))  , pch = "|")

text(tmp[1], 95, "CCW")
text(tmp[361], 95, "CW")
points(tmp[c( 90, 180, 270) +1], rep(95, 3), pch = "|")
}
}

if (density){
#these bandwidth settings seem to be fine
kde2dplot(as.numeric(x), y, xaxt = "n", h.relative= c(0.05,0.3), add = T, yaxt = "n", n = 200, shade = T, breaks = c(0.1,0.25,0.5), col=1)
}


invisible(list(x = x, y=y))
}
