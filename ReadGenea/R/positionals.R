#
#plot(acos(tmp[,2] / sqrt(rowSums(tmp^2)) ) *180/pi -90, col = hcl(180 *acos(tmp[,1] / sqrt(rowSums(tmp[,-2]^2)))/pi + 180 , alpha = constrain(1 - 10*abs( sqrt(rowSums(tmp^2)) - 1), 0,1) ), pch=".")


#halfday2 = read.bin("C:/Users/zhou/Data/__011661_2012-02-29 14-05-12.bin", cali=T)



positionals = function(x, start, end= NULL, length = NULL, time.format = c("auto", "seconds", "days", "proportion", "measurements", "time", "date"), filter=T, col = hcl(0:360), legend = T){ 

tmp2 = get.intervals(x, start, end, length, time.format , incl.date=T)

if (filter==0){
ind = rep(T, nrow(tmp2))
} else if (filter == 1) {
ind = expand((bapply.basic(1: nrow(tmp2), 100 , function(t) sum(cov(1:100, tmp2[t, 2:4])^2) / sum(apply(tmp2[t, 2:4], 2, sd)^2)) / var(1:100)) %bq% "[0, 0.5]", nrow(tmp2))

} else if (filter == 2){


tmp2 = apply(tmp2, 2, function(t) runmed(t, 501))

ind = rep(T, nrow(tmp2))
}
plot(times2(tmp2[ind,1]), -acos(tmp2[ind,3] / sqrt(rowSums(tmp2[ind,-1]^2)) ) *180/pi +90, col =col[ floor( length(col)* (sign(-tmp2[ind,2]) * 180 *acos(-tmp2[ind,4] / sqrt(rowSums(tmp2[ind,-c(1,3)]^2)))/pi +180)/360   ) + 1 ], ylim = c(-90, 100), xlab = "Time", ylab="Up/Down", pch=".", cex= 2, yaxt = "n"); abline(h = c(-2:2) * 45, lty= 2); axis(2, at = 45 * -2:2)

if (legend){
points(times2(seq(tmp2[1,1] , quantile(tmp2[,1], 0.2), len = 361) )-> tmp, rep(95, 361), col =col[ floor( length(col)* seq(0.999, 0 , len = 361)) +1 ] , pch = "|")
text(tmp[1], 95, "CCW")
text(tmp[361], 95, "CW")
points(tmp[c( 90, 180, 270) +1], rep(95, 3), pch = "|")
}


invisible(tmp2)
}
