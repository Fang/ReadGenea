#plot an 3d sequence with sedentary sphere


#start,end: specify by time in seconds, days, or proportion
plotsphere <- function(x, start, end= NULL, length = NULL, time.format = c("auto", "seconds", "days", "proportion", "measurements"), smooth = F, col, arrow = T){
sampling.freq = 100
time.format = match.arg(time.format)

require(rgl)
if (inherits(x, "list")){
sampling.freq = x$freq
 x = x$data.out[,2:4]
}
n = nrow(x)

#auto detect time format
if (time.format == "auto"){
if (start <1){
 time.format = "proportion"
}else if (floor(start) == start) {
 time.format = "seconds"
} else {
time.format = "days"
}
}
if (length(start) > 1) {
end = max(start)
start = min(start)
}

if (is.null(length)){
if ((time.format == "proportion") & (end > 1)){
length = end
end = NULL
}
}

if (is.null(end)) {
if ((time.format == "proportion") && (length >= 1)){
time.format ="seconds"
start = (start * n/sampling.freq)
}
end = start + length
}

#convert into measurements

if (time.format == "proportion"){
start = ceiling(start * n)
end  = floor(end * n)
} else if (time.format == "seconds") {
start = ceiling(start * sampling.freq)
end = floor(end * sampling.freq)
} else if (time.format == "days"){
start = ceiling(start * sampling.freq*60*60*24)
end = floor(end * sampling.freq*60*60*24)
}

plot3d( x[start:end,] -> tmp3)

if (missing(col)) col = heat.colors(nrow(tmp3))
lines3d(tmp3, col=col); aspect3d("iso");  spheres3d(0,0,0,1, alpha = 0.5, front="line", back = "line");grid3d(side = c("x","y","z"),at = c(0,0,0))#; plot(1:nrow(tmps)/nrow(tmps), tmps[,3], type="l");  abline(v = offset+c(0, leng), col=2) 

if (arrow){
#add a placeholder arrow


triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, coords = c(1,3,2), back="cull", col=1, alpha = 0.5)
 triangles3d(c(-1, 0, 1)/2, c(0,1,0)/2, c(0,0,0)/2, front="cull", col="gold", alpha=0.5)
 quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), back = "cull", col= 1, alpha = 0.5)
 quads3d( c(-0.5, 0.5, 0.5, -0.5)/2, c(0,0,-1,-1)/2, c(0,0,0,0), front = "cull", col= "gold", alpha = 0.5)
}



cat("Plotted ",nrow(tmp3) / sampling.freq , " seconds of data.\n")

}

