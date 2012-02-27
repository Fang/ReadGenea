#utility functions for ReadGenea


constrain <- function(x, minimum, maximum=-minimum){
  if (minimum > maximum) {
    temp <- minimum
    minimum <- maximum
    maximum <- temp
  }
  x = replace(x, which(x > maximum), maximum)
  x = replace(x, which(x < minimum), minimum)
  x
}

shift = function( x, offset, expand = 0, fill = c("zero", "edge", "loop", "mean")){
fill = match.arg(fill)

if (length(dim(x)) == 2){
if (length(expand) == 1) expand = rep(expand,2)
n = nrow(x)
p = ncol(x)
xlim = nrow(x) + expand[1]
ylim = ncol(x) + expand[2]
x2 = matrix(0, xlim, ylim)
if (fill =="mean") x2 = matrix(mean(x), xlim, ylim)
TL = c(1,1) - pmin(0, offset)
#BR = pmin(c(nrow(x) , ncol(x)) - TL + offset + c(1,1), c(xlim, ylim)) - offset
BR = pmin(offset + c(n,p) , c(xlim, ylim)) - offset

x2[0:((BR-TL)[1]) +max(offset[1],0)+1,0:((BR-TL)[2])+max(offset[2],0)+1] =  x[TL[1]:BR[1], TL[2]:BR[2]]


if ((fill != "zero") && (fill != "mean") ){

#top left corner
#if (min(offset) > 0){
#
#if (fill == "loop"){
#	x2[1: offset[1], 1:offset[2]] = x[ 1:offset[1] + n - offset[1], 1:offset[2] + p - offset[2]]
#} else {
#x2[1: offset[1], 1:offset[2]] = x[1,1]
#}
#}

#top edge
if (offset[1] > 0) x2[1:offset[1],] = matrix(shift(x[1,], offset[2], expand = ylim - p, fill="edge"), nrow  =  offset[1], byrow = T, ncol = ylim)
#left edge

if (offset[2] > 0) x2[,1:offset[2]] = matrix(shift(x[,1], offset[1], expand = xlim - n, fill="edge"), ncol = 1)

#bottom edge
if (n + offset[1] < xlim) x2[(n + offset[1]+1) :xlim,] = matrix(shift(x[n,], offset[2], expand = ylim - p, fill="edge"), nrow = xlim - offset[1] - n,  ncol= ylim, byrow = T)

#right edge

if (p + offset[2] < ylim) x2[,(p + offset[2]+1) :ylim] = matrix(shift(x[,p], offset[1], expand = xlim - n, fill="edge"), ncol = 1)


if (fill == "loop") print("Currently umimplemented loop fill, edging instead")
#if (sum(expand) != 0)	print("Warning, loop results may not make sense if matrix size changes")

}


} else {
#return(drop(shift(x=matrix(x, ncol=1), offset = c(offset,0), expand =c(expand, 0), fill = fill)))
offset = offset[1]
expand  = expand[1]
x2 = rep(0, length(x) + expand)
beg = 1 - pmin(offset, 0)
end =  min (offset + length(x), length(x2)) - offset # min(  length(x) - beg + offset + 1, length(x) + expand) - offset

x2[( max(offset,0) +1) : min(length(x2), length(x)+ offset)] = x[beg:end]

if (fill == "loop"){
if (offset > 0){
x2[1:offset] = tail(x, offset)
}else{
x2[((length(x) + offset + 1): length(x2))] = x[1:(-offset)]
}
} else if (fill == "edge"){
if (offset > 0){
x2[1:offset] = x[1]
}
if ((length(x) + offset) <  length(x2)){
x2[ (length(x) + offset +1) : length(x2)] = tail(x,1)
}

}

}
x2
}


#puts a vector into the range 0-1
conv01 <- function(x){
(x - min(x))/ (max(x)- min(x))
}



#convert time intervals #TODO: split out from plotsphere
get.intervals = function(x, start=0, end = 1, length = NULL, time.format = c("auto", "seconds", "days", "proportion", "measurements"), incl.date = F){

sampling.freq = 100
time.format = match.arg(time.format)

if (inherits(x, "list")){
sampling.freq = x$freq
 x = x$data.out[,(2- incl.date):4]
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

if (!is.null(length)) {
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

start = max(start,1)
end = min(end, n)

return(x[start:end,])
}


#'between' operator for convenience
#takes [min, max), or c("[", min, max, "]") style second terms
#default is [min, max] for c(,) terms
"%bt%" = function(X, y){
if (is.character(y)){
if (length(y) == 4) y = paste(y[1],y[2], ",",y[3], y[4], sep="")
y = strsplit(y, ",")[[1]]
if (substr(y[1],1,1) == "["){
res = (X >= as.numeric(substring(y[1], 2)))
}else {
res = (X > as.numeric(substring(y[1], 2)))
}
nc = nchar(y[2])
if (substr(y[2],nc,nc) == "]"){
res = res &(X <= as.numeric(substring(y[2],1, nc -1)))
}else {
res = res & (X < as.numeric(substring(y[2], 1,nc - 1)))
}
} else {
res = (X >= y[1] ) & (X<= y[2])
}
res
}
