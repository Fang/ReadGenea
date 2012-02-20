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
