#nn distance

del = function(x, Y, w = c(1,1)){
min((rep(x, each = nrow(Y)) - Y)^2 %*% w)
}

del2= function(a, b,w=c(1,1)){
a = scale(cbind(1:length(a), a))
b = scale(cbind(1:length(b), b))
res = apply(a, 1, function(t) del(t, b, w=w))
sum(res)
}


#for (i in 1:nrow(Y)){
#res = min(res, x - Y[i,]
