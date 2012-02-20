#fundamental frequency finding by YIN

#finds the first minimum of a vector under a certain threshold, or the global min
peakfind.yin = function(t, thresh=0.1){
ind = c((t < thresh),F)
if (sum(ind, na.rm= T) > 0){
beg = match(1,ind)
end = beg+ match(0, tail(ind, -beg))

return( beg +which.min(t[beg:end]))

} else {

return(which.min(c(Inf,t))-1 )
}
}


tmp = nadwrist$dat[,2]

diffobj = sapply(1:120, function(t) (tmp - shift(tmp, t, fill="zero"))^2)

tmp = nadwrist$dat[,3]
diffobj = diffobj +sapply(1:120, function(t) (tmp - shift(tmp, t, fill="zero"))^2)

tmp = nadwrist$dat[,4]
diffobj = diffobj + sapply(1:120, function(t) (tmp - shift(tmp, t, fill="zero"))^2)

diffmed = apply(diffobj, 2, function(t) runmed(t,301))


YIN = apply(diffmed, 1, function(t) t / (cumsum(t)/ 1:length(t)))

#plot(runmed(apply(YIN,1 , function(t) peakfind.yin(t, 0.6)), 201), type="l")

plot(runmed(apply(YIN,2 , function(t) peakfind.yin(t, 0.6)), 1001), type="l")


