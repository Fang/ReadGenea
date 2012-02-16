
u  = rnorm(3)
v = rnorm(3)
x = complex(u, v)


spectralpow = function(a,u, v){
if (a[1]^2+ a[2]^2 > 1) return(Inf)
a = c(a,sqrt(1 - a[1]^2 - a[2]^2))

-sum(a*u)^2 - sum(a*v)^2
}


plot(Mod(rowSums(t(res2) *fftout))[1:400], type="l", log="x")
lines(Mod(fftout[,1]), col=2)
lines(Mod(fftout[,2]), col=2)
lines(Mod(fftout[,3]), col=2)


lines(Mod(fft(prcomp(scale((nadwrist$dat[500*100 + 1:1000,2:4]), scale=F))$x[,1])), col=5, lwd=2)
lines(Mod(fft(prcomp(scale((nadwrist$dat[500*100 + 1:1000,2:4]), scale=F))$x[,2])), col=4, lwd=2)


