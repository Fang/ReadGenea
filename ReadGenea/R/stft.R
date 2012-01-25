#calculates Short Time Fourier Transforms.
#if center = T, remove means
#if calc.null, calculate 'null hypothesis' by randomising data (sampling w/o replacement) and calculating FFT on that.
stft <- function(X, win=min(80,floor(length(X)/10)), 
                 inc= max(1, floor(win/4)), coef=64, 
		 wtype="hanning.window", freq = 100, center = T, plot.it = T, calc.null = T )
  {
    numcoef <- 2*coef
    if (win < numcoef)
      {
	win <- numcoef
	cat ("stft: window size adjusted to", win, ".\n")
      }
    numwin <- trunc ((length(X) - win) / inc)

    ## compute the windows coefficients
    wincoef <- eval(parse(text=wtype))(win)

    ## create a matrix Z whose columns contain the windowed time-slices
    z <- matrix (0, numwin + 1, win)
    y <- matrix(0, numwin+1, win)
    st <- 1
    for (i in 0:numwin)
      {
	z[i+1, 1:win] <- (X[st:(st+win-1)] - mean(X[st:(st+win - 1)])* center) * wincoef
	y[i+1,] <- fft(z[i+1,] )
	st <- st + inc
      }
null.logmean = NULL
null.logsd = NULL
if (calc.null){
tmpdat = stft(sample(X), win = win, 
                 inc= inc, coef=coef, 
		 wtype=wtype, freq = freq, center = T, plot.it = F, calc.null = F )
null.logmean = mean(log(tmpdat$values[,-1]))
null.logsd = sd(log(tmpdat$values[,-1]))
}

    Y<- list (values = cbind(y[,1] ,2*Mod(y[,(2):coef])), windowsize=win, increment=inc,
		  windowtype=wtype, center = center, frequency = freq, null.logmean = null.logmean, null.logsd = null.logsd, principals = (freq * (1:coef  - 1 ) / win)[apply( Mod(y[,(1):coef]),1, which.max)]  )
    class(Y) <- "stft"
    if (plot.it) plot.stft(Y)
    return(Y)
  }

hanning.window = function (n) {
    if (n == 1) 
        c <- 1
    else {
        n <- n - 1
        c <- 0.5 - 0.5 * cos(2 * pi * (0:n)/n)
    }
    return(c)
}
uniform.window = function(n){
rep(1, n)
}


plot.stft <- function (x, col = gray (63:0/63), log.it = F, log = "", showmax = T, ...)
  {
    xv <- x$values
if (log.it){
xv = log(xv)
if (!is.null(x$null.logmean)) xv = pmax(xv, x$null.logmean)
}
time = (x$windowsize/2 +  x$increment * 0:(nrow(xv) - 1))/(x$freq)
frequency= x$freq * (1:ncol(xv)  - 1 ) / x$windowsize
if(log == "y"){
frequency[1] = frequency[2]^2/frequency[3]
}

    image( x = time , y = frequency,   z=xv, col=col, log = log,...)
if (showmax){
points ( time, frequency[apply(x$values,1,which.max)] ->principals, col=2 * (rowMeans(xv) > 0.1 * x$null.logmean)  , pch=".", cex = 3)
invisible(principals)
}

}

#example code
#plot(stft(subs(mag, 0.94,0.96), win = 1024, plot = F, coef = 512), log.it = T, log="y")
#plot(stft(subs(mag, 0.7,8), win = 1024, plot = F, coef = 512), log.it = T, log="y")
#plot(stft(subs(mag, 0.0001,0.005), win = 1024, plot = F, coef = 512), log.it = T)



#plot(stft(subs(mag, 0.7,0.8), win = 1024, plot = F), log.it = T, log = "y")
#plot( stft(rep(1, 1000) + c(sin(1:500/ 10 * 2*pi), rep(0, 500)) + c(rep(0, 300),sin(1:500/ 20 * 2*pi), rep(0, 200)) , freq = 1, plot.it = F), log="x")
#stft(sin(1:1000 / (1 +sqrt(1000:1)) * 2 * pi), freq = 1)
# stft(rep(1, 1000) + sin(1:1000/ 10 * 2*pi), freq = 1)

#subsets a proportion of the dataset, or a certain length of the dataset starting at a specific proportion position
subs <- function(x, a,b){
len = length(x)
if (a > 1){
return(x[a : (a+b - 1)])
} else if (b > 1) {
return( x [ floor(a * len): (floor(a*len) + b - 1)])
} else {
return (x[floor(a*len) : floor(b*len)])
}
}

#plot(stft(subs(mag, 0.7,0.8), win = 1024, plot = F, coef = 512), log.it = T)


