#if center = T, remove means
stft <- function(X, win=min(80,floor(length(X)/10)), 
                 inc=min(24, floor(length(X)/30)), coef=64, 
		 wtype="hanning.window", freq = 100, center = T, plot.it = T )
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

    Y<- list (values = Mod(y[,(1):coef]), windowsize=win, increment=inc,
		  windowtype=wtype, center = center, frequency = freq)
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


plot.stft <- function (x, col = gray (63:0/63), log.it = F, log = "", ...)
  {
    xv <- x$values
if (log.it) xv = log(xv)
time = (x$windowsize/2 +  x$increment * 0:(nrow(xv) - 1))/(x$freq)
frequency= x$freq * (1:ncol(xv)  - 1 ) / x$windowsize
if(log == "x"){
frequency[1] = frequency[2]^2/frequency[3]
}

    image( x = time , y = frequency,   z=xv, col=col, log = log,...)
}


plot( stft(rep(1, 1000) + c(sin(1:500/ 10 * 2*pi), rep(0, 500)) + c(rep(0, 300),sin(1:500/ 20 * 2*pi), rep(0, 200)) , freq = 1, plot.it = F), log="x")
stft(sin(1:1000 / (1 +sqrt(1000:1)) * 2 * pi), freq = 1)
# stft(rep(1, 1000) + sin(1:1000/ 10 * 2*pi), freq = 1)



