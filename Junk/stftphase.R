 
stftphase <- function(X, win=min(80,floor(length(X)/10)), 
                 inc= max(1, floor(win/2)), coef=floor(win/2), 
		 wtype="hanning.window", freq , center = T, plot.it = F, calc.null = T , pvalues = F, start.time = NULL, reassign = F )
  {
call = match.call()
if (length(dim(X)) ==2) {
start.time = X[1,1]
if (missing(freq)) freq =1/(X[2,1] -  X[1,1] )
X = X[,2]

}

if (missing(freq) ) freq = 100

Xdel = shift(X, c(1,0), fill = "edge")
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
pval = rep(0, numwin+1)
    z <- matrix (0, numwin + 1, win)
    y <- matrix(0, numwin+1, win)
    ydel <- matrix(0, numwin+1, win)
    st <- 1
pb <- txtProgressBar(min = 0, max = 100,style=1)
    for (i in 0:numwin)
      {
	z[i+1, 1:win] <- (X[st:(st+win-1)] - mean(X[st:(st+win - 1)])* center) * wincoef
	y[i+1,] <- fft(z[i+1,] )
if (reassign){	
	z[i+1, 1:win] <- (Xdel[st:(st+win-1)] - mean(Xdel[st:(st+win - 1)])* center) * wincoef
	ydel[i+1,] <- fft(z[i+1,] )
}
if (pvalues){
temp = sample(X[st:(st + win - 1)])
temp = (temp - mean(temp) * center)*wincoef
temp = Mod(fft(temp))[1:coef]^2
pval[i+1] = wilcox.test( (Mod(y[ i+1,])^2 - mean(Mod(y[ i+1,])^2))^2, (temp - mean(temp))^2)$p.value
}
	st <- st + inc

setTxtProgressBar(pb, 90* i/(numwin ))
      }
if (reassign){
yfreqdel = cbind(y[, win],y[, 2: win - 1])# t(apply(y, 1, function(t) shift(t, 1, fill = "loop")))

ydel = Arg(y*Conj(ydel)) *(freq/(2*pi))
yfreqdel = -(win/(2*pi*freq)) * Arg(y * Conj(yfreqdel)) + win/(2*freq)
} else {
yfreqdel = NULL
}



null.logmean = NULL
null.logsd = NULL
if (calc.null){
tmpdat = stft(sample(X), win = win, 
                 inc= inc, coef=coef, 
		 wtype=wtype, freq = freq, center = T, plot.it = F, calc.null = F )
null.logmean = log(sqrt(mean((tmpdat$values)^2)))
#null.logsd = sd(tmpdat$values))
}
setTxtProgressBar(pb, 100)
if (is.null(start.time)){
    Y<- list (values = cbind(Arg(y[,1]) ,Arg(y[,(2):coef])), windowsize=win, increment=inc,
		  windowtype=wtype, center = center, sampling.frequency = freq, null.logmean = null.logmean, null.logsd = null.logsd, principals = (freq * (1:coef  - 1 ) / win)[apply( Mod(y[,(1):coef]),1, which.max)], frequency = (freq * (1:coef  - 1 ) / win), times =  (win/2 +  inc * 0:(nrow(y) - 1))/(freq), p.values = pval, LGD = yfreqdel[,1:coef], CIF = ydel[,1:coef] )
} else {
times = (start.time + 946684800 + (win/2 +  inc * 0:(nrow(y) - 1))/freq)
   Y<- list (values = cbind(Arg(y[,1]) ,Arg(y[,(2):coef])), windowsize=win, increment=inc,
		  windowtype=wtype, center = center, sampling.frequency = freq, null.logmean = null.logmean, null.logsd = null.logsd, principals = (freq * (1:coef  - 1 ) / win)[apply( Mod(y[,(1):coef]),1, which.max)], frequency = (freq * (1:coef  - 1 ) / win), times = times, p.values = pval, LGD = yfreqdel[,1:coef], CIF = ydel[,1:coef]  )
}
close(pb)
Y$call = call
    class(Y) <- "stft"
    if (plot.it) plot.stft(Y)
    return(Y)
  }

