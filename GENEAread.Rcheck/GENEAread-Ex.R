pkgname <- "GENEAread"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('GENEAread')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("AccData")
### * AccData

flush(stderr()); flush(stdout())

### Name: AccData
### Title: Methods for processing and summarising AccData.
### Aliases: AccData [.AccData $.AccData print.AccData plot.AccData
###   summary.AccData
### Keywords: methods

### ** Examples


binfile  = system.file("binfile/TESTfile.bin", package = "GENEAread")[1]

#Read in the entire file, calibrated
procfile<-read.bin(binfile)

print(procfile)
summary(procfile)

plot(procfile$temperature)
plot(procfile[,c(1,7)])




cleanEx()
nameEx("GRtime")
### * GRtime

flush(stderr()); flush(stdout())

### Name: GRtime
### Title: Date time handling for the GENEAread package.
### Aliases: GRtime convert.time as.GRtime format.GRtime axis.GRtime
###   pretty.GRtime
### Keywords: methods

### ** Examples

as.GRtime("00:01")
#format is automatically set
convert.time(1:10)
convert.time(1:10*1000)
#we add a different default format
convert.time(1:10*1000, "%H:%M:%OS3") -> t
t
str(t)
#we override format with our own
format(t, format = "%a %d/%m/%y %H:%M:%OS3")

#plot calls axis.GRtime automatically. Notice
#that the format attribute is used.
plot(t, 1:10)
#strip out the default format
t2 = convert.time(t, format = NULL)
plot(t2, 1:10)

#image plots are a bit more complex

Z = matrix(rnorm(100), 10)
image(x = t, y = t2, z = Z, axes = FALSE)
axis.GRtime(x = t2, side = 2)
Axis(x = t, side = 1) #Axis also works
box() #complete the bounding box

#custom axes
plot(t2, 1:10, xaxt = "n")
axis.GRtime(at = pretty(t2, 20) , side = 1)



cleanEx()
nameEx("epoch.apply")
### * epoch.apply

flush(stderr()); flush(stdout())

### Name: epoch.apply
### Title: Compute epochal summary statistics.
### Aliases: epoch.apply epoch.mean epoch.sd epoch.median epoch.mad
###   epoch.autocor epoch.quantile svm
### Keywords: ts

### ** Examples


dat <- read.bin(system.file("binfile/TESTfile.bin", package = "GENEAread")[1]
    , calibrate = TRUE)

#look for the epochs that exceed a certain threshold 50% of the time
plot(epoch.apply( dat, epoch.size = 3 , 
    FUN = function(t) mean(abs(svm(t) -1)>0.2)> 0.5 ), type = "l")

plot(dat[,1], svm(dat), log = "y", pch = ".")
lines(epoch.mean(dat, incl.date = TRUE), lwd = 2)
lines(epoch.mean(dat, epoch.size = 30, incl.date = TRUE), col = 2, lwd = 2)
#this should give all the same results, but by a different way
lines(epoch.apply(dat, epoch.size = 30, 
    FUN = function(A) mean(svm(A, FALSE)), incl.date = TRUE), col = 3)
epsize = 30; lines(epoch.apply(dat, epoch.size = epsize, 
    FUN = function(t) median(t[,1])), epoch.apply(dat, epoch.size = epsize, 
    FUN = function(A) mean(svm(A, FALSE))), col = 4)
#note this is different
lines(epoch.apply(dat, epoch.size = epsize, 
    FUN = function(t) median(t[,1])),epoch.apply(dat, epoch.size = epsize, 
    FUN = function(A) mean(svm(A, sqrt = TRUE)))^2, col = 5)

#plot some statistics
par(mfrow = c(5,1), mar = c(1,4.5,1,1))
plot(epoch.sd(dat), type="l")
plot(epoch.median(dat), type= "l")
plot(epoch.mad(dat), type= "l")
plot(epoch.autocor(dat), type= "l")
tmp = epoch.quantile(dat, quantiles= c(0.1, 0.25, 0.5, 0.75, 0.9)); matplot(tmp, type = "l")





graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("get.intervals")
### * get.intervals

flush(stderr()); flush(stdout())

### Name: get.intervals
### Title: Extract an interval of data.
### Aliases: get.intervals print.VirtAccData VirtAccData
### Keywords: manip

### ** Examples


binfile  = system.file("binfile/TESTfile.bin", package = "GENEAread")[1]

#Read in a highly downsampled version of the file
procfile<-read.bin(binfile, downsample = 100)
print(procfile)
#Plot the x component
plot(procfile[,1:2], type = "l")

#Overlay some segments in different colour
lines(get.intervals(procfile, start = 0.4, end = 0.5, time.format = "prop", incl.date = TRUE)[,1:2], col=2) 
lines(get.intervals(procfile, start = 0.4, end = 5, time.format = "sec", incl.date = TRUE)[,1:2], col=3) 
lines(get.intervals(procfile, start = "16:51", end = "16:52", time.format = "time", incl.date = TRUE)[,1:2], col=4) 
#Note that measurements will depend on the downsampling rate, not the original sampling rate of the data
lines(get.intervals(procfile, start = 100, length = 10, time.format = "measurement", incl.date = TRUE)[,1:2], col=5) 
#This is also understood
lines(get.intervals(procfile, start = "16:52:10", 30,  incl.date = TRUE)[,1:2], col=6) 

#Now load in virtually
virtfile<-read.bin(binfile, virtual = TRUE)
#Notice that get.intervals with simplify = FALSE gives a genuine AccData object
realfile = get.intervals(virtfile, start = 0.5, end = 1, simplify = FALSE)
virtfile
realfile
#get.intervals calls read.bin automatically
points(get.intervals(virtfile, start = "16:52:10", "16:52:40",  incl.date = TRUE)[,1:2], col=4, pch = ".") 

#Alternatively, re-read procfile at a different resampling rate.
lines(get.intervals(procfile, start = "16:49:00", "16:49:30",  incl.date = TRUE, read.from.file = TRUE, downsample = 300)[,1:2], col=2) 





cleanEx()
nameEx("hanning.window")
### * hanning.window

flush(stderr()); flush(stdout())

### Name: hanning.window
### Title: Computes the Coefficients of a Hanning or Uniform Window.
### Aliases: hanning.window uniform.window
### Keywords: ts

### ** Examples
hanning.window(10)

x<-rnorm(500)
y<-stft(x, wtype="hanning.window")
plot(y)



cleanEx()
nameEx("header.info")
### * header.info

flush(stderr()); flush(stdout())

### Name: header.info
### Title: Get header info from GENEA output (.bin) file
### Aliases: header.info
### Keywords: IO

### ** Examples


fileheader <- header.info(system.file("binfile/TESTfile.bin", package = "GENEAread")[1], more = TRUE)
print(fileheader)
attr(fileheader, "calibration")



cleanEx()
nameEx("parse.time")
### * parse.time

flush(stderr()); flush(stdout())

### Name: parse.time
### Title: Parses a character time representation to another format.
### Aliases: parse.time
### Keywords: utilities

### ** Examples


t1 = parse.time("2012-06-21 13:04:01"); print(t1)
parse.time("21/06/12 13:04:01") #gives the same result

parse.time(c("19/07/70", "20/07/70"), format = "days")
#results here will depend on your locale
parse.time(c("19/07/70", "20/07/70"), format = "POSIX", tzone = -4)

#one is the same day, one can only find a match the next day
parse.time("13:05", start = t1) - t1
parse.time("13:00", start = t1) - t1
#asking to wait 1 midnight means both times are considered as 
#times on the same, full day of data
parse.time(c("1 13:05", "1 13:00"), start = t1) - t1
#2012-06-21 is a Thursday, so this is equivalent
parse.time(c("Fri 13:05", "Fri 13:00"), start = t1) - t1
#Longer form days of the week are also understood. Note that 
#the first day does not get matched.
parse.time(c("Thursday 13:05", "Thursday 13:00"), start = t1) - t1




cleanEx()
nameEx("plot.stft")
### * plot.stft

flush(stderr()); flush(stdout())

### Name: plot.stft
### Title: Plots and prints Short Time Fourier Transforms
### Aliases: plot.stft print.stft
### Keywords: hplot

### ** Examples


#Real data
binfile  = system.file("binfile/TESTfile.bin", package = "GENEAread")[1]

#Read in the entire file, calibrated
procfile<-read.bin(binfile)
#Create stft object
obj = stft(procfile, type = "svm", quiet = TRUE)
#Look at it
print(obj)

plot(obj, cex = 5)
plot(obj, showmax = FALSE, cex = 5) #suppress principals

#pval plot
plot(obj, mode = "pval", cex = 5)
#disable reassigned stft
plot(obj, mode = "pval", reassign = FALSE) 
#median smoothing
plot(obj, mode = "pval", reassign = FALSE, median = TRUE) 
#log scale frequency, no top bar
dev.new(); plot(obj, mode = "pval", reassign = FALSE, topthresh = Inf, log = "y") 



cleanEx()
nameEx("read.bin")
### * read.bin

flush(stderr()); flush(stdout())

### Name: read.bin
### Title: File processing function for binary files.
### Aliases: read.bin
### Keywords: IO

### ** Examples


binfile  = system.file("binfile/TESTfile.bin", package = "GENEAread")[1]

#Read in the entire file, calibrated
procfile<-read.bin(binfile)
print(procfile)
procfile$data.out[1:5,]

#Uncalibrated, mmap off
procfile2<-read.bin(binfile, calibrate = FALSE)
procfile2$data.out[1:5,]

#Read in again, reusing already computed mmap pagerefs
procfile3<-read.bin(binfile, pagerefs = procfile2$pagerefs )

#Downsample by a factor of 10
procfilelo<-read.bin(binfile, downsample = 10)
print(procfilelo)
object.size(procfilelo) / object.size(procfile)

#Read in a 1 minute interval
procfileshort <- read.bin(binfile, start = "16:50", end = "16:51")
print(procfileshort)

##NOT RUN: Read, and save as a R workspace
#read.bin(binfile, outfile="tmp.Rdata")
#print(load("tmp.Rdata"))
#print(processedfile)





cleanEx()
nameEx("stft")
### * stft

flush(stderr()); flush(stdout())

### Name: stft
### Title: Computes Short Time Fourier Transforms
### Aliases: stft
### Keywords: ts

### ** Examples

#Some artificial data
time = 1:5000
#sum of two sine curves at 0.3 Hz and 0.05 Hz
f1 = 0.3; f2 = 0.05
sin1 = sin(time * f1 * 2*pi)
sin2 = sin(time * f2 * 2*pi)
#add a bit of noise
signal = sin1 + sin2 + 1*rnorm(5000)
#non-reassigned
stft(signal, plot = TRUE, reassign = FALSE, win = 100)
#reassigned
stft(signal, plot = TRUE, reassign = TRUE, win = 100)

#add a third component: varying frequency.
stft(signal + sin( cumsum(seq(f2, f1, length = 5000))*2*pi), plot = TRUE, reassign = TRUE, win = 100)

#Real data
binfile  = system.file("binfile/TESTfile.bin", package = "GENEAread")[1]

#Read in the entire file, calibrated
procfile<-read.bin(binfile)
#Default is mv
stft(procfile, plot.it = TRUE)
#Try sum?
stft(procfile, plot.it = TRUE, type = "sum", reassign = FALSE)

#Just look at the last 50% of the data
stft(procfile, start = 0.5, plot.it = TRUE)

#not reassigned, svm
stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE)
#a narrower 5 second window means better time resolution
stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE, win = 5)
#choose increments so as not to overlap
stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE, win = 5, inc = 5)
#uniform windows
stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE, wtype = "uniform.window")

#Svm, reassigned, quietly
obj = stft(procfile, type = "svm", quiet = TRUE)
plot(obj, cex = 3, showmax = FALSE, mode = "pval")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
