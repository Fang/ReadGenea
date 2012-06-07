pkgname <- "ReadGenea"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ReadGenea')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("RGtime")
### * RGtime

flush(stderr()); flush(stdout())

### Name: RGtime
### Title: Date time handling for the ReadGenea package.
### Aliases: RGtime convert.time as.RGtime format.RGtime axis.RGtime
###   pretty.RGtime
### Keywords: manip

### ** Examples

as.RGtime("00:01")
#format is automatically set
convert.time(1:10)
convert.time(1:10*1000)
#we add a different default format
convert.time(1:10*1000, "%H:%M:%OS3") -> t
t
str(t)
#we override format with our own
format(t, format = "%a %d/%m/%y %H:%M:%OS3")

#plot calls axis.RGtime automatically. Notice
#that the format attribute is used.
plot(t, 1:10)
#strip out the default format
t2 = convert.time(t, format = NULL)
plot(t2, 1:10)

#image plots are a bit more complex

Z = matrix(rnorm(100), 10)
image(x = t, y = t2, z = Z, axes = FALSE)
axis.RGtime(x = t2, side = 2)
Axis(x = t, side = 1) #Axis also works
box() #complete the bounding box

#custom axes
plot(t2, 1:10, xaxt = "n")
axis.RGtime(at = pretty(t2, 20) , side = 1)



cleanEx()
nameEx("epoch.apply")
### * epoch.apply

flush(stderr()); flush(stdout())

### Name: epoch.apply
### Title: Compute epochal summary statistics.
### Aliases: epoch.apply epoch.mean epoch.sd epoch.median epoch.mad
###   epoch.autocor epoch.quantile svm
### Keywords: manip

### ** Examples


dat <- read.bin(system.file("binfile/TESTfile.bin", package = "ReadGenea")[1]
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
nameEx("header.info")
### * header.info

flush(stderr()); flush(stdout())

### Name: header.info
### Title: Get header info from Genea output (.bin) file
### Aliases: header.info
### Keywords: manip

### ** Examples


fileheader <- header.info(system.file("binfile/TESTfile.bin", package = "ReadGenea")[1], more = TRUE)
print(fileheader)
attr(fileheader, "calibration")



cleanEx()
nameEx("parse.time")
### * parse.time

flush(stderr()); flush(stdout())

### Name: parse.time
### Title: Parses a character time representation to another format.
### Aliases: parse.time
### Keywords: manip

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
nameEx("read.bin")
### * read.bin

flush(stderr()); flush(stdout())

### Name: read.bin
### Title: File processing function for binary files.
### Aliases: read.bin
### Keywords: manip

### ** Examples


binfile  = system.file("binfile/TESTfile.bin", package = "ReadGenea")[1]

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
