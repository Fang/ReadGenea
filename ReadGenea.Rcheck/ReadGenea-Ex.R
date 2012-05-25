pkgname <- "ReadGenea"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ReadGenea')

assign(".oldSearch", search(), pos = 'CheckExEnv')
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

### Name: reformat.time
### Title: Reformats a character time representation to a POSIXct object.
### Aliases: reformat.time
### Keywords: manip

### ** Examples


#reformat.time("2011:02:11:12:34:02","julian")

#reformat.time("2010-07-19 13:04:01","POSIX")




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
