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


#Not run: Examples of binary file processing:

#procfile<-read.bin("binfile.txt","myprocessedfile",start=1,end=10,do.temp=TRUE)

#procfile2<-read.bin("binfile.txt",start="2010-10-02 12:32:01",end="2010-10-04 12:05:11")

#procfile3<-read.bin("binfile.txt",correct.z=TRUE,calibrate=TRUE)

#processedfile<-read.bin("binfile.txt","myprocessedfile",start=1,end=10,do.temp=TRUE,calibrate=TRUE)

#processedfile2<-read.bin("binfile2.txt",start="2010-10-02 12:32:01",end="2010-10-04 12:05:11",do.temp=TRUE,calibrate=TRUE)



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
