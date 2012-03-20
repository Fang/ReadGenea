reformat.time <-
function (t, format = "julian") 
{
    t <- unlist(strsplit(t, split = " "))
    if (length(t) == 1) {
        tmp <- unlist(strsplit(t, split = ":"))
        lt <- length(tmp)
        startdate <- paste(tmp[1:3], collapse = "-")
        starttime <- tmp[4:lt]
    }
    else {
        startdate <- t[1]
#reprocess d/m/y
tmpdate = unlist(strsplit(startdate, split = "/"))
if (length(tmpdate) == 2){
tmpdate = c(tmpdate,  format(Sys.time(), "%Y"))
}
if (length(tmpdate) == 3){
startdate = paste( tmpdate[3], tmpdate[2], tmpdate[1], sep="-")
}
tmpdate = unlist(strsplit(startdate, split= "-"))
if (nchar(tmpdate[1]) < 4) startdate = paste("20", startdate , sep="")
        starttime <- t[2]
    }
    tmp <- unlist(strsplit(starttime, split = ":"))
    if (length(tmp) == 2) {
        tmp[3] <- "00"
    }
    if (length(tmp) == 3) {
        tmp[4] <- "000"
    }
    starttime <- paste(tmp[1:3], collapse = ":")
    t1 <- paste(startdate, starttime, sep = " ")
    switch(format, julian = {
	# modified julian (julian date-time from 2000-01-01 00:00)
	    t1 <- as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%OS")
        jdt <- as.numeric(julian(t1))
	jdt <- jdt + 2440587.5 - 2451544.5
        t1 <- jdt + as.numeric(tmp[4])/864000000
    }, POSIX = {
        t1 <- paste(t1,tmp[4],sep=".")
    t1 <- as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%OS")
    }, seconds = {
	# modified "seconds" from origin: 2000-01-01 00:00
    t1 <- as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%OS")
        t1 <- as.numeric(t1) + as.numeric(tmp[4])/1000
	t1 <- t1 - 946684800
    })
    t1
}

