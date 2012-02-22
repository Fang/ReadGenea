#extends time display from package chron to use h:m:s for >1 day times.

times2 = function(x, units = c("seconds", "days"),...){
require(chron)
units = match.arg(units)
#convert to days
if (units == "seconds") x = x /(60*60*24)
#this bit might trigger a y2k style bug, remove when time formats are rationalised or we go to a class based system
if (x[1] < (946684800/(60*60*24))) x = x + 946684800/(60*60*24)
out = times(x,...)
class(out) = c("times2",class(out))
out
}

chron2 = function(x, units = c("seconds", "days"), ...){
require(chron)
units = match.arg(units)
if (units == "seconds") x = x/(60 *60*24)
if (x[1] < (946684800/(60*60*24)) & (x[1] > 365 ) ) x = x + 946684800/(60*60*24)
chron(x,...)
}

print.times2 <-
function(x, digits, quote = FALSE, prefix = "", simplify, ...)
{
    if(!as.logical(length(x))) {
        cat("times(0)\n")
        return(invisible(x))
    }
    if(missing(simplify) &&
       is.null(simplify <- getOption("chron.simplify")))
        simplify <- FALSE
    xo <- x
    ## print whole days (no fraction) as regular integers
#ZF#    if(all(is.na(x)) || any(x[!is.na(x)] >= 1))
 #ZF#       cat("Time in days:\n")
    x <- format.times2(x, simplify = simplify)
#    NextMethod("print", quote = quote)
print(x, quote = quote)
    invisible(xo)
}


"format.times2"<-
function(x, format. = "h:m:s", simplify = FALSE, ...)
{
    if(!as.logical(length(x)))
        return("")
    if(all(is.na(x)))
        return(rep("NA", length = length(x)))
    if(!is.numeric(x))
        stop(paste(deparse(substitute(x)), "must be numeric"))
    att <- attributes(x)
    if(inherits(x, "times")) {
        if(missing(format.))
            format. <- switch(mode(att$format),
                              character = ,
                              list = rev(att$format)[[1]],
                              name = ,
                              "function" = att$format,
                              NULL = format.,
                              stop("invalid output times format"))
        class(x) <- NULL
    }
    if(!is.character(format.)) {
        ## format may be a function or name
        FUN <- switch(mode(format.),
                      "function" = format.,
                      name = eval(format.),
                      stop(paste("unrecognized time format",
                                 deparse(substitute(format.)))))
        return(FUN(unclass(x), ...))
    }
    else format. <- rev(format.)[1]	
    nas <- is.na(x)
    att$class <- att$format <- att$origin <- NULL
    ## <NOTE>
    ## DJ's design is that
    ##   times greater than 1 day  should format like numerics
    ## To change this (e.g., have times(1.5) format as 36:00:00), simply
    ## comment the code below, and make the corresponding change in
    ## print.times().
    days <- abs(floor(x))
    if(any(days[!nas] > 0)) {
	x = x - floor(x)
     #   attributes(x) <- att
     #   return(format(x))
    }
    ## </NOTE>
    sec <- round(24 * 3600 * abs(x))
    hh <- sec %/% 3600
    mm <- (sec - hh * 3600) %/% 60
    ss <- round(sec - hh * 3600 - 60 * mm) # round instead of truncate
    out <- list(h = substring(paste("0", hh, sep = ""), nchar(paste(hh))), 
		m = substring(paste("0", mm, sep = ""), nchar(paste(mm))),
                s = substring(paste("0", ss, sep = ""), nchar(paste(ss))))
  #  style <- parse.format(format.)
#override style
style = list(periods = c("h", "m", "s"), sep = ":")
    o <- style$periods
    if(!simplify)
        out <- paste(out[[o[1]]], out[[o[2]]], out[[o[3]]],
                     sep = style$sep)
    else {
        if(simplify == 1) {
            ## no secs
            o <- o[o != "s"]
            out <- paste(out[[o[1]]], out[[o[2]]], sep = style$sep)
        }
        else out <- out$h
    }
    if(any(x[!nas] < 0))
        out <- paste(ifelse(x < 0, "-", " "), out, sep = "")
    out[nas] <- NA
    out[x == Inf] <- "Inf"
    out[x ==  - Inf] <- "-Inf"
    attributes(out) <- att
    out
}
