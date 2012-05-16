#extends time display from package chron to use h:m:s for >1 day times.

times2 = function(x, ...){
require(chron)
#units = match.arg(units)
##convert to days
#if (units == "seconds") x = x /(60*60*24)

##this bit might trigger a y2k style bug, remove when time formats are rationalised or we go to a class based system
#if (x[1] < (946684800/(60*60*24))) x = x + 946684800/(60*60*24)
out = times(x,...)
class(out) = c("times2",class(out))
out
}

#resist m/d/y!
chron2 = function(x, units = c("seconds", "days"), out.format = list(dates= "d/m/y", times = "h:m:s"),...){
require(chron)
units = match.arg(units)
if (units == "seconds") x = x/(60 *60*24)
#if (x[1] < (946684800/(60*60*24)) & (x[1] > 365 ) ) x = x + 946684800/(60*60*24)
chron(x,out.format = out.format, ...)
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
	x = x /(60*60*24)
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

"plot.times2" <-
function(x, y, ...,
         xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
         simplify)
{
    if(missing(simplify))
        if(is.null(simplify <- getOption("chron.simplify")))
            simplify <- TRUE
    x.times <- inherits(x, "times")	# is x a times?
    if(missing(y)) {
        x <- sort(x)                    # NA's will be ignored
        y <- seq_along(as.vector(x))
        if(missing(ylab))
            ylab <- "Counts"
    }
    y.times <- inherits(y, "times")	# is y a times?
    dots <- list(...)
    if(is.null(axes <- dots$axes)) axes <- TRUE # do we draw axes? 
    ## only xaxt="n" or yaxt="n" requests in ... are honored!
    if(is.null(req.xaxt <- dots$xaxt) || req.xaxt != "n")
        req.xaxt <- "s"
    if(is.null(req.yaxt <- dots$yaxt) || req.yaxt != "n")
        req.yaxt <- "s"
    old <- par("xaxt", "yaxt")
    on.exit(par(old))
    ## trap graphical pars in ... that affect axis() in addition to plot()
    if(is.null(adj <- dots$adj))
        adj <- par("adj")
    if(is.null(cex <- dots$cex.axis))
        cex <- par("cex")
    if(is.null(col <- dots$col.axis))
        col <- par("col")
    if(is.null(font <- dots$font.axis))
        font <- par("font")
    if(is.null(las <- dots$las))
        las <- par("las")
    if(is.null(lab <- dots$lab))
        lab <- par("lab")
    if(is.null(mgp <- dots$mgp))
        mgp <- par("mgp")
    if(is.null(tcl <- dots$tcl)) tcl <- par("tcl")	
    ## for some plot types we need to sort according to x
    if(!is.null(type <- dots$type))
        if(any(type == c("l", "b", "o"))) {
            xlab; ylab                  # force promises
            nas <- is.na(x)
            o <- order(x[!nas])
            x <- x[!nas][o]
            y <- y[!nas][o]
        }
    xx <- unclass(x)
    yy <- unclass(y)
    if(x.times)
        xaxt <- "n"
    else xaxt <- req.xaxt
    if(y.times)
        yaxt <- "n"
    else yaxt <- req.yaxt
    if(!is.null(l <- dots$log)) {
        if(inherits(x, "dates") && any(l == c("x", "xy", "yx")))
            stop("cannot do logarithmic plot of a dates object")
        if(inherits(y, "dates") && any(l == c("y", "xy", "yx")))
            stop("cannot do logarithmic plot of a chron object")
    }
    ## unfortunately we can't use (easily) NextMethod when y is missing!
    plot.default(xx, yy, xlab = xlab, ylab = ylab, ...,
                 xaxt = xaxt, yaxt = yaxt)
    if(axes) {
        if(req.xaxt == "n")
            par(xaxt = "n")
        else if(x.times)
            axis.times2(1, x, simplify = simplify, labels = TRUE,
                       adj = adj, col = col, cex = cex, font = font,
                       las = las, lab = lab, mgp = mgp, tcl = tcl)
        if(req.yaxt == "n")
            par(yaxt = "n")
        else if(y.times)
            axis.times2(2, y, simplify = simplify, srt = 90, labels
                       = TRUE, adj = adj, col = col, cex = cex,
                       font = font, las = las, lab = lab, mgp = mgp,
                       tcl = tcl)
    }
    invisible(list(x = x, y = y))
}

"axis.times2"<-
function(n, x, add = TRUE, labels, simplify = TRUE, ...)
{
    if(!inherits(x, "times2"))
        x <- times2(x)
    bad <- is.na(x) | abs(as.vector(x)) == Inf
    rng <- if(n == 1 || n == 3) par("usr")[1:2] else par("usr")[3:4]
    tmp <- c(rng, as.numeric(x[!bad]))
tmp = tmp/(60*60*24)
    rng1 <- diff(range(tmp))
    if (rng1 > 1) fctr <- 1
    else if (rng1 > 1/24) fctr <- 24
    else if (rng1 > 1/1440) fctr <- 1440
    else fctr <- 86400
    tmp <- pretty(fctr*tmp)/fctr
    if (simplify) {
        step <- diff(tmp[1:2])
    	simplify <- step >= 1/1440
    #	if (inherits(x, "chron") && step >= 1) class(x) <- class(x)[-1]
    }
    tmp = tmp * 60*60*24
    att <- attributes(x)
    at.x <- structure(tmp[tmp >= rng[1] & tmp <= rng[2]], format = att$
                      format, origin = att$origin, class = att$class)
    if(missing(labels) || (is.logical(labels) && labels)) 
        labels <- format(at.x, simplify = simplify)
    if(add)
        axis(n, at = at.x, labels = labels, ...)
    invisible(list(n = n, at = at.x, labels = labels))
}

