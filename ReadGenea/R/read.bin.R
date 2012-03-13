#reads binary accelerometer data
#blocksize = number of pages to read at a time
read.bin <-
function (binfile, outfile = NULL, start = NULL, end = NULL, 
    verbose = FALSE, do.temp = TRUE, calibrate = FALSE, gain = NULL, 
    offset = NULL, luxv = NULL, voltv = NULL, tformat = "seconds",warn=FALSE, downsample = NULL, blocksize = Inf, test = FALSE) 
{
#variables for positions and record lengths in file
    nobs <- 300
    headlines <- 59
    reclength <- 10
    position.data <- 10
    position.temperature <- 6
    orig.opt <- options(digits.secs = 3)
    fc <- file(binfile, "rt")
    freq <- scan(fc, skip = 19, what = "", n = 2, sep = ":", 
        quiet = TRUE)[2]
    freq <- as.numeric(strsplit(freq, " ")[[1]][1])
    xgain <- as.integer(scan(fc, skip = 27, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    xoffset <- as.integer(scan(fc, skip = 0, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    ygain <- as.integer(scan(fc, skip = 0, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    yoffset <- as.integer(scan(fc, skip = 0, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    zgain <- as.integer(scan(fc, skip = 0, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    zoffset <- as.integer(scan(fc, skip = 0, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    volts <- as.integer(scan(fc, skip = 0, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    lux <- as.integer(scan(fc, skip = 0, what = "", n = 2, sep = ":", 
        quiet = TRUE)[2])
    npages <- as.integer(scan(fc, skip = 2, what = "", n = 2, 
        sep = ":", quiet = TRUE)[2])
    t1 <- scan(fc, skip = 4, what = "", quiet = TRUE, nlines = 1)
    freq <- as.integer(scan(fc, skip = 4, what = "", n = 2, sep = ":", 
        quiet = TRUE)[2])
	if (!is.null(downsample)) {
		cat("Downsampling to ", freq/downsample[1] , " Hz \n")
		if (nobs %% downsample[1] != 0) cat("Warning, downsample divisor not factor of ", nobs, "!\n")
	}
    if (verbose) {
        cat("Number of pages in binary file:", npages, "\n")
    }
    close(fc)
    freqseq <- seq(0, by = 1/freq, length = nobs)
    timespan <- nobs/freq
    t1 <- t1[2:length(t1)]
    t1[1] <- substr(t1[1], 6, nchar(t1[1]))
    t1c <- reformat.time(t1, format = "POSIX")
    t1 <- reformat.time(t1, format = tformat)
    timestampsc <- seq(t1c, by = timespan, length = npages)
    timestamps <- seq(t1, by = timespan, length = npages)
    tnc <- timestampsc[npages]
    tn <- timestamps[npages]
    if (is.null(start)) {
        start <- 1
    }
    if (is.null(end)) {
        end <- npages
    }
    if (is.numeric(start)) {
        if ((start > npages)) {
            stop(cat("Please input valid start and end times between ", 
                t1c, " and ", tnc, " or pages between 1 and ", 
                npages, ".\n\n"), call. = FALSE)
        } else if (start < 1) {
#specify a proportional point to start
		start = timestamps[max(floor( start * npages),1)]
	} else {
            start <- timestamps[start]
        }
    }
    if (is.numeric(end)) {
        if ((end < 1)) {
#            stop(cat("Please input valid start and end times between ", 
#                t1c, " and ", tnc, " or pages between 1 and ", 
#                npages, ".\n\n"), call. = FALSE)
#specify a proportional point to end
		end= ceiling(end * npages)
		end = timestamps[end]
        }
        else {
            end <- min(end, npages)
            end <- timestamps[end]
        }
    }
    if (is.character(start)) {
        start <- reformat.time(start, format = tformat)
    }
    if (is.character(end)) {
        end <- reformat.time(end, format = tformat)
    }
    if (end < start) {
        cat("Warning, specified end time is before specified start time.  Reordering.\n")
        tmp <- end
        end <- start
        start <- tmp
    }
    if ((start > tn) | (end < t1)) {
        stop(cat("Please input valid start and end times between ", 
            t1c, " and ", tnc, " or pages between 1 and ", npages, 
            ".\n\n"), call. = FALSE)
    }
    start <- max(start, t1)
    end <- min(end, tn)
    index <- which((timestamps >= start) & (timestamps <= end))
    d1 <- max(which((timestamps - start) <= 0))
    index <- unique(c(d1, index))
    if (length(index) == 0) {
        d1 <- timestampsc[max(which((timestamps - start) < 0))]
        d2 <- timestampsc[min(which((timestamps - end) > 0))]
        if (npages > 15) {
            stop("No pages to process with specified timestamps.  Please try again.\n", 
                call. = FALSE)
        }
        else {
            stop("No pages to process with specified timestamps.  Please try again.Timestamps in binfile are:\n\n", 
                paste(timestampsc, collapse = " \n"), " \n\n", 
                call. = FALSE)
        }
    }
    if (do.temp) {
        temperature <- NULL
    }
    if (calibrate) {
        if (!is.null(gain)) {
            if (!is.numeric(gain)) {
                stop("Please enter 3 valid values for the x,y,z gains.\n")
            }
            else {
                xgain <- gain[1]
                ygain <- gain[2]
                zgain <- gain[3]
            }
        }
        if (!is.null(offset)) {
            if (!is.numeric(offset)) {
                stop("Please enter 3 valid values for the x,y,z offsets.\n")
            }
            else {
                xoffset <- offset[1]
                yoffset <- offset[2]
                zoffset <- offset[3]
            }
        }
        if (!is.null(voltv)) {
            if (!is.numeric(voltv)) {
                stop("Please enter a valid value for the volts.\n")
            }
            else {
                volts <- voltv
            }
        }
        if (!is.null(luxv)) {
            if (!is.numeric(luxv)) {
                stop("Please enter a valid value for the lux.\n")
            }
            else {
                lux <- luxv
            }
        }
    }
    nstreams <- length(index)
    if(warn){
    	if (nstreams > 100) {
        	cat("About to read and process", nstreams, "datasets.  Continue?  Press Enter or control-C to quit.\n")
        	scan(, quiet = TRUE)
    	}
    }
##break up processing if too long?
#
##choose a memory limit
#sizelim = floor(min(memory.limit(), 2000) * 1e6 /(300*12*20))
#
##read blocks of pages at a time as memory allows
#
#
#tempchunk = list(data.out = NULL, page.timestamps = NULL, freq = freq)
#if (!is.null(downsample)){
#	downsampleoffset = 1
#	if (length(downsample) == 2) downsampleoffset = downsample[2]
#	downsample = downsample[1]
#}
#	
#while(length(index) > 0){
#if (is.null(downsample)){
#	tempobj = read.bin(binfile, outfile = NULL, start = index[1], end = index[min(blocksize, length(index))], 
#	    verbose , do.temp, calibrate, gain, 
#	    offset, luxv, voltv, tformat,warn, downsample = NULL, blocksize = Inf)
#} else {
#	tempobj = read.bin(binfile, outfile = NULL, start = index[1], end = index[min(blocksize, length(index))], 
#	    verbose , do.temp, calibrate, gain, 
#	    offset, luxv, voltv, tformat,warn, downsample = c(downsample, downsampleoffset), blocksize = Inf)
#	downsampleoffset = downsample - (nobs*blocksize - downsampleoffset  )%% downsample 
#}
#
#tempchunk$data.out = rbind(tempchunk$data.out, tempobj$data.out)
#tempchunk$page.timestamps = c(tempchunk$page.timestamps, tempobj$page.timestamps)
#index = index[- (1: min(blocksize, length(index)))]
#
#}
#tempchunk$freq  = freq * nrow(tempchunk$data.out)/ (nstreams*nobs)
#    if (!(is.null(outfile))) {
#        save(tempchunk, file = outfile)
#    }
#return(tempchunk)
#}
#
    data <- NULL
#skip to start of data blocks
binfile = file(binfile, "rt")
#skip header
tmpd <- readLines(binfile, n = headlines)
#skip unneeded pages
replicate ( min( index - 1 ), is.character(readLines(binfile, n=reclength)))


numblocks = 1
blocksize = min(blocksize, nstreams)
if (nstreams > blocksize ){
cat("Splitting into ", ceiling(nstreams/blocksize), " chunks.\n") 
numblocks = ceiling(nstreams/blocksize)
}
Fulldat = NULL
Fullindex = index#matrix(index, ncol = numblocks)
index.orig = index


if(!is.null(downsample)){
	downsampleoffset = 1
		if (length(downsample) == 2){
			downsampleoffset = downsample[2]
			downsample = downsample[1]
		}
}

if (test){
close(binfile)
Fulldat = rep(timestamps[index], each = length(freqseq)) + freqseq
if (!is.null(downsample)) Fulldat = bapply.basic( Fulldat, downsample, function(t) t[downsampleoffset])
return(list(data.out = Fulldat, page.timestamps = timestampsc[index.orig], freq=as.double(freq) * length(Fulldat) / (nobs *  nstreams) ))
}

for (blocknumber in 1: numblocks){
index = Fullindex[1:min(blocksize, length(Fullindex))]
Fullindex = Fullindex[-(1:blocksize)]
    proc.file <- NULL
    start.proc.time <- Sys.time()

    tmpd <- readLines(binfile, n = ((max(index) - min(index)) +1) * reclength  )
bseq = (index - min(index) ) * reclength
	    cat("done file reading.  Processing...\n")
	if (is.null(downsample)){
	    data <- strsplit(paste(tmpd[ bseq + position.data], collapse = ""), "")[[1]]

	    if (do.temp) {	
        	tdata <- tmpd[bseq + position.temperature]
	        temp <- as.numeric(substring(tdata, 13, nchar(tdata)))
	        temperature <- rep(temp, each = nobs)
	    }
    # line below added for future beneficial gc
	    rm(tmpd)
  #  data <- check.hex(data) #removed checks because taking too long, convert.hexstream should throw an error anyway.
	    proc.file <- convert.hexstream(data)

    		nn <- rep(timestamps[index], each = length(freqseq)) + freqseq
##So we are downsampling
	} else {
	
		 data <- strsplit(paste(tmpd[ bseq + position.data], collapse = ""), "")[[1]]
	    if (do.temp) {	
        	tdata <- tmpd[bseq + position.temperature]
	        temp <- as.numeric(substring(tdata, 13, nchar(tdata)))
	        temperature <- rep(temp, each = nobs)
	    }
    # line below added for future beneficial gc
	    rm(tmpd)
  #  data <- check.hex(data) #removed checks because taking too long, convert.hexstream should throw an error anyway.
	    proc.file <- convert.hexstream(data)
    		nn <- rep(timestamps[index], each = length(freqseq)) + freqseq
		positions = downsampleoffset + (0: floor(( nobs * length(index)  - downsampleoffset )/downsample)) * downsample
		proc.file = proc.file[, positions]
		temperature = temperature[positions]
		nn  = nn[positions]
	#	freq = freq * ncol(proc.file)/ (nobs * (length(index)))
	downsampleoffset = downsample - (nobs*blocksize - downsampleoffset  )%% downsample 
	}	


    if (calibrate) {
        proc.file[1, ] <- (proc.file[1, ] * 100 - xoffset)/xgain
        proc.file[2, ] <- (proc.file[2, ] * 100 - yoffset)/ygain
        proc.file[3, ] <- (proc.file[3, ] * 100 - zoffset)/zgain
        proc.file[4, ] <- proc.file[4, ] * lux/volts
    }

    proc.file <- t(proc.file)
    proc.file <- cbind(nn, proc.file)
#    rownames(proc.file) <- paste("obs.", 1:nrow(proc.file)) # strip out row labels - waste of memory
    cnames <- c("timestamp", "x", "y", "z", "light", "button")
    if (do.temp) {
        proc.file <- cbind(proc.file, temperature)
        colnames(proc.file) <- c(cnames, "temperature")
    }
    else {
        colnames(proc.file) <- cnames
    }
    end.proc.time <- Sys.time()
    cat("processing took:", format(round(as.difftime(end.proc.time - 
        start.proc.time), 3)), ".\n")

Fulldat= rbind(Fulldat, proc.file)
}

freq = freq * nrow(Fulldat) / (nobs *  nstreams)

close(binfile)
    processedfile <- list(data.out = Fulldat, page.timestamps = timestampsc[index.orig], freq= freq)
class(processedfile) = c("AccData", class(processedfile))
    if (is.null(outfile)) {
        return(processedfile)
    }
    else {
        save(processedfile, file = outfile)
    }
}

