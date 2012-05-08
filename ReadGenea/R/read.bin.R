
#internal function for read.bin
convert.hexstream <-function (stream) 
{

maxint <- 2^(12 - 1)

#packet <- as.integer(paste("0x",stream,sep = "")) #strtoi is faster
packet <-bitShiftL(strtoi(stream, 16),4*(2:0))
packet<-rowSums(matrix(packet,ncol=3,byrow=TRUE))

packet[packet>=maxint] <- -(maxint - (packet[packet>=maxint] - maxint))

packet<-matrix(packet,nrow=4)

light <- bitShiftR(packet[4,],2)
button <-bitShiftR(bitAnd(packet[4,],2),1)

packet<-rbind(packet[1:3,],light,button)

packet
}


#reads binary accelerometer data
#blocksize = number of pages to read at a time
read.bin <-
function (binfile, outfile = NULL, start = NULL, end = NULL, 
    verbose = FALSE, do.temp = TRUE,do.volt = TRUE, calibrate = FALSE, downsample = NULL, blocksize = Inf, virtual = FALSE, ...) 
{

 
 # optional argument initialization as NULL. Arguments assigned
 # if they appear in the function call.
 
 opt.args<-c("gain","offset","luxv","voltv", "warn")
 
 warn <- FALSE
 gain<-offset<-NULL
 luxv<-voltv<-NULL
 
 argl<-as.list(match.call())
 
 argind<-pmatch(names(argl),opt.args)
 argind<-which(!is.na(argind))
 
 if(length(argind)>0){
 	called.args<-match.arg(names(argl),opt.args,several.ok=TRUE)
 	for(i in 1:length(called.args)){
 		assign(called.args[i],eval(argl[[argind[i]]]))
 	}
 }
 

#variables for positions and record lengths in file
    nobs <- 300
    headlines <- 59
    reclength <- 10
    position.data <- 10
    position.temperature <- 6
    position.volts <- 7
    orig.opt <- options(digits.secs = 3)
    fc <- file(binfile, "rt")
    tmp <- substring(scan(fc, skip = 22, what = "", n = 3, sep = " ", 
        quiet = TRUE)[3], c(1,2,5),c(1, 3, 6))
#time zone offset from UTC in seconds
	tzone = ifelse(tmp[1] == "-", -1, 1) * (as.numeric(tmp[3]) + 60* as.numeric(tmp[2])) /60
    xgain <- as.integer(scan(fc, skip = 24, what = "", n = 2, 
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
    t1 <- substring(scan(fc, skip = 4, what = "", quiet = TRUE, nlines = 1, sep = "\n"), 11)
    freq <- as.numeric(scan(fc, skip = 4, what = "", n = 2, sep = ":", 
        quiet = TRUE)[2])
#stop reading freq from file, calculate from page times instead (if possible)
    t1c <- parse.time(t1, format = "POSIX", tzone = tzone)
    t1midnight = floor(parse.time(t1, format = "day")) * 60*60*24
    t1 <- parse.time(t1, format = "seconds")
if (npages > 1){
t2 =  parse.time(substring(scan(fc, skip = 4, what = "", quiet = TRUE, nlines = 1, sep = "\n"), 11), format = "seconds")
freq = nobs/(t2 - t1)
###################TODO
}
	freqint = round(freq)
	if (!is.null(downsample)) {
		cat("Downsampling to ", round(freq/downsample[1],2) , " Hz \n")
		if (nobs %% downsample[1] != 0) cat("Warning, downsample divisor not factor of ", nobs, "!\n")
		if ( downsample[1] != floor( downsample[1]) ) cat("Warning, downsample divisor not integer!\n")
	}
    if (verbose) {
        cat("Number of pages in binary file:", npages, "\n")
    }
    close(fc)
    freqseq <- seq(0, by = 1/freq, length = nobs)
    timespan <- nobs/freq
#    t1 <- t1[2:length(t1)]
 #   t1[1] <- substr(t1[1], 6, nchar(t1[1]))
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


#goal is to end up with start, end as page refs
    if (is.numeric(start)) {
        if ((start[1] > npages)) {
            stop(cat("Please input valid start and end times between ", 
                t1c, " and ", tnc, " or pages between 1 and ", 
                npages, ".\n\n"), call. = FALSE)
        } else if (start[1] < 1) {
#specify a proportional point to start
		start = pmax(floor( start * npages),1)
        }
    }
    if (is.numeric(end)) {
        if ((end[1] <= 1)) {
#specify a proportional point to end
		end= ceiling(end * npages)
        }
        else {
            end <- pmin(end, npages)
        }
    }
#parse times, including partial times, and times with day offsets
    if (is.character(start)) {
        start <- parse.time(start, format = "seconds")
	if (start < t1midnight)	start = start + t1midnight
	if (start < t1) start = start + 60*60*24
	start = findInterval(start-0.5, timestamps, all.inside = T)#which(timestamps >= start-(0.5))[1]
	t1 = timestamps[start+1]
    }
    if (is.character(end)) {
        end <- parse.time(end, format = "seconds")
	
	if (end < t1midnight){
		if (end >= 24*60*60){
			end = end + t1midnight
		} else {
			end = end +ceiling((t1 - end)/(60*60*24)) * 60*60*24
		}
	}
	end = findInterval(end, timestamps, all.inside = T) +1#max(which(timestamps<= (end+0.5) ))
    }

    index <-  NULL
	for (i in 1:length(start)){
		index = c(index, start[i]:end[i])
	}

#    d1 <- max(which((timestamps - start) <= 0))
 #   index <- unique(c(d1, index))
    if (length(index) == 0) {
        if (npages > 15) {
            stop("No pages to process with specified timestamps.  Please try again.\n", 
                call. = FALSE)
        }
        else {
            stop("No pages to process with specified timestamps.  Please try again. Timestamps in binfile are:\n\n", 
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

    data <- NULL
#skip to start of data blocks
fc2 = file(binfile, "rt")
#skip header
tmpd <- readLines(fc2, n = headlines)
#skip unneeded pages
replicate ( min( index - 1 ), is.character(readLines(fc2, n=reclength)))


numblocks = 1
blocksize = min(blocksize, nstreams)
if (nstreams > blocksize ){
cat("Splitting into ", ceiling(nstreams/blocksize), " chunks.\n") 
numblocks = ceiling(nstreams/blocksize)
}
Fulldat = NULL
Fullindex = index#matrix(index, ncol = numblocks)
index.orig = index

	    cat("Processing...\n")
pb <- txtProgressBar(min = 0, max = 100,style=1)

    start.proc.time <- Sys.time()
if(!is.null(downsample)){
	downsampleoffset = 1
		if (length(downsample) == 2){
			downsampleoffset = downsample[2]
			downsample = downsample[1]
		}
}

if (virtual){
if (is.null(downsample)) downsample = 1
close(pb)
close(fc2)
#todo...
Fulldat = timestamps[index]
#Fulldat = rep(timestamps[index], each = length(freqseq)) + freqseq
#if (!is.null(downsample)) Fulldat = bapply.basic( Fulldat, downsample, function(t) t[downsampleoffset])
cat("Virtually loaded", length(Fulldat)*length(freqseq)/downsample, "records at", round(freq/downsample,2), "Hz (Will take up approx ", round(56 * as.double(length(Fulldat) * length(freqseq)/downsample )/1000000) ,"MB of RAM)\n")
cat(as.character(chron2(Fulldat[1]))," to ", as.character(chron2(tail(Fulldat,1) + nobs /freq)), "\n")
output = list(data.out = Fulldat, page.timestamps = timestampsc[index.orig], freq= as.double(freq)/downsample , filename =tail(strsplit(binfile, "/")[[1]],1), page.numbers = index.orig, call = argl, nobs = floor(length(freqseq)/downsample) )
class(output) = "VirtAccData"
return(invisible( output  ))
}

voltages = NULL
lastread = min(index) -1
for (blocknumber in 1: numblocks){
index = Fullindex[1:min(blocksize, length(Fullindex))]
Fullindex = Fullindex[-(1:blocksize)]
    proc.file <- NULL

    tmpd <- readLines(fc2, n = (max(index) -lastread) * reclength  )
bseq = (index - lastread -1 ) * reclength
	lastread = max(index)
if (do.volt){
vdata = tmpd[bseq + position.volts]
voltages = c(voltages, as.numeric(substring(tdata, 17, nchar(tdata))))
}
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

	setTxtProgressBar(pb, 100 *  (blocknumber-0.5) / numblocks )

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
 

Fulldat= rbind(Fulldat, proc.file)
	setTxtProgressBar(pb, 100 *  blocknumber / numblocks)
}
close(pb)
freq = freq * nrow(Fulldat) / (nobs *  nstreams)
   end.proc.time <- Sys.time()
    cat("Processing took:", format(round(as.difftime(end.proc.time - 
        start.proc.time), 3)), ".\n")
#cat("Loaded", nrow(Fulldat), "records (Approx ", round(object.size(Fulldat)/1000000) ,"MB of RAM)\n")
#cat(as.character(chron2((Fulldat[1,1])))," to ", as.character(chron2(tail(Fulldat[,1],1))), "\n")

close(fc2)
    processedfile <- list(data.out = Fulldat, page.timestamps = timestampsc[index.orig], freq= freq, filename =tail(strsplit(binfile, "/")[[1]],1), page.numbers = index.orig, call = argl, volt = voltages)
class(processedfile) = "AccData"
    if (is.null(outfile)) {
        return(processedfile)
    }
    else {
        save(processedfile, file = outfile)
    }
}

print.VirtAccData <- function(x){
cat("[Virtual ReadGenea dataset]: ", length(x$data.out)*x$nobs, "records at", round(x$freq,2), "Hz (Approx ", round(object.size(x$data.out)/1000000) ,"MB of RAM if loaded)\n")
cat(as.character(chron2((x$data.out[1])))," to ", as.character(chron2(tail(x$data.out,1) + x$nobs /x$freq)), "\n")
cat("[", x$filename, "]\n")
}
print.AccData <- function(x){
cat("ReadGenea dataset: ", nrow(x$data.out), "records at", round(x$freq,2), "Hz (Approx ", round(object.size(x$data.out)/1000000) ,"MB of RAM)\n")
#if (getOption("chron.year.abb")){
#st = paste("(20", substring( as.character(chron2((x$data.out[1,1]))), 2), sep="")
#en =  paste("(20", substring( as.character(chron2(tail(x$data.out[,1],1))), 2), sep="")
#cat(st," to ", en, "\n")
#} else {
cat(as.character(chron2((x$data.out[1,1])))," to ", as.character(chron2(tail(x$data.out[,1],1))), "\n")
#}
cat("[", x$filename, "]\n")
}



"[.AccData"    <- function (x, i=1:dim(x$data.out)[1], j=NULL, drop=T) {
if (is.null(j)){
x$page.timestamps = x$page.timestamps[ unique(ceiling(i/300))]
x$data.out = x$data.out[i,]
return(x)
} else if (identical(j, 1)){
return(times2(x$data.out[i,j, drop = drop]))
} else {
return(x$data.out[i,j, drop=drop])
}
}


c.AccData <- function(..., recursive=FALSE){
tmp = list(...)
out = list()
out$data.out = NULL
out$page.timestamps = NULL
for (i in 1:length(tmp)){
  out$data.out = rbind(out$data.out, tmp[[i]]$data.out)
  out$page.timestamps = c(out$page.timestamps, tmp[[i]]$page.timestamps)
}
out$freq = tmp[[1]]$freq
out$filename = tmp[[1]]$filename
class(out) = class(tmp[[1]])
out
}
