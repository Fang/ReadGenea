header.info <-
function (binfile) 
{
    info <- vector("list", 15)
    index <- c(2, 20:22, 26:29, 38:44)
    nm <- NULL
    for (i in 1:length(index)) {
        line <- scan(binfile, what = "", skip = index[i] - 1, 
            nlines = 1, quiet = TRUE, sep = ":")
        el <- paste(line[2:length(line)],collapse=":")
        info[[i]] <- el
        nm[i] <- paste(strsplit(line[1], split = " ")[[1]], collapse = "_")
    }
    info <- as.data.frame(matrix(info), row.names = nm)
#    info <- matrix(info)
    colnames(info) <- "Value"
    info
}

