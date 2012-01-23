check.hex <-
function (hex) 
{
    hex <- tolower(hex)
    hexspl <- strsplit(hex, "")[[1]]
    ishex <- all(hexspl %in% format.hexmode(0:15))
    if (!ishex) {
        stop("You need to supply a valid hex string!\n")
    }
    else {
	return(hex)
    }

}

