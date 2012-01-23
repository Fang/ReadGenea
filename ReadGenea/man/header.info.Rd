\name{header.info}
\alias{header.info}
\title{
Get header info from Genea output (.bin) file
}
\description{
Function to extract relevant header fields and values from a file.
}
\usage{
header.info(binfile)
}
\arguments{
  \item{binfile}{The file from which to extract the header}
}
\details{
	The function extracts useful information from a .bin file, such as information about the Genea device used to produce the output, and characteristics of the subject who wore the device.
}
\value{
A \code{data.frame} with extracted header information, each row a particular header field with its value. 
}

\section{Warning}{
This function is specific to current header structure in Genea output files.  If order or field names are changed in future .bin files, this function will have to be edited appropriately.
} 

\examples{

## Not run:

## fileheader <- header.info("binfile.txt")
}
\keyword{manip}
