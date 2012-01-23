\name{convert.hexstream}
\alias{convert.hexstream}
\title{
Converts hexadecimal to expanded data format for Genea devices.
}
\description{
This function unpacks accelerometer data from a binary output file for a Genea device
}
\usage{
convert.hexstream(stream)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stream}{A vector of characters representing numbers in hexadecimal.  Must be a multiple of 12 characters}
}
\details{
The hex stream is converted according to the packet data structure in the following way.  First the stream is arranged into hex triplets representing integers.  These integers are then converted to their 12-bit two complement form.  The last integer is then split into the 10-bit integer representing the light meter; the 11th bit is recorded as the button value, whilst the last bit is discarded. 
}
\value{
A matrix of 5 rows, the first 3 rows correspond to raw accelerometer values (+/-2048); the fourth row corresponds to the light meter measurement, and the last row are the button values.  Note that these values are the uncalibrated accelerometry and light values.
}

\seealso{
\code{\link{read.bin}}
}
\examples{

convert.hexstream(c("0","3","A","F","C","C","3","1","B","9","0","4"))

}
\keyword{manip}
