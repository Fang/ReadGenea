\name{check.hex}
\alias{check.hex}
\title{
A check function for hexadecimal representations.
}
\description{
This function checks whether the elements of a hexadecimal value are valid.

}
\usage{
check.hex(hex)
}
\arguments{
  \item{hex}{The hexadecimal representation to check.}
}
\details{
The function splits the elements of a possible hexadecimal representation, and checks each one for membership in the set \{0-9,A-F,a-f\}.
}
\value{
If valid, the hexdecimal representation is returned, in lower case.
}
\examples{
# create a hex value:

hex<-"F0C12"

check.hex(hex)

}
\keyword{manip}
