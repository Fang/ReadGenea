\name{reformat.time}
\alias{reformat.time}
\title{
Reformats a character time representation to a POSIXct object. 
}
\description{
A function to reformat a time representation to a POSIXct object.
}
\usage{
reformat.time(t,format="julian")
}
\arguments{
  \item{t}{A character string representation of a date-time expression, for example colon-separated.}
  \item{format}{A character string indicating which date-time representation to output.  Can be either \code{POSIX}, \code{julian} or \code{seconds}.}
}
\details{
The function separates individual components in a date-time representation, adding enough components if necessary so that the representation has seconds.
Note that the representation should either be colon-separated, or as a space-separated date-time string of the form: Y-m-d h:m.  Once this is done the time
is converted to the required format: \code{POSIX} is the usual R POSIXct format; \code{julian} is the julian date-time representation, modified so that the origin is midnight, 1st January, 2000; \code{seconds} is the number of seconds (including subseconds) since 1st January, 2000.  Note that this is similar to the usual Unix \code{date '+\%s'} function, which is the number of seconds since 1st January, 1970.
}

\value{
A converted date-time string in the specified format.
}
\examples{

#reformat.time("2011:02:11:12:34:02","julian")

#reformat.time("2010-07-19 13:04:01","POSIX")

}
\keyword{manip}
