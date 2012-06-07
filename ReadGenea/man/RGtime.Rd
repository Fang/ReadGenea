\name{RGtime}
\alias{RGtime}
\alias{convert.time}
\alias{as.RGtime}
\alias{format.RGtime}
\alias{axis.RGtime}
\alias{pretty.RGtime}
\title{
Date time handling for the ReadGenea package.
}
\description{
Stores date time data as a numeric, with facility for pretty printing and axis commands.
}
\usage{
convert.time(x, format = NULL)
as.RGtime(x, format = NULL, \dots)
\method{format}{RGtime}(x, format = NULL, \dots)
\method{axis}{RGtime}(side, x=NULL, at=NULL, format = NULL,labels  = TRUE, add = TRUE,  \dots)
\method{pretty}{RGtime}(x, n = 5, ...)
}
\arguments{
  \item{x}{Object to process. For \code{convert.time}, must be numeric. For \code{as.RGtime} may be numeric or character. For \code{format.RGtime}, a RGtime object, or a numeric.}
  \item{format}{A character string indicating the form of output. See \code{\link{strptime}} for details. If NULL, will be automatically chosen.}
  \item{add}{logical. If TRUE, actually plot the axis.}
  \item{at, side, labels}{Additional arguments as in \code{\link{axis}}.}
  \item{n}{Approximate number of breakpoints.}
  \item{\dots}{Additional arguments to be passed to \code{\link{parse.time}}, \code{\link{as.numeric}}, \code{\link{format.POSIXct}}, \code{\link{axis}}, \code{\link{pretty.POSIXt}}. }
}
\details{
The RGtime class handles dates and times for the ReadGenea class. The class treats dates as numerics denoting seconds since the UNIX epoch, with potentially a string attached specifying the format to print in. Unlike \code{POSIXct}, we avoid some of the processing, especially with respect to time zones, and allow some more flexibility in time computation and display. A range of operators are defined.

\code{convert.time} converts numerics to RGtime objects. The \code{format} argument allows a format string to be attached specifying the default format to display in. \code{as.RGtime} is a wrapper to \code{convert.time}, that when supplied with character input, coerces the value first to numeric using \code{parse.time}.

\code{format.RGtime} formats RGtime objects for pretty printing. If \code{format} is provided as argument, that is used. Else, if the \code{format} attribute is set on \code{x}, that is used. Finally, if formats are not provided, and \code{x} is of length greater than one, the range of values of \code{x} is used to decide the units displayed. Numerics are also accepted - they are coerced to RGtime.

\code{axis.RGtime} is used to plot RGtime axis, choosing, by default, breakpoints that give 'pretty' sub intervals. Note that \code{\link{plot.default}} uses \code{axis.RGtime} by default if supplied with a RGtime object in one of the directions. However, \code{\link{image.default}} based functions do not use the class axis functions, so axes must be plotted manually.

\code{pretty.RGtime} computes 'pretty' breakpoints, using the algorithm of \code{pretty.POSIXt}. Attributes are preserved.

}

\value{
For \code{convert.time}, \code{as.RGtime} and \code{pretty.RGtime}, a RGtime object.

For \code{format.RGtime} a character string representation.

For \code{axis.RGtime} a list containing positions and labels for axis markers.
}
\seealso{
\code{\link{parse.time}}, \code{\link{get.intervals}}, \code{\link{print.AccData}}
}
\examples{
as.RGtime("00:01")
#format is automatically set
convert.time(1:10)
convert.time(1:10*1000)
#we add a different default format
convert.time(1:10*1000, "\%H:\%M:\%OS3") -> t
t
str(t)
#we override format with our own
format(t, format = "\%a \%d/\%m/\%y \%H:\%M:\%OS3")

#plot calls axis.RGtime automatically. Notice
#that the format attribute is used.
plot(t, 1:10)
#strip out the default format
t2 = convert.time(t, format = NULL)
plot(t2, 1:10)

#image plots are a bit more complex

Z = matrix(rnorm(100), 10)
image(x = t, y = t2, z = Z, axes = FALSE)
axis.RGtime(x = t2, side = 2)
Axis(x = t, side = 1) #Axis also works
box() #complete the bounding box

#custom axes
plot(t2, 1:10, xaxt = "n")
axis.RGtime(at = pretty(t2, 20) , side = 1)
}
\keyword{manip}
