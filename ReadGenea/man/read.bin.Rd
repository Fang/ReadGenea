\name{read.bin}
\alias{read.bin}
\title{
File processing function for binary files.
}
\description{
A function to process binary accelerometer files and convert the information into R objects.
}
\usage{
read.bin(binfile, outfile = NULL, start = NULL, end = NULL, 
verbose = FALSE, do.temp = TRUE, calibrate = FALSE, tformat="seconds", 
warn=FALSE,...)
}
\arguments{
  \item{binfile}{
A filename of a binary file to process.
}
  \item{outfile}{
An optional filename specifying where to save the processed data object.
}
  \item{start}{Either:
A time representation of when the processing is to occur, see \code{\link{reformat.time}} for possible representations, or 
a page number corresponding to the start of the time window, or
a proportional point (between 0 and 1) to begin reading at.
}
  \item{end}{Either:
A time representation of when to end processing, see \code{\link{reformat.time}} for possible representations, or 
a page number corresponding to the end of the time window, or
a proportional point (between 0 and 1) to begin reading at.
}
  \item{verbose}{
	A boolean variable indicating whether information should be printed.
}
  \item{do.temp}{
	A boolean variable indicating whether the temperature signal should be extracted.
}
  \item{calibrate}{
	A boolean variable indicating whether the raw accelerometer values and the light variable should be calibrated.
}
 \item{tformat}{
        A character string indicating which (numerical) format the timestamps should be saved in.  See \code{\link{reformat.time}} for more details.
}
\item{warn}{
        A boolean variable indicating whether the function should warn you before reading in a large number of pages (>=500).  Defaults to \code{FALSE}.
}
\item{...}{Any other optional arguments can be supplied that affect manual calibration and data processing.  These are: \cr\cr
  \code{correct.z}: a boolean variable indicating whether to systematic correction of the z-axis accelerometer values.  See Details for more information.  Defaults to FALSE.\cr

  \code{gainfactor}: an initial value for the multiplication factor for z-axis gain correction (used if \code{correct.z==TRUE}).  Defaults to 1.1.\cr

  \code{gain}: a vector of 3 values for manual gain calibration of the raw (x,y,z) axes.  If \code{gain=NULL}, the gain calibration values are taken from within the output file itself.\cr

  \code{offset}: a vector of 3 value for manual offset calibration of the raw (x,y,z) axes.  If \code{offset=NULL}, the offset calibration values are taken from within the output file itself.\cr

  \code{luxv}: a value for manual lux calibration of the light meter.  If \code{luxv=NULL}, the lux calibration value is taken from within the output file itself.\cr

  \code{voltv}: a value for manual volts calibration of the light meter.  If \code{voltv=NULL}, the volts calibration value is taken from within the output file itself.\cr

  \code{nchunks}: the number of chunks in which to do processing of the data pages.  If not specified, this is calculated given a \code{chunksize} of 500 pages.\cr

  \code{chunksize}: the size (pages) of each data processing step.  See details for more information.
}
\item{blocksize}{
	Integer value giving maximum number of data pages to read in each pass. Defaults to infinite - smaller sizes will split very large data files to read chunk by chunk, reducing memory requirements. \code{blocksize = 1e4} seems often reasonable.
}

}
\details{
The function reads in the desired analysis time window specified by start and end.  These can either be numeric pages, or character timestamps.  The \code{start} and \code{end} variables are checked for consistency.  They are then converted to POSIXct objects and compared to the timestamps within \code{binfile}.  \cr Accordingly, the relevant pages between \code{start} and \code{end} are processed from binary format using \code{\link{convert.hexstream}}.  The processing is done in "chunks" (optionally specified by \code{nchunks} or \code{chunksize}), which can provide both speed and memory benefits for processing big files or a large number of pages.  By default, this is set at 1000-page chunks, which seems to be a suitable choice for the present binary file structure. Note that the processing is conservative, meaning that if a specified timestamp falls within the span of a page, that (whole) page is processed.  \cr After processing, the data is then (optionally) adjusted, first with the systematic correction for the z-axis specified by \code{correct.z}.  For this correction, the observations 2-300 in the z direction are corrected with a rolling multiplication factor set according to the ratio of the first and second x-axis measurements on a page.  The second calibration is then either performed using values from the binary file, or using manually inputted values (using the \code{gain}, \code{offset},\code{luxv} and \code{voltv} arguments).  \cr Note also that the function assumes a header in the binary file of a certain format, 59 lines long.  It also assumes the format of the header is of a standard format so that the calibration data can be extracted correctly.  The processed data object list is optionally saved to a file specified by \code{outfile}.
}
\value{A list of three components:
\item{data.out}{A 6 or 7 column matrix of the processed pages, the rows of which are the processed observations in order of processed pages.  The matrix has columns (timestamp,x-axis,y-axis,z-axis,light,button) or (timestamp,x-axis,y-axis,z-axis,light,button,temperature) if \code{do.temp=TRUE}.}
\item{page.timestamps}{The timestamps as POSIXct representations (as opposed to those within the \code{proc.file} array.}
\item{freq}{The effective sampling frequency (in Hz).}
}
\section{Warning}{
	\strong{Reading in an entire .bin file will take a long time if the file contains a lot of datasets.}
}
\seealso{
\code{\link{convert.hexstream}}, \code{\link{reformat.time}} 
}
\examples{

#Not run: Examples of binary file processing:

#procfile<-read.bin("binfile.txt","myprocessedfile",start=1,end=10,do.temp=TRUE)

#procfile2<-read.bin("binfile.txt",start="2010-10-02 12:32:01",end="2010-10-04 12:05:11")

#procfile3<-read.bin("binfile.txt",correct.z=TRUE,calibrate=TRUE)

#processedfile<-read.bin("binfile.txt","myprocessedfile",start=1,end=10,do.temp=TRUE,calibrate=TRUE)

#processedfile2<-read.bin("binfile2.txt",start="2010-10-02 12:32:01",end="2010-10-04 12:05:11",do.temp=TRUE,calibrate=TRUE)
}
\keyword{manip}
