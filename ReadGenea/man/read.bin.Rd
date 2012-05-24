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
    verbose = TRUE, do.temp = TRUE,do.volt = TRUE, calibrate = FALSE, downsample = NULL, blocksize , virtual = FALSE, mmap.load = (.Machine$sizeof.pointer >= 8), pagerefs = TRUE, ...)
}
\arguments{
  \item{binfile}{
A filename of a file to process.
}
  \item{outfile}{
An optional filename specifying where to save the processed data object.
}
  \item{start}{Either:
A representation of when in the file to begin processing, see Details.
}
  \item{end}{Either:
A representation of when in the file to end processing, see Details.
}
  \item{verbose}{
	A boolean variable indicating whether some information should be printed during processing should be printed.
}
  \item{do.temp}{
	A boolean variable indicating whether the temperature signal should be extracted.
}
 \item{do.volt}{
	A boolean variable indicating whether the voltage signal should be extracted.
}
  \item{calibrate}{
	A boolean variable indicating whether the raw accelerometer values and the light variable should be calibrated according to the calibration data in the headers.
}
\item{downsample}{
        A variable indicating the type of downsampling to apply to the data as it is loaded. Can take values:\cr\cr
	\code{NULL}: (Default) No downsampling\cr
	Single numeric: Reads every \code{downsample}-th value, starting from the first.\cr
	Length two numeric vector: Reads every \code{downsample[1]}-th value, starting from the \code{downsample[2]}-th.\cr\cr

Non-integer, or non-divisor of 300 downsampling factors are allowed, but will lead to imprecise frequency calculations, leap seconds being introduced, and generally potential problems with other methods. Use with care.
}

\item{blocksize}{
	Integer value giving maximum number of data pages to read in each pass. Defaults to 10000 for larger data files. Sufficiently small sizes will split very large data files to read chunk by chunk, reducing memory requirements for the read.bin function (without affecting the final object), but conversely possibly increasing processing time. Can be set to Inf for no splitting.
}

\item{virtual}{
	logical. If set TRUE, do not do any actual data reading. Instead construct a VirtualAccData object containing header information to allow use with \code{\link{get.intervals}}.
}

\item{mmap.load}{
	logical. If TRUE (Default on 64bit R), use the \code{\link{mmap}} package to process the binfile.
}

\item{pagerefs}{
	A variable that can take two forms, and is considered only for \code{mmap.load = TRUE}\cr\cr
	NULL or FALSE, in which case pagerefs are dynamically calculated for each record. (Default) \cr
	A vector giving sorted byte offsets for each record for mmap reading of data files.\cr
	TRUE, in which case a full page reference table is computed before any processing occurs.\cr\cr

Computing pagerefs takes a little time and so is a little slower. However, it is safer than dynamic computations in the case of missing pages and high temperature variations. Further, once page references are calculated, future reads are much faster, so long as the previously computed references are supplied.
}


\item{...}{Any other optional arguments can be supplied that affect manual calibration and data processing.  These are: \cr\cr

  \code{gain}: a vector of 3 values for manual gain calibration of the raw (x,y,z) axes.  If \code{gain=NULL}, the gain calibration values are taken from within the output file itself.\cr

  \code{offset}: a vector of 3 value for manual offset calibration of the raw (x,y,z) axes.  If \code{offset=NULL}, the offset calibration values are taken from within the output file itself.\cr

  \code{luxv}: a value for manual lux calibration of the light meter.  If \code{luxv=NULL}, the lux calibration value is taken from within the output file itself.\cr

  \code{voltv}: a value for manual volts calibration of the light meter.  If \code{voltv=NULL}, the volts calibration value is taken from within the output file itself.\cr

  \code{warn}: if set to true, give a warning if input file is large, and require user confirmation.
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
	\strong{Reading in an entire .bin file will take a long time if the file contains a lot of datasets. Reading in such files without downsampling can use up all available memory. See \code{\link{memory.limits}}.
}
}
\seealso{
\code{\link{print.AccData}} 
}
\examples{

#Not run: Examples of binary file processing:

binfile  = system.file("binfile/TESTfile.bin", package = "ReadGenea")[1]
procfile<-read.bin(binfile)

print(procfile)
procfile$data.out[1:5,]

procfile2<-read.bin(binfile, calibrate = TRUE)
procfile2$data.out[1:5,]
#procfile2<-read.bin("binfile.txt",start="2010-10-02 12:32:01",end="2010-10-04 12:05:11")

#procfile3<-read.bin("binfile.txt",correct.z=TRUE,calibrate=TRUE)

#processedfile<-read.bin("binfile.txt","myprocessedfile",start=1,end=10,do.temp=TRUE,calibrate=TRUE)

#processedfile2<-read.bin("binfile2.txt",start="2010-10-02 12:32:01",end="2010-10-04 12:05:11",do.temp=TRUE,calibrate=TRUE)
}
\keyword{manip}
