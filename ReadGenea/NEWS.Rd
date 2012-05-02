\name{NEWS}
\title{News for Package \pkg{ReadGenea}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 2.3}{
 \itemize{
	\item BUGFIX: Changed handing of timezones, times and dates should be more reasonable now
	\item Allowed 'day time' references to periods
	\item Implemented virtual object reads - read data in as and when it is required
}
}

\section{Changes in version 2.2}{
 \itemize{
	\item Implemented some class features for matrix-like addressing of read databases
}
}

\section{Changes in version 2.1}{
 \itemize{
    \item Added downsampling and block based loading of data (for very large datasets)    
    \item Added specification of start and end by proportions of data
    \item Some speed improvements/optimisations
    \item Experimental implementations of stft, GFLasso and FLasso.    
}
}

\section{Changes in version 2.0-3}{
 \itemize{
    \item BUGFIX: Issue with reading of dates
}
}

\section{Changes in version 2.0-2}{
  \itemize{
    \item Speed optimisations for read.bin/convert.hexstream
    \item Removed row labels on created object to reduce memory overheads
    \item NEWS file created
  }
}
