\name{LoadApneaWFDB}
\alias{LoadApneaWFDB}
\title{ Loads apnea episodes for WFDB record }
\description{
  Loads the information of apnea episodes and stores it into the data structure containing the beat positions and other related information
}
\usage{
LoadApneaWFDB(HRVData, RecordName, RecordPath = ".", Tag = "APNEA", 
		verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The WFDB file to be used }
  \item{RecordPath}{ The path of the WFDB file }
  \item{Tag}{ to include APNEA episodes }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register and other related information and apnea episodes information
}
\note{
  On Windows and Macosx operating systems is necessary to define\cr 
	  a .Renviron file in the user workspace indicating the directory\cr
	  of the wfdbtools commands. Examples for both OS are given below:
	\cr\cr
	## .Renviron on Windows\cr
    PATH = "c:\\\\cygwin\\\\bin"\cr
    DYLD_LIBRARY_PATH = "c:\\\\cygwin\\\\lib"\cr
	\cr
    ## .Renviron on Macosx\cr
    PATH = "/opt/local/bin"\cr
    DYLD_LIBRARY_PATH = "/opt/local/bin"\cr
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
