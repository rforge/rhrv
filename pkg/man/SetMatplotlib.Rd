\name{SetMatplotlib}
\alias{SetMatplotlib}
\title{ Sets Matplotlib mode on or off }
\description{Sets Matplotlib mode on or off. Matplotlib is a boolean component of the data structure HRVData that allows to specify if plotting functions are used to generate graphs. Matplotlib mode requires a working installation of Matplotlib, which is a python plotting library. For more information see http://matplotlib.sourceforge.net}
\usage{
SetMatplotlib(HRVData, Matplotlib)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Matplotlib}{ Boolean argument that allows to specify if Matplotlib is used for plots }
}
\value{
  Returns HRVData, the structure that will contain beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, and one or more analysis structures
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
