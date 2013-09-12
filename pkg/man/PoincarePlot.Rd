\name{PoincarePlot}
\alias{PoincarePlot}
\title{Poincare Plot}
\usage{
  PoincarePlot(HRVData, timeLag = 1,
    indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis),
    doPlot = FALSE)
}
\arguments{
  \item{HRVData}{Data structure that stores the beats
  register and information related to it}

  \item{indexNonLinearAnalysis}{Reference to the data
  structure that will contain the nonlinear analysis}

  \item{timeLag}{Integer denoting the number of time steps
  that will be use to construct the dependance relation:
  \eqn{RR_{j+timeLag}}{RR_(j+timeLag)} as a function of
  \eqn{RR_j}.}

  \item{doPlot}{Logical value. If TRUE (default), the
  PoincarePlot is shown.}
}
\value{
  A \emph{HRVData} structure containing a
  \emph{PoincarePlot} field storing the \eqn{SD_1} and
  \eqn{SD_2} parameters. The \emph{PoincarePlot} field is
  stored under the \emph{NonLinearAnalysis} list.
}
\description{
  Poincare Plot
}
\details{
  The Poincare plot is a graphical representation of the
  dependance between successive RR intervals obtained by
  plotting the \eqn{RR_{j+\tau}}{RR_(j+tau)} as a function
  of \eqn{RR_j}. This dependance is often quantified by
  fitting an ellipse to the plot. In this way, two
  parameters are obtained: \eqn{SD_1} (The standard
  deviation of the points perpendicular to the line of
  identity) and \eqn{SD_2} (The standard deviation along
  the line of identity). \eqn{SD_1} characterizes
  short-term variability whereas that \eqn{SD_2}
  characterizes long-term variability.
}

