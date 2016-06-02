############################# Poincare plot ####################################
#' Poincare Plot
#' @description  The Poincare plot is a graphical representation of the dependance
#'  between successive RR intervals obtained by plotting the \eqn{RR_{j+\tau}}{RR_(j+tau)}
#'  as a function of \eqn{RR_j}. This dependance is often quantified by fitting an
#'  ellipse to the plot. In this way, two parameters are obtained:  
#' \eqn{SD_1}  and \eqn{SD_2}.
#' \eqn{SD_1} characterizes short-term variability
#' whereas that \eqn{SD_2} characterizes long-term variability.
#' @param HRVData Data structure that stores the beats register and information related to it
#' @param indexNonLinearAnalysis Reference to the data structure that will contain the nonlinear analysis
#' @param timeLag Integer denoting the number of time steps that will be use to construct the 
#' dependance relation:  \eqn{RR_{j+timeLag}}{RR_(j+timeLag)} as a function of \eqn{RR_j}.
#' @param confidenceEstimation Logical value. If TRUE, the covariance matrix is
#' used for fitting the ellipse and computing the \eqn{SD_1} and
#' \eqn{SD_2} parameters (see details). Default: FALSE. 
#' @param confidence The confidence used for plotting the confidence ellipse.
#' @param doPlot Logical value. If TRUE (default), the PoincarePlot is shown. 
#' @param xlab A title for the x axis.
#' @param ylab A title for the y axis.
#' @param main An overall title for the Poincare plot.
#' @param pch Plotting character (symbol to use).
#' @param cex Character (or symbol) expansion.     
#' @param ... Additional parameters for the Poincare plot figure.
#' @details In the HRV literature, when \emph{timeLag = 1}, the \eqn{SD_1} and \eqn{SD_2}
#' parameters are computed using time domain measures. This is the default approach in this
#' function if \emph{timeLag=1}. This function also allows the user to fit a ellipse
#'  by computing the covariance matrix of 
#'  (\eqn{RR_{j}}{RR_(j)},\eqn{RR_{j+\tau}}{RR_(j+tau)})
#' (by setting \emph{confidenceEstimation = TRUE}). In most cases, both approaches
#' yield similar results.
#' @examples
#' \dontrun{
#'  data(HRVProcessedData)
#'  # rename for convenience
#'  hd = HRVProcessedData
#'  hd = CreateNonLinearAnalysis(hd)
#'  hd = PoincarePlot(hd, doPlot = T)
#' }
#' @return  A \emph{HRVData} structure containing a \emph{PoincarePlot} field storing
#' the \eqn{SD_1} and \eqn{SD_2} parameters. The \emph{PoincarePlot} field is
#' stored under the \emph{NonLinearAnalysis} list.
PoincarePlot = function(HRVData, indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis),
                        timeLag = 1, confidenceEstimation = FALSE, confidence = 0.95,
                        doPlot =FALSE, main = "Poincare plot",xlab="RR[n]", 
                        ylab = paste(sep="","RR[n+",timeLag,"]"), pch=1,
                        cex=0.3, ...){
  # -------------------------------------
  # Poincare plot and SD1 and SD2 index
  # -------------------------------------
  CheckAnalysisIndex(indexNonLinearAnalysis, length(HRVData$NonLinearAnalysis), 
                     "nonlinear")
  
  VerboseMessage(HRVData$Verbose, "Calculating SD1 and SD2 parameters")  
  CheckNIHR(HRVData)
  
  if ( (confidence < 0) || (confidence > 1) ) {
    stop("Confidence must be in the [0,1] interval.")
  }
  
  rrSeries = HRVData$Beat$RR
  SD = computeSD(timeSeries = rrSeries, timeLag = timeLag,
                 confidenceEstimation = confidenceEstimation,
                 confidence = confidence)
  # Get critical value depending on the confidenceValue
  cv2 = qchisq(confidence, 2)
  cv = sqrt(cv2)  
  # greatest value is SD2
  sd1 = SD$sd[[2]]
  sd2 = SD$sd[[1]]
  sd1Direction = SD$direction[,2]
  sd2Direction = SD$direction[,1]
  # plot if necessary
  if (doPlot) {
    VerboseMessage(HRVData$Verbose,   
                   paste("Creating Poincare Plot with time lag = ",timeLag))
    # get 2D-phase space
    takens = buildTakens(rrSeries, embedding.dim=2, time.lag = timeLag)
    mu = c(mean(takens[,1]),mean(takens[,2]))
    ifelse(confidenceEstimation, yes = {maxLen = (2.1 - confidence) * sd2},
           no = {maxLen = 1.5 * sd2})
    # plotting
    plot(takens[,1],takens[,2], 'p', col=2, pch=pch, cex=cex, xlab=xlab,
         ylab = ylab, main = main, ...)
    # a will denote the largest axis of the ellipse and b the shortest one.
    drawEllipse(a=sd2*cv,b=sd1*cv,aVector=sd2Direction, bVector=sd1Direction,
                mu=mu)
    drawArrows(mu = mu, a = sd2*cv, b = sd1*cv, aVector = sd2Direction,
               bVector = sd1Direction, maxLen = maxLen )
    legend("bottomright", c("SD1","SD2"),
           col = c(3,4), lty = c(1,1), lwd = c(2,2))
  }
  
  HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$PoincarePlot$SD1 = sd1
  HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$PoincarePlot$SD2 = sd2
  
  VerboseMessage(HRVData$Verbose, paste("SD1 = ", round(sd1,4)))
  VerboseMessage(HRVData$Verbose, paste("SD2 = ", round(sd2,4)))
  
  return(HRVData)
}

computeSD <- function(timeSeries, timeLag, confidenceEstimation, confidence){
  # compute parameters
  if (confidenceEstimation){
    takens = buildTakens(time.series=timeSeries,embedding.dim=2,time.lag=timeLag)
    SD = confidenceEllipse(x = takens[,1], y = takens[,2], confidence=confidence)
    
  }else{
    sd1 = sd( diff(timeSeries) )/sqrt(2)
    sd2 = sqrt( 2*var(timeSeries)-sd1^2 )  
    directions = matrix(c(1,1,-1,1)/sqrt(2),2, byrow=FALSE)
    # return values in decreasing order
    SD = list(sd = c(sd2,sd1), directions = directions)
  }
  return (SD)
}

drawEllipse <- function(a, aVector, b, bVector, mu){
  angle = seq(0, 2*pi, len=100)
  # Get the ellipse
  xEllipse = a * cos(angle)
  yEllipse = b * sin(angle)
  ellipse = cbind(xEllipse, yEllipse)
  # Rotate the ellipse
  auxiliar = cbind(aVector,bVector) %*% t(ellipse)
  ellipse =  t(auxiliar)
  # Plot 
  lines(ellipse + mu,col="black",lwd=5)  
  
}

drawArrows <- function(mu,a,b, aVector=c(1,1)/sqrt(2), bVector=c(-1,1)/sqrt(2),maxLen){
  meanX = mu[[1]]
  meanY = mu[[2]]
  #Plot main axis
  end = c(meanX,meanY) + maxLen * aVector
  arrows(x0 = meanX, y0 = meanY, x1 = end[[1]], y1 = end[[2]],
         angle = 15, col = "black", code = 2, lty = 1, lwd = 2)
  end = c(meanX,meanY) + maxLen * bVector
  arrows(x0 = meanX, y0 = meanY, x1 = end[[1]], y1 =  end[[2]],
         angle = 15, col = "black", code = 2, lty = 1, lwd = 2)
  # Plot axis of the ellipse
  end = c(meanX,meanY) + a * aVector
  arrows(x0 = meanX, y0 = meanY, x1 = end[[1]], y1 = end[[2]],
         angle = 15, col = 3, code = 2, lty = 1, lwd = 2)
  end = c(meanX,meanY) + b * bVector
  arrows(x0 = meanX, y0 = meanY, x1 = end[[1]], y1 = end[[2]],
         angle = 15, col = 4, code = 2, lty = 1, lwd = 2)
  
}


confidenceEllipse <-function(x,y, confidence=0.95){
  
  # Get covariance matrix and mean point
  covMatrix = cov(cbind(x,y))
  # Compute eigenvalues and eigenvectors
  eigenComputations = eigen(covMatrix)
  eigenvalues = eigenComputations$values
  eigenvectors = eigenComputations$vectors
  # rotate eigenvectors to obtain "usual" directions
  # first eigenvector must have all components > 0
  if ( (eigenvectors[1,1]*eigenvectors[1,2]) < 0){
    rotationMatrix = matrix(c(-1,0,0,-1),ncol=2,byrow=FALSE)
    eigenvectors = rotationMatrix %*% eigenvectors
  }
  
  return (list(sd=sqrt(eigenvalues), directions = eigenvectors))
}
