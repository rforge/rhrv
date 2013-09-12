############################# Poincare plot ####################################
#' Poincare Plot
#' @details  The Poincare plot is a graphical representation of the dependance between successive RR
#' intervals obtained by plotting the \eqn{RR_{j+\tau}}{RR_(j+tau)} as a function of \eqn{RR_j}. This dependance
#' is often quantified by fitting an ellipse to the plot. In this way, two parameters are obtained:  
#' \eqn{SD_1} (The standard deviation of the points perpendicular to the line of identity) and \eqn{SD_2}
#' (The standard deviation along the line of identity). \eqn{SD_1} characterizes short-term variability
#' whereas that \eqn{SD_2} characterizes long-term variability.
#' @param HRVData Data structure that stores the beats register and information related to it
#' @param indexNonLinearAnalysis Reference to the data structure that will contain the nonlinear analysis
#' @param timeLag Integer denoting the number of time steps that will be use to construct the 
#' dependance relation:  \eqn{RR_{j+timeLag}}{RR_(j+timeLag)} as a function of \eqn{RR_j}.
#' @param doPlot Logical value. If TRUE (default), the PoincarePlot is shown. 
#' @return  A \emph{HRVData} structure containing a \emph{PoincarePlot} field storing the \eqn{SD_1}
#' and \eqn{SD_2} parameters. The \emph{PoincarePlot} field is stored under the \emph{NonLinearAnalysis} list.
PoincarePlot = function(HRVData, timeLag = 1, indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis),
                        doPlot =FALSE){
  # -------------------------------------
  # Poincare plot and SD1 and SD2 index
  # -------------------------------------
  
  
  checkingNonLinearIndex(indexNonLinearAnalysis, length(HRVData$NonLinearAnalysis))
  
  if (HRVData$Verbose){
    cat("  --- Calculating SD1 and SD2 parameters ---\n")  
  }
  
  if (is.null(HRVData$Beat$niHR)){
    stop("RR time series not present\n")
  }
  
  RRlen = length(HRVData$Beat$niHR)
  # compute the ellipse axis 
  RRmean = mean(HRVData$Beat$niHR)
  sd1 = sd(diff(HRVData$Beat$niHR))/sqrt(2)
  sd2 = sqrt(2*var(HRVData$Beat$niHR)-sd1^2)
  # plot if necessary
  if (doPlot){
    if (HRVData$Verbose){
      cat(" --- Creating Poincare Plot with time lag =",timeLag,"---\n")
    }
    # plot the ellipse adjusting the data
    # t is the parametric angle
    t = seq(0,2*pi,0.01)
    # parametric ellipse with an angle of 45º between the x-axis and the major axis of the ellipse.
    X = RRmean + sd2*cos(t)*cos(pi/4)-sd1*sin(t)*sin(pi/4)
    Y = RRmean + sd2*cos(t)*sin(pi/4)+sd1*sin(t)*cos(pi/4)
    maxAxis=max(sd2,sd1)
    # plotting
    takens = buildTakens(HRVData$Beat$niHR, embedding.dim=2, time.lag = timeLag)
    plot(takens[,1],takens[,2], 'p', col="blue", pch=1, cex=0.3, xlab="RR[n]",
         ylab = paste(sep="","RR[n+",timeLag,"]"), main = "Poincare plot")
    lines(X,Y,col="black",lwd=2)
    arrows(x0 = RRmean, y0 = RRmean, x1 = RRmean + 2*maxAxis, y1 = RRmean + 2*maxAxis,
           angle = 15, col = "black", code = 2, lty = 1, lwd = 2)
    arrows(x0 = RRmean, y0 = RRmean, x1 = RRmean-2*maxAxis, y1 = RRmean + 2*maxAxis, 
           angle = 15, col = "black", code = 2, lty = 1, lwd = 2)
    arrows(x0 = RRmean, y0 = RRmean, x1 = RRmean + sd2/sqrt(2), y1 = RRmean + sd2/sqrt(2),
           angle = 15, col = "green", code = 2, lty = 1, lwd = 2)
    arrows(x0 = RRmean, y0 = RRmean, x1 = RRmean-sd1/sqrt(2), y1 = RRmean + sd1/sqrt(2),
           angle = 15, col = "red", code = 2, lty = 1, lwd = 2)
    legend("bottomright",c("SD2","SD1"),col=c("green","red"),lty=c(1,1),lwd=c(2,2))
  }
  HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$PoincarePlot$SD1 = sd1
  HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$PoincarePlot$SD2 = sd2
  
  if (HRVData$Verbose){
    cat(" --- SD1 = ",sd1," ---\n")
    cat(" --- SD2 = ",sd2," ---\n")
  }
  
  return(HRVData)
  
}