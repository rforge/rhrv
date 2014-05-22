#' PlotHR
#' @title Simple plot of interpolated heart rate 
#' @description  Plots in a simple way the interpolated instantaneous heart
#'  rate signal.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param Tag List of tags to specify which episodes, as apnoea or oxygen 
#' desaturation, are included in the plot. Tag="all" plots all episodes 
#' present in the data. 
#' @param verbose Deprecated argument maintained for compatibility, use 
#' SetVerbose() instead.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab a label for the y axis
#' @param type 1-character string giving the type of plot desired. See 
#' \code{\link[graphics]{plot.default}}.
#' @param ylim The y limits of the plot.
#' @param ... Other graphical parameters. See 
#' \code{\link[graphics]{plot.default}}.
#' @references L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri,
#' X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate
#' variability spectral analysis", Computer Methods and Programs in
#' Biomedicine 103(1):39-50, july 2011.
#' @author M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila, C.A. Garcia
#' @keywords aplot
PlotHR <-
function(HRVData, Tag=NULL, verbose=NULL, 
         main = "Interpolated instantaneous heart rate",
         xlab="time (sec.)", ylab="HR (beats/min.)",type="l",ylim=NULL, ...) {
# -----------------------------
# Plots interpolated Heart Rate
# -----------------------------
#	Tag -> Tags of episodes to include in the plot
#    "all" includes all types


	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n
        --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Plotting interpolated instantaneous heart rate **\n");
	}
   
   if (!is.null(Tag) & is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present ---\n    --- Quitting now!! ---\n")
   }

	if (is.null(HRVData$HR)) { 
      stop("  --- Interpolated heart rate not present ---\n    --- Quitting now!! ---\n")
	}
	
	npoints = length(HRVData$HR)
	if (HRVData$Verbose) {
		cat("   Number of points:",npoints,"\n");
	}

	
	first = head(HRVData$Beat$Time,1)
	last = tail(HRVData$Beat$Time,1)
	x=seq(first,last,length.out=npoints)

	HRMin=min(HRVData$HR)
	HRMax=max(HRVData$HR)
	HRDiff=HRMax-HRMin

	if (!is.null(Tag)) {
		if (Tag[1]=="all") {
			Tag=levels(HRVData$Episodes$Type)
		}

		if (HRVData$Verbose) {
			cat("   Episodes in plot:",Tag,"\n")
		}
	}

  if (is.null(ylim)){
    ylim = c(HRMin-0.1*HRDiff,HRMax)
  }
	plot(x,HRVData$HR,type=type, xlab=xlab, ylab=ylab,
       ylim=ylim, ...)
	grid()
	
	if (!is.null(Tag)) {
		# Data for representing episodes
		EpisodesAuxLeft=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag]
		EpisodesAuxBottom=c(HRMin-0.09*HRDiff,HRMin-0.04*HRDiff)
		EpisodesAuxRight=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag] + 
			HRVData$Episodes$Duration[HRVData$Episodes$Type %in% Tag]
		EpisodesAuxTop=c(HRMin-0.07*HRDiff,HRMin-0.02*HRDiff)
		EpisodesAuxType=HRVData$Episodes$Type[HRVData$Episodes$Type %in% Tag]

		Pal=rainbow(length(Tag))
		Bor=Pal[match(EpisodesAuxType,Tag)]

		cat("   No of episodes:",length(EpisodesAuxLeft),"\n")
		cat("   No of classes of episodes:",length(Pal),"\n")

		rect(EpisodesAuxLeft,EpisodesAuxBottom,EpisodesAuxRight,EpisodesAuxTop,border=Bor,col=Bor)
		for (i in 1:length(EpisodesAuxLeft)) {

			lines(rep(EpisodesAuxLeft[i],times=2),c(HRMin-0.1*HRDiff,HRMax),lty=2,col=Bor[i])
			lines(rep(EpisodesAuxRight[i],times=2),c(HRMin-0.1*HRDiff,HRMax),lty=2,col=Bor[i])
		}

		legend("topright",inset=0.01,legend=Tag,fill=Pal,cex=0.6,horiz=FALSE,bg='white')
	}

	title(main=main)

}

