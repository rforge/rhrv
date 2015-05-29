# Private Function for RHRV functions use only
checkPlotPowerBandOpts <- function(HRVData, indexFreqAnalysis, Tag){
  
  if ((length(HRVData$FreqAnalysis) < indexFreqAnalysis) || (indexFreqAnalysis<1) ) {
    stop("  --- Frequency analysis no.", indexFreqAnalysis," not present!! ---\n    --- Quitting now!! ---\n")
  }
  
  if (!is.null(Tag) & is.null(HRVData$Episodes)) {
    stop("  --- Episodes not present!! ---\n    --- Quitting now!! ---\n")
  }
  
  if (is.null(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF)) {
    stop("   --- Power per band not present!! ---\n")
    return(HRVData)
  }
  
}




#' PlotPowerBand
#' @title Plots power determined by CalculatePowerBand function 
#' @description Plots the power of the heart rate signal at different bands 
#' of physiological interest.
#' @param HRVData Data structure that stores the beats register and 
#' information related to it 
#' @param indexFreqAnalysis Numeric parameter used to reference a particular
#' frequency analysis 
#' @param normalized Plots normalized powers if TRUE 
#' @param hr Plots heart rate signal if TRUE 
#' @param ymax Maximum value for y axis (unnormalized plots) 
#' @param ymaxratio Maximum value for y axis in LF/HF band (normalized and 
#' unnormalized plots) 
#' @param ymaxnorm Maximum value for y axis (normalized plots) 
#' @param Tag Argument that allows to specify if episodes contained in Data are 
#' represented by means of coloured boxes, for example apnoea or oxygen 
#' desaturation, "ALL" for all episodes.
#' @param verbose Deprecated argument maintained for compatibility, use 
#' setVerbose() instead 
#' @references
#' L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri,
#' X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate 
#' variability spectral analysis", Computer Methods and Programs in 
#' Biomedicine 103(1):39-50, july 2011.
#' @author   M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
#' @seealso \code{\link{CalculatePowerBand}} for power calculation
#' @examples
#' \dontrun{
#' # Reading a wfdb register and storing into a data structure:
#' md = CreateHRVData(Verbose = TRUE)
#' md = LoadBeatWFDB(md, RecordName = "register_name", 
#'                   RecordPath = "register_path")
#' 
#' # Calculating heart rate signal:md = BuildNIHR(md)
#' 
#' # Filtering heart rate signal:
#' md = FilterNIHR(md)
#' # Interpolating heart rate signal:
#' md = InterpolateNIHR(md)
#' # Calculating spectrogram and power per band:
#' md = CreateFreqAnalysis(md)
#' md = CalculatePowerBand(md, indexFreqAnalysis = 1, size = 120, 
#'                         shift = 10, sizesp = 1024)
#' # Plotting Power per Band
#' PlotPowerBand(md, hr = TRUE, ymax = 700000, ymaxratio = 4)
#' }
#' @keywords hplot 
PlotPowerBand <-
  function(HRVData, indexFreqAnalysis = length(HRVData$FreqAnalysis),
           normalized = FALSE, hr = FALSE, ymax = NULL, ymaxratio = NULL,
           ymaxnorm = 1, Tag = NULL, verbose = NULL) {
    # --------------------
    # Plots power per band
    # --------------------
    #  indexFreqAnalysis: index of an existing frequency analysis to use
    #   normalized: plots normalized powers if TRUE
    # 	hr: plots heart rate signal if TRUE
    # 	ymax: maximum value for y axis (unnormalized plots)
    # 	ymaxratio: maximum value for y axis in LF/HF band (normalized and unnormalized plots)
    #	Tag -> Tags of episodes to include in the plot
    #    "all" includes all types
    
    
    if (!is.null(verbose)) {
      cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
      SetVerbose(HRVData,verbose)
    }
    
    if (HRVData$Verbose) {
      cat("** Plotting power per band **\n")
    }
    checkPlotPowerBandOpts(HRVData, indexFreqAnalysis, Tag)
    # if (is.null(ymax)) {
    # 	ymax = max(max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF) , max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF) , max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF) , max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF) )
    # }
    
    
    # normalization
    if (normalized) {
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF=(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF))/(max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF)-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF))
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF=(HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF))/(max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF)-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF))
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF=(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF))/(max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF)-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF))
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF=(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF))/(max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF)-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF))
      # HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF=(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF))/(max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF)-min(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF))
      if (HRVData$Verbose) {
        cat("   Power per band normalized\n")
      }
    }
    
    
    if (hr)
      numfilas=6
    else
      numfilas=5
    
    
    # lframes is the number of frames for plotting power per band
    lframes=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)
      
    # Episodes
    if (!is.null(Tag)) {
      
      if (Tag[1]=="all") {
        Tag=levels(HRVData$Episodes$Type)
      }
      
      if (HRVData$Verbose) {
        cat("   Episodes in plot:",Tag,"\n")
      }
      
      # Data for representing episodes
      EpisodesAuxLeft=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag]
      EpisodesAuxLeftFrame=EpisodesAuxLeft*lframes/(tail(HRVData$Beat$Time,1)-head(HRVData$Beat$Time,1)) # Beg of episodes (frames)
      EpisodesAuxRight=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag] + 
        HRVData$Episodes$Duration[HRVData$Episodes$Type %in% Tag]
      EpisodesAuxRightFrame=EpisodesAuxRight*lframes/(tail(HRVData$Beat$Time,1)-head(HRVData$Beat$Time,1)) # Beg of episodes (frames)
      EpisodesAuxType=HRVData$Episodes$Type[HRVData$Episodes$Type %in% Tag]
      if (HRVData$Verbose) {
        cat("   No of episodes:",length(EpisodesAuxLeft),"\n")
      }
      
      Pal=rainbow(length(Tag))
      Bor=Pal[match(EpisodesAuxType,Tag)]
      
      
      EpisodesLeft=HRVData$Episodes$InitTime # Beg of episodes (seconds)
      EpisodesLeftFrame=EpisodesLeft*lframes/(tail(HRVData$Beat$Time,1)-head(HRVData$Beat$Time,1)) # Beg of episodes (frames)
      EpisodesRight=HRVData$Episodes$InitTime+HRVData$Episodes$Duration # Beg of episodes (seconds)
      EpisodesRightFrame=EpisodesRight*lframes/(tail(HRVData$Beat$Time,1)-head(HRVData$Beat$Time,1)) # Beg of episodes (frames)
    }
    previousPar = par()
    par(mfrow=c(numfilas,1),omi=c(0,0,0,0),mai=c(0,0,0,0),mar=c(2,4,1,1),oma=c(1,0,2,0),mgp=c(1.5,.5,0))
    
    # ---------- LF/HF ----------
    if (is.null(ymaxratio)) {
      ymaxratio = max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF)
    }
    
    mfg=c(1,1,numfilas,1)
    plot(seq(from=0,to=lframes,length.out=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)),HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF,type='l',xlab="",ylab="LF/HF",ylim=c(0,ymaxratio*1.1))
    if (!is.null(Tag)) {
      EpisodesAuxTop=c(ymaxratio*1.09,ymaxratio*1.04)
      EpisodesAuxBottom=c(ymaxratio*1.06,ymaxratio*1.01)
      rect(EpisodesAuxLeftFrame,EpisodesAuxBottom,EpisodesAuxRightFrame,EpisodesAuxTop,border=Bor,col=Bor)
      
      for (i in 1:length(EpisodesAuxLeftFrame)) {
        lines(rep(EpisodesAuxLeftFrame[i],times=2),c(0,ymaxratio*1.1),lty=2,col=Bor[i])
        lines(rep(EpisodesAuxRightFrame[i],times=2),c(0,ymaxratio*1.1),lty=2,col=Bor[i])
      }
      
      
      par(xpd=NA) 
      legend(lframes/2,ymaxratio,legend=Tag,fill=Pal,cex=0.9,ncol=length(Tag),xjust=0.5,yjust=-0.2,bty="n")
      
    }
    if (HRVData$Verbose) {
      cat("   Plotted LF/HF\n")
    }
    
    # ---------- ULF ----------
    if (normalized==TRUE) {
      ymaxv=c(0,ymaxnorm)
    } else if (!is.null(ymax)) {
      ymaxv=c(0,ymax)
    } else {
      ymaxv = c(0,max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF))
    }
    
    mfg=c(1,2,numfilas,1)
    plot(seq(from=0,to=lframes,length.out=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)),
         HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF,type='l',xlab="",ylab="ULF",ylim=ymaxv)
    if (!is.null(Tag)) {
      for (i in 1:length(EpisodesAuxLeftFrame)) {
        lines(rep(EpisodesAuxLeftFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
        lines(rep(EpisodesAuxRightFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
      }
    }
    if (HRVData$Verbose) {
      cat("   Plotted ULF\n")
    }
    
    # ---------- VLF ----------
    if (normalized==TRUE) {
      ymaxv=c(0,ymaxnorm)
    } else if (!is.null(ymax)) {
      ymaxv=c(0,ymax)
    } else {
      ymaxv = c(0,max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF))
    }
    
    mfg=c(1,3,numfilas,1)
    plot(seq(from=0,to=lframes,length.out=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)),
         HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF,type='l',xlab="",ylab="VLF",ylim=ymaxv)
    if (!is.null(Tag)) {
      for (i in 1:length(EpisodesAuxLeftFrame)) {
        lines(rep(EpisodesAuxLeftFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
        lines(rep(EpisodesAuxRightFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
      }
    }
    if (HRVData$Verbose) {
      cat("   Plotted VLF\n")
    }
    
    # ---------- LF ----------
    if (normalized==TRUE) {
      ymaxv=c(0,ymaxnorm)
    } else if (!is.null(ymax)) {
      ymaxv=c(0,ymax)
    } else {
      ymaxv = c(0,max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF))
    }
    
    mfg=c(1,4,numfilas,1)
    plot(seq(from=0,to=lframes,length.out=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)),
         HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF,type='l',xlab="",ylab="LF",ylim=ymaxv)
    if (!is.null(Tag)) {
      for (i in 1:length(EpisodesAuxLeftFrame)) {
        lines(rep(EpisodesAuxLeftFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
        lines(rep(EpisodesAuxRightFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
      }
    }
    if (HRVData$Verbose) {
      cat("   Plotted LF\n")
    }
    
    # ---------- HF ----------
    if (normalized==TRUE) {
      ymaxv=c(0,ymaxnorm)
    } else if (!is.null(ymax)) {
      ymaxv=c(0,ymax)
    } else {
      ymaxv = c(0,max(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF))
    }
    
    mfg=c(1,5,numfilas,1)
    texto4="No. of frames"
    plot(seq(from=0,to=lframes,length.out=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)),
         HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF,type='l',xlab=texto4,ylab="HF",ylim=ymaxv)
    if (!is.null(Tag)) {
      for (i in 1:length(EpisodesAuxLeftFrame)) {
        lines(rep(EpisodesAuxLeftFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
        lines(rep(EpisodesAuxRightFrame[i],times=2),c(ymaxv[1],ymaxv[2]),lty=2,col=Bor[i])
      }
    }
    if (HRVData$Verbose) {
      cat("   Plotted HF\n")
    } 
    
    # ---------- HR ----------
    if (numfilas==6) {
      mfg=c(1,6,numfilas,1)
      # lsecs is the duration of the record in seconds for plotting heart rate signal
      lsecs=tail(HRVData$Beat$Time,1)-head(HRVData$Beat$Time,1)
      plot(seq(from=0,to=lsecs,length.out=length(HRVData$HR)),
           HRVData$HR,type='l',xlab="Time (sec.)",ylab="HR (bps)")
      if (HRVData$Verbose) {
        cat("   Plotted HRV\n")
      }
      
      if (!is.null(Tag)) {
        for (i in 1:length(EpisodesAuxLeftFrame)) {
          lines(rep(EpisodesAuxLeft[i],times=2),c(min(HRVData$HR),max(HRVData$HR)),lty=2,col=Bor[i])
          lines(rep(EpisodesAuxRight[i],times=2),c(min(HRVData$HR),max(HRVData$HR)),lty=2,col=Bor[i])
        }
        #rect(EpisodesAuxLeft,rep(min(HRVData$HR),times=length(EpisodesAuxLeft)),EpisodesAuxRight,rep(max(HRVData$HR),times=length(EpisodesAuxLeft)),border=Bor)
      }
      # --------------------
    }
    
    if (HRVData$Verbose & !is.null(Tag)) {
      cat("   Episodes plotted\n")
    }
    
    
    if ((normalized == TRUE) && (numfilas == 6)) 
      title(main="Normalized power bands of HRV and Heart Rate Signal",outer=TRUE)
    else if ((normalized == FALSE) && (numfilas == 6))
      title(main="Power bands of HRV and Heart Rate Signal",outer=TRUE)
    else if ((normalized == TRUE) && (numfilas != 6))
      title(main="Normalized power bands of HRV",outer=TRUE)
    else
      title(main="Power bands of HRV",outer=TRUE)
    
    if (HRVData$Verbose) {
      cat("   Power per band plotted\n")
    }	 
    
    #restore previous graphical parameters
    par(mfrow=previousPar$mfrow, omi=previousPar$omi, mai=previousPar$mai,
        mar=previousPar$mar,oma=previousPar$oma,mgp=previousPar$mgp)
    
  }





#' PlotSinglePowerBand
#' @description Plots a concrete power band computed by the CalculatePowerBand 
#' function
#' @param HRVData Data structure that stores the beats register and 
#' information related to it 
#' @param indexFreqAnalysis Numeric parameter used to reference a particular
#' frequency analysis 
#' @param band The frequency band to be plotted. Allowd bands are "ULF", "VLF", "LF" 
#' (default), "HF" and "LF/HF")
#' @param normalized Plots normalized powers if TRUE 
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis
#' @param type 1-character string giving the type of plot desired. See 
#' \code{\link[graphics]{plot.default}}.
#' @param Tag List of tags to specify which episodes, as apnoea or oxygen 
#' desaturation, are included in the plot. Tag="all" plots all episodes present
#' in the data.
#' @param eplim Two-component vector specifying the y-range (min,max) for the
#' vertical lines limiting each episode.
#' @param epColorPalette Vector specifying the color of each of the episodes that 
#' will be plotted. The length of colorPalette should be equal or greater than 
#' the number of different episodes to be plotted.
#' @param markEpisodes Boolean specyfing if a horizontal mark should be included 
#' for each of the episodes.
#' @param ymark Two-component vector specifying the y-range (min,max) for the
#'  horizontal marks. Only used if markEpisodes = TRUE.
#' @param showEpLegend Boolean argument. If TRUE, a legend of the episodes is 
#' included.
#' @param epLegendCoords Two-component vector specifiying the coordinates where
#' the legend should be placed. By defaul, the legend is placed on top of the
#' plot.
#' @param ... Other graphical parameters for plotting the power band. See 
#' \code{\link[graphics]{plot.default}}.
#' @author C.A. Garcia
#' @seealso \code{\link{CalculatePowerBand}} for power calculation
#' @examples
#' \dontrun{
#' 
#' # Read file "a03" from the physionet apnea-ecg database
#' library(RHRV)
#' HRVData <- CreateHRVData()
#' HRVData <- LoadBeatWFDB(HRVData,RecordName="test_files/WFDB/a03")
#' HRVData <- LoadApneaWFDB(HRVData,RecordName="test_files/WFDB/a03")
#' # Calculating heart rate signal:
#' HRVData <- BuildNIHR(HRVData)
#'  
#' # Filtering heart rate signal:
#' HRVData <- FilterNIHR(HRVData)
#' 
#' # Interpolating heart rate signal:
#' HRVData = InterpolateNIHR(HRVData)
#' 
#' HRVData = CreateFreqAnalysis(HRVData)
#' HRVData = CalculatePowerBand(HRVData, indexFreqAnalysis = 1,
#'           size = 300, shift = 60, sizesp = 1024)
#'           
#' layout(matrix(1:4, nrow = 2))
#' PlotSinglePowerBand(HRVData, 1, "VLF", Tag = "APNEA", epColorPalette = "red",
#'                     epLegendCoords = c(2000,7500))
#' PlotSinglePowerBand(HRVData, 1, "LF", Tag = "APNEA", epColorPalette = "red",
#'                     eplim = c(0,6000),
#'                     markEpisodes = F, showEpLegend = FALSE)
#' PlotSinglePowerBand(HRVData, 1, "HF", Tag = "APNEA", epColorPalette = "red",
#'                     epLegendCoords = c(2000,1700))
#' PlotSinglePowerBand(HRVData, 1, "LF/HF", Tag = "APNEA", epColorPalette = "red",
#'                     eplim = c(0,20),
#'                     markEpisodes = F, showEpLegend = FALSE)
#' # Reset layout
#' par(mfrow = c(1,1))
#' }
PlotSinglePowerBand <- 
  function(HRVData, indexFreqAnalysis = length(HRVData$FreqAnalysis),
           band = c("ULF","VLF","LF","HF","LF/HF")[3],
           normalized = FALSE,  main = paste(band, "Power Band"), xlab ="Time",
           ylab = paste("Power in",band), type = "l",
           Tag = NULL,  eplim = NULL,  epColorPalette = NULL,
           markEpisodes = TRUE, ymark = NULL,
           showEpLegend = TRUE, epLegendCoords = NULL, 
           ...){
    
    if (HRVData$Verbose) {
      cat("** Plotting power per band **\n")
    }
    
    checkPlotPowerBandOpts(HRVData, indexFreqAnalysis, Tag)
    
    if (!(band %in% c("ULF","VLF","LF","HF","LF/HF"))){
      stop ( paste("** Invalid Frequency band name **",
                   "** Allowed values are ULF, VLF, LF, HF and LF/HF **",
                   sep="\n"))
    }
    
    if (band == "LF/HF"){band = "LFHF"}
    
    bandData = HRVData$FreqAnalysis[[indexFreqAnalysis]][[band]]
    if (normalized) {
      bandData = bandData/(max(bandData) - min(bandData))
      
      if (HRVData$Verbose) {
        cat("   Power per band normalized\n")
      }
    }
    
    timeAxis = HRVData$FreqAnalysis[[indexFreqAnalysis]]$Time
    
    plot(timeAxis, bandData, main = main, xlab = xlab, 
         ylab = ylab, type = type, ... )
    
    # Episodes plotting
    if (!is.null(Tag)){
      ylim = list(...)$ylim
      if (is.null(ylim)) ylim = range(bandData, finite = TRUE)
      if (is.null(eplim)) eplim = c(ylim[1], 0.90 * ylim[2])
      if (is.null(ymark)) ymark = c(0.99 * eplim[2], eplim[2])
      OverplotEpisodes(HRVData, Tag=Tag, eplim = eplim,
                       showEpLegend = showEpLegend,                     
                       epLegendCoords = epLegendCoords,
                       epColorPalette = epColorPalette,
                       markEpisodes = markEpisodes,
                       ymark = ymark)
    }
  }
