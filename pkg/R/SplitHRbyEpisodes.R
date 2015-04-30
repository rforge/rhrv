SplitHRbyEpisodes <-
function(HRVData, Tag="", verbose=NULL) {
# -------------------------------------------------
# Splits Heart Rate Data using Episodes information
# -------------------------------------------------
#  Tag -> specifies tag of episodes
#  Returns a list with two vectors: InEpisodes and OutEpisodes

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Splitting heart rate signal using episodes **\n");
   }

   if (is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present\n    --- Quitting now!! ---\n")
   }

	if (is.null(HRVData$HR)) { 
      stop("  --- Interpolated heart rate not present\n    --- Quitting now!! ---\n")
	}

	if (HRVData$Verbose) {
      if (Tag=="") {
		   cat("   No tag was specified\n");
      } else {
		   cat("   Using episodes with tag:",Tag,"\n");
      }
	}

   # Select episodes to split signal
   if (Tag=="") {
      ActiveEpisodes=HRVData$Episodes
   } else {
      ActiveEpisodes=subset(HRVData$Episodes,HRVData$Episodes$Type==Tag)
   }

   if (HRVData$Verbose) {
      cat("   Number of episodes:",length(ActiveEpisodes$InitTime),"\n")
   }

   Beg=ActiveEpisodes$InitTime
   End=ActiveEpisodes$InitTime+ActiveEpisodes$Duration

	npoints = length(HRVData$HR)
	first = head(HRVData$Beat$Time,1)
	last = tail(HRVData$Beat$Time,1)
	x=seq(first,last,length.out=npoints)

   # Auxiliary signal used to mark points inside episodes
   Aux=rep(0,times=npoints)
   for (i in 1:length(Beg)) {
      Aux[x>=Beg[i] & x<=End[i]] = 1
   }

   l=list(InEpisodes=HRVData$HR[Aux==1],OutEpisodes=HRVData$HR[Aux==0])

   if (HRVData$Verbose) {
      cat("   Inside episodes:",length(l$InEpisodes),"points\n")
      cat("   Outside episodes:",length(l$OutEpisodes),"points\n")
   }

   return(l)
}


############################## Window #########################################
#' Time windows of RR intervals
#' @description
#' Extracts a subset of RR intervals between the times start and end. 
#' @details 
#' If the \emph{HRVData} contains episodes or the interpolated RR time series,
#' these will be also extracted into the new HRV structure. On the other hand,
#' all the analysis stored in the original structure will be lost.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param start The start time of the period of interest.
#' @param end The end time of the period of interest.
#' @return A new \emph{HRVData} structure containing the subset of RR intervals
#' within the specified range.  
#' @author Constantino A. Garcia
#' @examples
#' \dontrun{
#' data(HRVProcessedData)
#' # Rename for convenience
#' HRVData = HRVProcessedData
#' PlotNIHR(HRVData)
#' newHRVData = Window(HRVData,2000,4000)
#' PlotNIHR(newHRVData)
#' }
Window = function(HRVData, start, end){
  # Check if some beats have been loaded  
  CheckBeats(HRVData)
  # check proper window definition
  if (start > end){
    stop("'start' cannot be after 'end'")
  } 
  
  # Create empty new HRVData
  newHRVData = CreateHRVData()
  newHRVData$datetime = HRVData$datetime
  newHRVData$Verbose = HRVData$Verbose
  # indx inside the window
  indx = which( (HRVData$Beat$Time >= start) & (HRVData$Beat$Time <= end) )
  if ( length(indx) == 0){
    stop("Window out of range: No values were selected")
  }else{
    newHRVData$Beat = HRVData$Beat[indx,]
  }
  # If HRVData has interpolated data, we also interpolate in newHRVData
  if( !is.null(HRVData$HR) ){
    interpTime = seq(HRVData$Beat$Time[1], tail(HRVData$Beat$Time, 1), 
                     len=length(HRVData$HR))
    indx = which( (interpTime >= start) & (interpTime <= end) )
    newHRVData$Freq_HR = HRVData$Freq_HR
    newHRVData$HR = HRVData$HR[indx]
  }
  # Finally include episodes
  WindowEpisodes(newHRVData,HRVData,start,end)

}

# Private function: copies proper episodes from HRVData to newHRVData
WindowEpisodes = function(newHRVData,HRVData,start,end){
  if (!is.null(HRVData$Episodes)){
    episodes = SelectWindowEpisodes(HRVData,start,end)
    if (nrow(episodes) > 0){
      newHRVData$Episodes = episodes
    }
  }
  newHRVData
}

# Private function: selects proper episodes from HRVData on the basis of start
# and end 
SelectWindowEpisodes = function(HRVData,start,end){
  # This should not be necessary
  if (is.null(HRVData$Episodes)){ return(NULL) }
  # The episodes to drop are those that begin after the time 'end'
  # and those that end before time 'start'.
  endTimes = HRVData$Episodes$InitTime + HRVData$Episodes$Duration
  indx = which( (HRVData$Episodes$InitTime) > end | (endTimes < start))
  if (length(indx) > 0){
    # eliminate them
    HRVData$Episodes[-indx,]
  }else{
    HRVData$Episodes
  }
}
