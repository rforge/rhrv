CheckAnalysisIndex <- function(index, numberOfIndex, type){
  
  useName = switch(type,
                   frequency = "frequency",
                   nonlinear = "non-linear")
  
  useMethod = switch(type,
                     frequency = "CreateFreqAnalysis()",
                     nonlinear = "CreateNonLinearAnalysis()")
  
  useIndexName = switch(type,
                    frequency = "indexFreqAnalysis",
                    nonlinear = "indexNonLinearAnalysis")
  
  if (numberOfIndex < 1 )  {
    msg = paste(" --- There are no",useName,"analysis structures!!   ---\n   --- Create some using",useMethod,"---\n")
    stop(msg)
  }
  
  if (index < 1)  {
    msg = paste(" --- Invalid",useIndexName,index," ---\n   --- Quitting now!! ---\n")
    stop(msg)
  }
  
  if (numberOfIndex < index)  {
    msg = paste(" ---",useIndexName,"analysis no.",index," not present!! ---\n   --- Quitting now!! ---\n")
    stop(msg)
  }
}

# Check if interpolation has been performed 
CheckInterpolation = function(HRVData){
  CheckNIHR(HRVData)
  if(is.null(HRVData$HR)){
    stop(" --- Frequency analysis needs interpolated data!!   ---\n   --- Use the InterpolateNIHR() function ---\n")
  }  
}


# Check if some Beats have been loaded
CheckBeats = function(HRVData){
  if(is.null(HRVData$Beat)){
    stop(" --- RR intervals have not been loaded!!   ---\n   --- Use the LoadBeat() function ---\n")
  }  
} 


# Check if the BuildNIHR has been called 
CheckNIHR = function(HRVData){
  CheckBeats(HRVData)
  if(is.null(HRVData$Beat$RR)){
    stop(" --- RR intervals have not been calculated!!   ---\n   --- Use the BuildNIHR() function ---\n")
  }  
}


# Check if the periodogram has been computed
CheckPeriodogram =  function(HRVData, indexFreqAnalysis){
  if(is.null(HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram)){
    stop(" --- PSD not found !!   ---\n   --- Use the CalculatePSD() function ---\n")
  }
}
