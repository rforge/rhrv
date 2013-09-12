#' Nonlinear noise reduction
#' @description
#' Function for denoising the RR time series using nonlinear analysis techniques. 
#' @details
#' This function takes the RR time series and denoises it. The denoising
#' is achieved by averaging each Takens' vector in an m-dimensional space
#' with his neighbours (time lag=1). Each neighbourhood is specified with balls of a given radius
#' (max norm is used).
#' @param HRVData Data structure that stores the beats register and information related to it
#' @param embeddingDim Integer denoting the dimension in which we shall embed the RR time series.
#' @param radius The radius used to looking for neighbours in the phase space (see details). If the radius
#' is not specified, a radius depending on the standard deviation of the RR time series is used.
#' @param ECGsamplingFreq The sampling frequency of the ECG from which the RR time series was derived. Although it
#' is not necessary, if it is provided it may improve the noise reduction.
#' @return A HRVData structure containing the denoised RR time series.
#' @references H. Kantz  and T. Schreiber: Nonlinear Time series Analysis (Cambridge university press)
#' @author Constantino A. Garcia
#' @rdname nonLinearNoiseReduction
#' @note This function is based on the \code{\link[nonlinearTseries]{nonLinearNoiseReduction}} function from the 
#' nonlinearTseries package.
#' @seealso \code{\link[nonlinearTseries]{nonLinearNoiseReduction}}
NonLinearNoiseReduction <- function(HRVData, embeddingDim = NULL, radius = NULL , ECGsamplingFreq = NULL ) {
  # -------------------------------------
  # Uses nonlinear noise reduction
  # -------------------------------------
  kSdDenoising = 1.05
  
  if (HRVData$Verbose) {
    cat("** Denoising RR time series using nonlinear techniques **\n")
  }
  if (is.null(HRVData$Beat$RR)) {
    stop("RR time series not present!! Quitting now!\n")
  }
  
  estimations = automaticEstimation(HRVData,timeLag=1,embeddingDim,theilerWindow=0)
  embeddingDim = estimations[[2]]
  
  
  if (!is.null(ECGsamplingFreq)){
    Ts = 1/ECGsamplingFreq
    noise = runif(n=length(HRVData$Beat$RR),min=-(Ts/2),max=(Ts/2))
    HRVData$Beat$RR = HRVData$Beat$RR + noise
  }
  if (is.null(radius)){
    radius = kSdDenoising * sd(HRVData$Beat$RR)
  }
  
  RRseries = nonLinearNoiseReduction(time.series=HRVData$Beat$RR,embedding.dim=embeddingDim,
                                      radius = radius)
  time = diffinv(tail(RRseries,-1))/1000
  time = time + HRVData$Beat$Time[[1]]
  HRVData$Beat$Time = time
  
  HRVData = BuildNIHR(HRVData)
  
  return(HRVData)
}
