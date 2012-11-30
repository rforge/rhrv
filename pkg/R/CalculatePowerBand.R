CalculatePowerBand<-
function(HRVData, indexFreqAnalysis=-1, size, shift, sizesp=1024, scale="linear", ULFmin=0, ULFmax=0.03, VLFmin=0.03, VLFmax=0.05, LFmin=0.05, LFmax=0.15, HFmin=0.15, HFmax=0.4, verbose=NULL,type="fourier",wavelet="d4",bandtolerance=0.1,relative=FALSE) {
# -------------------------
# Calculates power per band
# -------------------------
#  indexFreqAnalysis: index of an existing frequency analysis to use
#  size, disp: size and displacement of window (sec.)
#  sizesp: seconds for calculating spectrogram (zero padding)
#  ULF band: from 0 to 0.03Hz
# 	VLF band: from 0.03 to 0.05Hz
# 	LF band: from 0.05 to 0.15Hz
# 	HF band: from 0.15 to 0.4Hz
#   type: type of analysis, "fourier" or "wavelet"
#   wavelet: nama of the wavelet for analysis
#   bandtolerance: bandtolerance in % for the wavelet tree decomposition
  

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Calculating power per band **\n")
	}

	if (indexFreqAnalysis==-1 ) {
      	stop("  --- Frequency analysis not present ---\n    --- Quitting now!! ---\n")
   	}

 if ((length(HRVData$FreqAnalysis) < indexFreqAnalysis) || (indexFreqAnalysis<1) ) {
	  	stop("   --- Frequency analysis no.",indexFreqAnalysis,"not present!! ---\n    --- Quitting now!! ---\n")
 }
 
 if ((type!="fourier") &&( type!="wavelet")){
       stop("   --- Incorrect type of analysis:",type," ---\n    --- Quitting now!! ---\n")
 }
 
if (( type=="wavelet")&& (bandtolerance< 0)){
       stop("   --- Band tolerance:",bandtolerance,"%, must be positive:",type," ---\n    --- Quitting now!! ---\n")
 }
 
 if (( type=="wavelet")&& (max(ULFmin,ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax)>HRVData$Freq_HR)){
       stop("   --- bandtolerance:",max(ULFmin,ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax)," bigger than sampling frequency. ---\n    --- Quitting now!! ---\n")
 }
 
 
  if (type=="fourier"){
          if (HRVData$Verbose) {
              cat("** Using Fourier analysis **\n")
        	}

          shiftsamples = shift*HRVData$Freq_HR
          sizesamples=floor(size*HRVData$Freq_HR)
          signal=HRVData$HR/60.0
          hamming=0.54-0.46*cos(2*pi*(0:(sizesamples-1))/(sizesamples-1))
          hammingfactor=1.586

          # Calculates the number of windows
          nw=1
          begnw=1
          repeat {
              begnw=begnw+shiftsamples
              if ((begnw+sizesamples-1)>=length(signal)) {
                  break
              }
            nw=nw+1
          }
          if (HRVData$Verbose) {
            cat("   Windowing signal... ",nw," windows \n",sep="")
          }

          for (i in 1:nw) {
            beg=1+(shiftsamples*(i-1))
            window = signal[beg:(beg + sizesamples - 1)]
            window = window - mean(window)
            window = window*hamming
            spectrum = spec.pgram(window,detrend=FALSE,plot=FALSE,taper=0)
            freqs=HRVData$Freq_HR*spectrum$freq
            # cat("Window no.:",i,"\n")
            HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV[i]=mean(spectrum$spec)*hammingfactor

            ULFBand = spectrum$spec[freqs>=ULFmin & freqs<ULFmax]
            HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[i]=sum(ULFBand)/length(spectrum$spec)*hammingfactor

            VLFBand = spectrum$spec[freqs>=VLFmin & freqs<VLFmax]
            HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[i]=sum(VLFBand)/length(spectrum$spec)*hammingfactor

            LFBand = spectrum$spec[freqs>=LFmin & freqs<LFmax]
            HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[i]=sum(LFBand)/length(spectrum$spec)*hammingfactor

            HFBand = spectrum$spec[freqs>=HFmin & freqs<=HFmax]
            HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[i]=sum(HFBand)/length(spectrum$spec)*hammingfactor

            # cat("  Power ULFBand: ",HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[i],"Hz^2\n")
            # cat("  Power VLFBand: ",HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[i],"Hz^2\n")
            # cat("  Power LFBand: ",HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[i],"Hz^2\n")
            # cat("  Power HFBand: ",HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[i],"Hz^2\n")
            # cat("  Power outband: ",sum(outband)/length(spectrum$spec),"Hz^2\n")
            # cat("  Power: ",HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV[i],"\n")
            # cat("  Before: ",HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF[i],"\n")
            HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF[i]=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[i]/HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[i]
            # cat("  Now: ",HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF[i],"\n")
          }

          HRVData$FreqAnalysis[[indexFreqAnalysis]]$size=size
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$shift=shift
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$sizesp=sizesp

            


  }
   if (type=="wavelet"){
          if (HRVData$Verbose) {
              cat("** Using Wavelet analysis **\n")
          }
          
          
          
          powers=modwptAnalysis(HRVData$HR,wavelet, ULFmin, ULFmax , VLFmin, VLFmax, LFmin, LFmax, HFmin , HFmax, HRVData$Freq_HR,bandtolerance,relative)
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF=powers$ULF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF=powers$VLF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF=powers$LF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF=powers$HF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV=powers$ULF+powers$VLF+powers$LF+powers$HF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF=powers$LF/powers$HF
          #metadata for wavelet analysis
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$wavelet=wavelet
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$bandtolerance=bandtolerance
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$depth=powers$depth
          # rule of thumb used to determine if wavelet analysis is descending too many levels
          L=length(wave.filter(wavelet)$lpf)
          N=length(HRVData$HR)
          J=log2(N/(L-1)+1)                
          if ((HRVData$Verbose)&&(powers$depth>J)){
              cat("** Warning: Wavelet analysis requires descending too many levels **\n")
        	}
          
   }
 #common metadata for both analysis
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$type=type
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULFmin=ULFmin
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULFmax=ULFmax
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLFmin=VLFmin
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLFmax=VLFmax
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFmin=LFmin
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFmax=LFmax
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$HFmin=HFmin
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$HFmax=HFmax
 
  
	if (HRVData$Verbose) {
		cat("Power per band calculated\n")
	}
	return(HRVData)
}
