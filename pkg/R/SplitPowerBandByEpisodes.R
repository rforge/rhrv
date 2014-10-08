SplitPowerBandByEpisodes <-
function(HRVData, indexFreqAnalysis = length(HRVData$FreqAnalysis), Tag="", verbose=NULL) {
# ------------------------------------------------
# Splits Power Per Band using Episodes information
# ------------------------------------------------
#  Tag -> specifies tag of episodes
#  Returns a list with two lists: InEpisodes and OutEpisodes
#    Both lists include ULF, VLF, LF and HF bands

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
   if (HRVData$Verbose) {
		cat("** Splitting power bands using episodes**\n")
	}

   if (is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present ---\n    --- Quitting now!! ---\n")
   }

	if (is.null(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF)) {
      stop("  --- Power per band not present ---\n    --- Quitting now!! ---\n")
	}

	if (HRVData$Verbose) {
      if (Tag=="") {
		   cat("   No tag was specified\n");
      } else {
		   cat("   Using episodes with tag:",Tag,"\n");
      }
	}

   # Select episodes to split bands
   if (Tag=="") {
      ActiveEpisodes=HRVData$Episodes
   } else {
      ActiveEpisodes=subset(HRVData$Episodes,HRVData$Episodes$Type==Tag)
   }

   if (HRVData$Verbose) {
      cat("   Number of episodes:",length(ActiveEpisodes$InitTime),"\n")
   }



   lframes=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)
   # lframes is the number of frames

   # cat("No. of frames: ", lframes, "\n")
   # cat("Beginning of file: ",head(HRVData$Beat$Time,1),"\n")
   # cat("End of file: ",tail(HRVData$Beat$Time,1),"\n")

   index=c()

   for (i in 1:lframes) {
      BegOfFrame = head(HRVData$Beat$Time,1)+HRVData$FreqAnalysis[[indexFreqAnalysis]]$shift*(i-1)
      EndOfFrame = BegOfFrame + HRVData$FreqAnalysis[[indexFreqAnalysis]]$size
      # if (i<10 || i>lframes-10) {
      #    cat("Frame: ",i,"  -  ")
      #    cat("CenterOfFrame: ",CenterOfFrame,"\n")
      # }
      inEp = FALSE
      if (length(ActiveEpisodes$InitTime)>0) {
         for (j in 1:length(ActiveEpisodes$InitTime)) {
            begEp = ActiveEpisodes$InitTime[j]
            endEp = ActiveEpisodes$InitTime[j]+ActiveEpisodes$Duration[j]
            if (BegOfFrame>=begEp && EndOfFrame<=endEp) {
               inEp = TRUE
            }
         }
      }

      if (inEp) {
         # cat("Frame in episode: ",i,"  -  ")
         # cat("Beg of frame: ",BegOfFrame,"  -  ")
         # cat("End of frame: ",EndOfFrame,"\n")
         index=c(index,i)
      }

   }

   if (length(index)==0){
      cat("   --- Warning: no frames in episodes with tag '",Tag,"'!! ---\n",sep="")
   }

   l=list()


   l$InEpisodes=list(ULF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[index],
      VLF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[index],
      LF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[index],
      HF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[index]
   )

   if (length(index)==0){
      l$OutEpisodes=list(ULF=c(HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF),
         c(VLF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF),
         c(LF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF),
         c(HF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF)
      )
   } else {
      l$OutEpisodes=list(ULF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[-index],
         VLF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[-index],
         LF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[-index],
         HF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[-index]
      )   
   }


   if (HRVData$Verbose) {
      cat("   No. of frames:",lframes,"\n")
      cat("   No. of frames in episodes:",length(l$InEpisodes$ULF),"\n")
      cat("   No. of frames outside episodes:",length(l$OutEpisodes$ULF),"\n")
   }

   return(l)

}

