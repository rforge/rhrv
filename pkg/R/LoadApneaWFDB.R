LoadApneaWFDB <-
  function(HRVData, RecordName, RecordPath=".", Tag="APNEA", verbose=NULL) {
    #--------------------------------------- 
    # Loads apnea episodes from an wfdb file
    #---------------------------------------
    #	RecordName -> record containing beat positions
    #	RecordPath -> path
    #  Tag -> tag to include in episodes
    
    if (!is.null(verbose)) {
      cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
      SetVerbose(HRVData,verbose)
    }
    
    if (HRVData$Verbose) {
      cat("** Loading apnea episodes for record:",RecordName,"**\n")
    }
    
    dir=getwd()
    on.exit(setwd(dir))
    
    if (HRVData$Verbose) {
      cat("   Path:",RecordPath,"\n")
    }
    setwd(RecordPath)
    
    # Reads header, verbose=FALSE
    if (is.null(HRVData$datetime)) {
      if (HRVData$Verbose) {
        cat("   Reading header info for:",RecordName,"\n")
      }
      HRVData = LoadHeaderWFDB(HRVData,RecordName,RecordPath)
    } else {
      if (HRVData$Verbose) {
        cat("   Header info already present for:",RecordName,"\n")
      }
    }
    
    auxHeader = readLines(paste(RecordName,".hea",sep=""),1)
    splitAuxHeader = strsplit(auxHeader," ")
    
    if(length(splitAuxHeader[[1]])>2)
      samplingFrequency = splitAuxHeader[[1]][3]
    else
      samplingFrequency = "250"
    
    samplingFrequency = as.numeric(samplingFrequency)
    
    if (HRVData$Verbose) {
      cat("   Sampling frequency for apnea annotations:",samplingFrequency,"\n")
    }
    
    inApnea = FALSE
    accumulator = 0
    initT = c()
    endT = c()
    con = file(paste(RecordName,".apn",sep=""),"rb")
    repeat {
      value = readBin(con,"integer",n=1,size=1,signed=FALSE)+256*readBin(con,"integer",n=1,size=1,signed=FALSE)

      #cat("value:",value,"\n")

      code = bitwShiftR(value,10)
      #cat("code:",code,"\n")

      time = value %% 1024

      #cat("time:",time,"\n")

      if(code==0 && time==0)
        break

      if (code==8 && !inApnea) {
        #cat("Onset: ", accumulator, "\n")
        inApnea = TRUE
        initT = c(initT,accumulator-30)
      }

      if (code==1 && inApnea) {
        #cat("End: ",accumulator, "\n")
        inApnea = FALSE
        endT = c(endT,accumulator-30)
      }

      if (code==59) {
        interval = (readBin(con,"integer",n=1,size=1,signed=FALSE)+readBin(con,"integer",n=1,size=1,signed=FALSE)*256)*65536+(readBin(con,"integer",n=1,size=1,signed=FALSE)+readBin(con,"integer",n=1,size=1,signed=FALSE)*256)
        accumulator = accumulator + interval/samplingFrequency
        next
      }

    }

    if (inApnea) {
      endT = c(endT,accumulator)
      #cat("End: ",accumulator, "\n")
    }

    close(con)
    
    HRVData = AddEpisodes(
      HRVData,
      InitTimes = initT,
      Tags = Tag,
      Durations = endT-initT,
      Values = 0
    ) 
    
    return(HRVData)
  }

