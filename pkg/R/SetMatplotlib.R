SetMatplotlib <-
function(HRVData,Matplotlib) {
# ------------------------------
# Sets matplotlib mode on or off
# ------------------------------
   if (HRVData$Verbose) {
      if (Matplotlib == TRUE) {
         cat("** Enabling Matplotlib mode... **\n")
      }

      if (Matplotlib == FALSE) {
         cat("** Disabling Matplotlib mode...d **\n")
      }
   }

   if (.Platform$OS.type!="unix" && Matplotlib==TRUE) {
      cat("   --- ERROR: Matplotlib mode not available for this platform\n")
      return(HRVData)
   }

   HRVData$Matplotlib=Matplotlib

   return(HRVData)
}

