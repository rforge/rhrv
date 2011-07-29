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

   if (.Platform$OS.type=="windows" && Matplotlib==TRUE) {
      cat("   --- Warning: Matplotlib mode is experimental in Windows platforms\n")
   }

   if (.Platform$OS.type=="unix" && Matplotlib==TRUE) {
      cat("   Matplotlib mode activated\n")
   }

   if (Matplotlib==FALSE) {
      cat("   Matplotlib mode disabled\n")
   }

   HRVData$Matplotlib=Matplotlib

   return(HRVData)
}

