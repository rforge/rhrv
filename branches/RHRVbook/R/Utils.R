
getIndexValuesInRange = function(x, rang){
  which( (rang[[1]] <= x) & (x <= rang[[2]]) )
}


VerboseMessage = function(verbose, msg, symbol="**", tab=""){
  if(verbose){
    message(paste(tab,symbol," ",msg," ",symbol,sep=""))
  }
}



