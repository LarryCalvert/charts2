# pre-processing

# Pre-process in1pp in in1pp out
preproc <- function(PPROC=NA, input=NA) {
  for (pproc in as.list(strsplit(PPROC, "")[[1]])) {
    if (pproc == "L") {
      #print("Found L") # do logs
      input <- sign(input) * log(abs(input + 1)) # transform handles neg and zero
    }
    if (pproc == "D") {
      #print("Found D") # do difference
    }  
    if (pproc == "S") {
      #print("Found S") # do standardize
    }  
    if (pproc == "B") {
      #print("Found B") # do best norm
    }  
  }
  return(input)
}  

undo_preproc <- function(PPROC=NA, input=NA) {
  for (pproc in as.list(strsplit(PPROC, "")[[1]])) {          
    if (pproc == "L") {
      input <- sign(input) * exp(abs(input)) - 1          
    }
    if (pproc == "D") {
      #print("Found D") # do difference
    }  
    if (pproc == "S") {
      #print("Found S") # do standardize
    }  
    if (pproc == "B") {
      #print("Found B") # do best norm
    }
  }
  return(input)
}
############### RND #########################