# pre-processing

loffset <<- 0.00 # for logs
diff1 <<- 0.00 # for difference
m1 <<- 0.00 # for standardize
sd1 <<- 0.00 # for standardize
BN_OBJ <<- NA # for best normalize

# Pre-process in1pp in in1pp out
preproc <- function(PPROC=NA, input=NA) {
  if (PPROC == "none") return(input)
  
  for (pproc in rev(as.list(strsplit(PPROC, "")[[1]]))) { # Note reverse
    
    if (pproc == "L") {
      #print("Found L") # do logs
      loffset <<- 0.00
      if (min(input) <= 0) { # if zeror or negative
        loffset <<- abs(min(input)) + 1  
      }
      input <- log(input + loffset)
    }
    
    if (pproc == "D") {
      #print("Found D") # do difference
      diff1 <<- input[1]
      input <- diff(input, lag = 1, differences = 1)
    }  
    if (pproc == "S") {
      #print("Found S") # do standardize
      m1 <<- mean(input)
      sd1 <<- std(input)
      input <- (input - m1) / sd1
    }  
    if (pproc == "B") {
      #print("Found B") # do best norm
      BN_OBJ <<- bestNormalize(input, allow_lambert_s = TRUE,
                              standardize = TRUE, loo = TRUE)
      input <- predict(BN_OBJ)
    }  
  }
  return(input)
}  

# TODO - reverse the order of pproc
undo_preproc <- function(PPROC=NA, input=NA) {
  if (PPROC == "none") return(input)
  
  for (pproc in as.list(strsplit(PPROC, "")[[1]])) {          
    
    if (pproc == "L") {
      input <- exp(input) - loffset          
    }
    
    if (pproc == "D") {
      #print("Found D") # do difference
      input <- diffinv(input, lag = 1, differences = 1, xi = diff1)
    }  
    if (pproc == "S") {
      #print("Found S") # do standardize
      input <- input * sd1 + m1
    }  
    if (pproc == "B") {
      #print("Found B") # do best norm
      input <- predict(BN_OBJ, newdata = input, warn = FALSE, inverse = TRUE)    
    }
  }
  return(input)
}
############### RND #########################