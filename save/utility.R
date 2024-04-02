#############################################################
# utility.R
#############################################################

# package loader
# from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
using <- function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    print(libsmsg)
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse="")
    if(winDialog(type = c("yesno"), libsmsg)=="YES"){       
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }
}

# errors of single vector
RMSE <- function(input) { sqrt(mean(input^2,na.rm=T)) }
MAXE <- function(input) { max(abs(input),na.rm=T) }
STDE <- function(input) {
  a <- input - mean(input,na.rm=T)
  b <- a / sd(a)
  return(abs(b))
}

# combined error so we have a sngle number to check
CHKE <- function(input) { (RMSE(input) + MAXE(input)) / 2.0 }

standardize <- function(x=x, mean=mean, sd=sd)
{
  len <- length(x)
  stand <- vector(mode="double",length=len)
  for (i in 1:len) {
    stand[i] <- (x[i] - mean) / sd
  }
  return(stand)
}

undo_standardize <- function(x=x, mean=mean, sd=sd)
{
  len <- length(x)
  stand <- vector(mode="double",length=len)
  for (i in 1:len) {
    stand[i] <- (x[i] * sd) + mean
  }
  return(stand)
}

####################################################
last_cross <- function(x,y)
{
  cnt <- 0
  len <- length(x)
  sx <- sign(x)
  sy <- sign(y)
  if (len == length(y)) {
    for (i in seq(from=len, to=1, by=-1)) {
      if (sx[i] != sy[i]) break
      cnt <- cnt + 1
    }
  }
  return(cnt)
}

last_crosssign <- function(x)
{
  cnt <- 0
  len <- length(x)
  sx <- sign(x)
  for (i in seq(from=len, to=2, by=-1)) {
    if (sx[i] != sx[i-1]) break
    cnt <- cnt + 1
  }
  return(cnt)
}

######## My stop function #############
# allows stopping script in Rstudio - red hit stop button
mystop <- function()
{
  print("at mystop - press red stop button to stop script")
	while(1) {
		Sys.sleep(3)
	}
}

# example for missing y value in function
#filter.fft(y = stop("y-value is missing"), x = NULL, fc = 0,
 # BW = 0, n = 3)
 
#DETAILDT %>% dplyr::filter(!is.na(rmserr)) # remove na rows

################# END ################################