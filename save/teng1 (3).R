# Trade tune and predict Engine

# Clear R studio workspace, plots, and console
if(!is.null(dev.list())) dev.off()
cat("\014") # Clear console
rm(list=ls()) # Clean workspace

#############################################################
VNAME <- "teng1"
VERSION <- "V0.85"
VDATE <- "02/01/2023"
VAUTHOR <- "Larry Calvert"
#############################################################
# 12/19/2022 changed method to char
# 12/21/2022 just before separating out bar predict as function -> detail tibble
#############################################################

DEBUG <- FALSE
DATADIR <- "c:\\futures\\working" #msdata file directory

# set our directory 
setwd("C:\\Users\\Lcalv\\Documents\\R\\teng1") #NOTE! Hard-coded for now!
#SRCDIR <- "test1" # below /use/documnet/r/
BASEDIR <- normalizePath(getwd())
LOGFILE <- paste0(BASEDIR, "\\teng1.log")

##############################################################
library ("conflicted")
library ("pracma") # for interp1
library ("log4r")
library ("ggplot2")
library ("tidyverse")
#library ("tibble") now in tidyverse
library ("tidyquant") # used for msdata dates - no longer needed
#using("itsmr") # test37 only want fft filter overides forecast
library ("forecast")
library ("forecastHybrid") # needed for Hybrid predict
library ("beepr")
library ("marima") # test multi arima
library("plotly")

source("utility.R") # Larry's utilities
source("an_func7.R") # arma/tsnn prediction needed for pretty mcuh everything
source("Multiplot.R") # for Multiple plots per page
source("readdata.R") # Read metastock data
source("tables.R")
source("preproc.R")
source("predbars.R")
##################################################################

use_cores <- nbr_cores <- 1
PARALLEL <- FALSE
library("parallel") # for nbr_cores
nbr_cores <-detectCores()
# overhead on parallel is actually slowing things down
# OFF for now - Now seems to really help on hybridcv
if (nbr_cores >= 3) {
  use_cores <- floor( 0.75 * nbr_cores)
  if (use_cores >= 2) PARALLEL <- TRUE
}

######### logging ###########################
logger <- create.logger()
logfile(logger) <- LOGFILE
level(logger) <- "INFO" # (one of "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
info(logger, paste(VNAME, VERSION, VDATE)) # (one of debug, info, warn, error, fatal)
########################################

#sessionInfo()

#TODO LRC get date format right
# check date format
#msdataint <- load_data(symbol="1WZ2",intl=INTL,dec=2)
#view(head(msdataint$date,n=10))
#view(head(msdataint$datet,n=10))

#################################################
####### Catchup alignment test ##################
#################################################

# get data madataint
i = 1
symbol <- SYMBOL_LIST[[i,1]]

cline <- paste0("C:\\Users\\Lcalv\\Documents\\R\\teng1\\readms ", DATADIR, " ", symbol, " OHLC")
retval <- system(cline, minimize=TRUE, wait=TRUE)

msdataint <- load_data(symbol=symbol,intl=INTL,dec=2) 
view(tail(msdataint$date,3*ppb))
print(paste0("######### ", symbol, " ##########"))
lastddatew <- tail(msdataint$date,1)
lastddate <- substr(lastddatew,3,10)

# get DETAIL
DETAIL <- read_csv("detail.csv",col_names = TRUE)

# EXtrcat from DETAIL to scr
symbol1 = "1WH3"
group1 = "actual"
dt1 = "h"
pproc = "none"
method = "zlma"
scr <- DETAIL %>% dplyr::filter(symbol==symbol1, group==group1,
                                    dt==dt1,pproc==pproc,method==method) %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))
lastsdatew <- tail(scr$date,1)
lastsdate <- substr(lastsdatew,3,10)
view(tail(scr$date,ppb))

print(paste0("data date=",lastddate," scr date=",lastsdate))

# count bars back in data to last bar in DETAIL extract (scr)
bubars = 0
if (lastsdate == lastddate) {
  print("All caught up")
} else {
  for(i in seq(from = 1, to = 5 * ppb, by = ppb)) {
    budatew <- msdataint$date[[nrow(msdataint)-i]]
    budate <- substr(budatew,3,10)
    #print(budate) 
    if (budate == lastsdate) {
      bubars = i
      break
    }
  }
}
if (bubars > 0) {
  print(paste0("Need to catchup bar(s)=",bubars))
  # last scr bar fill in target and error
  # do predicts
  # add rows scr to detailw
  # when all symbols methods done DETAOL <- detailw
  # write detail.csv
}
########## END catchup date test #############################

#################################################
## Main Logic
#################################################

# Validate that we have data for each in SYNBOL_LIST
# creates datafiles symbol.txt - uses msread.exe
for(i in 1:nrow(SYMBOL_LIST)) {
  symbol <- SYMBOL_LIST[[i,1]]
  cline <- paste0("C:\\Users\\Lcalv\\Documents\\R\\teng1\\readms ", DATADIR, " ", symbol, " OHLC")
  #print(cline)
  retval <- system(cline, minimize=TRUE, wait=TRUE)
  #message(cline,": ", retval)
}


dec <- 2 # tmp move to symbol_list


# if we have detail file just read it - skip main loop
if (file.exists("detail.csv") == TRUE) {
  DETAIL <- read_csv("detail.csv",col_names = TRUE)
  ############################# Do catchup here !!!!
  for(i in 1:nrow(SYMBOL_LIST)) {
    symbol <- SYMBOL_LIST[[i,1]]
    lastdate <- tail(DETAIL$date,1)
    msdataint <- load_data(symbol=symbol,intl=INTL,dec=2) 
    date  <- request_data_l(msdataint=msdataint,type="date",bupts=0,1)
    if (file.exists("predtab.csv") == TRUE) {
      PREDTAB <- read_csv("predtab.csv",col_names = TRUE)
    }
    print(paste0("Catchup:",symbol," ", date(date), " ",date(lastdate)) )
  }  
} else {
  DETAIL <- pred_bars(
            symbol_list=SYMBOL_LIST,
            datatype_list=DATATYPE_LIST,
			      pproc_list=PPROC_LIST,
            method_list=METHOD_LIST,
            intl=INTL,
            nbr_bars=2 * TUNEBARS
            )
}

# regroup DETAIL here  date, symbol dtgroup dt
write_csv(DETAIL,"detail.csv",append = FALSE,col_names = TRUE) # take out col-names for append


#------- End Detail ---------------------------------------------

# score each group LRC
# DETAIL2 summarizes by pproc method
# DETAIL3 summarizes by group

DETAIL2 <- tibble(
  tunedate = Date(),
  symbol   = character(),
  group    = character(),
  dt       = character(),
  pproc    = character(),
  method   = character(),
  score    = double()  
)

#symrow <- 1
for(symrow in 1:nrow(SYMBOL_LIST)) { # symbol loop
  symbol1 <- SYMBOL_LIST$symbol[[symrow]]
  #dtrow <- 1
  for (dtrow in 1:nrow(DATATYPE_LIST)) { # datatype loop
    dt <- as.list(DATATYPE_LIST$dt[[dtrow]])
    group1 <- DATATYPE_LIST$group[[dtrow]]
    # hlcm in group loop
    #col1 <- 1
    for (col1 in 1:length(dt)) {  
      dt1 <- as.character(dt[col1])
      
      #############
      #TODO add pproc here also want score by pproc and method
      ###################
      for(pprocrow in 1:nrow(PPROC_LIST)) {        
        pproc <- PPROC_LIST[[pprocrow,"PPROC"]]
      #methrow <- 1
      for(methrow in 1:nrow(METHOD_LIST)) { 
        method <- METHOD_LIST[[methrow,"METHOD"]]
      
        scr <- DETAIL %>% dplyr::filter(symbol==symbol1, group==group1,
          dt==dt1,pproc==pproc,method==method) %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))
        
        scr2 <- scr %>% dplyr::summarise(mean(sderr,na.rm=T))
        score <- round(scr2[[1]],2*dec)
        #lastdate <- as.character(tail(scr$date,1))
        lastdate <- tail(scr$date,1)
        
        #print(paste0(lastdate, " ",symbol1," ", group1," ",dt1," ",method," ",score))
  
        DETAIL2 <- DETAIL2 %>% 
          add_row(
            tunedate = lastdate,
            symbol = symbol1,
            group = group1,
            dt = dt1,
            pproc = pproc,
            method = method,
            score = score                
          )
        } # method loop
      } # pproc loop
    } # datatype loop
  } # group loop
} #symbol loop

#view(DETAIL2)
write_csv(DETAIL2,"detail2.csv",append = FALSE,col_names = TRUE) # take out col-names for append

# accum score by group and method
DETAIL3 <- tibble(
  tunedate = Date(),
  symbol   = character(),
  group    = character(),
  dt       = character(),
  pproc    = character(),
  method   = character(),
  score    = double()  
)

# select best score (lowest) by datatype within group

# TODO LRC here add pproc

#symrow <- 1
for(symrow in 1:nrow(SYMBOL_LIST)) { # symbol loop
  symbol1 <- SYMBOL_LIST$symbol[[symrow]]
  #dtrow <- 1
  for (dtrow in 1:nrow(DATATYPE_LIST)) { # datatype loop
    dt <- as.list(DATATYPE_LIST$dt[[dtrow]])
    group1 <- DATATYPE_LIST$group[[dtrow]]
    # hlcm in group loop
    #col1 <- 1
    for (col1 in 1:length(dt)) {  
      dt1 <- as.character(dt[col1])
      scr <- DETAIL2 %>% dplyr::filter(symbol==symbol1,group==group1,
                      dt==dt1) %>% dplyr::arrange(score)
      scr2 <- head(scr,1) # want lowest score 

      DETAIL3 <- DETAIL3 %>% 
        add_row(
          tunedate = lastdate,
          symbol   = symbol1,
          group    = group1,
          dt       = dt1,
          pproc    = scr2$pproc,
          method   = scr2$method,
          score    = scr2$score              
        )
    } # datatype loop    
  } # group loop
} #symbol loop

#view(DETAIL3)
write_csv(DETAIL3,"detail3.csv",append = FALSE,col_names = TRUE) # take out col-names for append

# Now find best group (lowest score) 
# accum score by group pproc and method
DETAIL4 <- tibble(
  tunedate = Date(),
  symbol   = character(),
  group    = character(),
  score    = double()  
)

# select best score (lowest) by datatype within group

#symrow <- 1
for(symrow in 1:nrow(SYMBOL_LIST)) { # symbol loop
  symbol1 <- SYMBOL_LIST$symbol[[symrow]]
  #dtrow <- 1
  for (dtrow in 1:nrow(DATATYPE_LIST)) { # datatype loop
    dt <- as.list(DATATYPE_LIST$dt[[dtrow]])
    group1 <- DATATYPE_LIST$group[[dtrow]]
    # hlcm in group loop
    accum <- 0
    cnt <- 0
    #col1 <- 1
    for (col1 in 1:length(dt)) {  
      dt1 <- as.character(dt[col1])
      
      scr <- DETAIL3 %>% dplyr::filter(symbol==symbol1,group==group1,dt==dt1)
      accum <- accum + scr$score
      cnt <- cnt + 1
    } # datatype loop 
      
    DETAIL4 <- DETAIL4 %>% 
      add_row(
        tunedate = lastdate,
        symbol = symbol1,
        group = group1,
        score = round(accum / cnt, 2*dec)              
      )
  } # group loop
} #symbol loop

#view(DETAIL4)
write_csv(DETAIL4,"detail4.csv",append = FALSE,col_names = TRUE) # take out col-names for append

# Now from this pick best (lowest score) group per symbol
BESTSUM <- tibble(
  tunedate = Date(),
  symbol   = character(),
  group    = character(),
  score    = double()  
)

# select best score (lowest) by datatype within group
#symrow <- 1
for(symrow in 1:nrow(SYMBOL_LIST)) { # symbol loop
  symbol1 <- SYMBOL_LIST$symbol[[symrow]]

  scr <- DETAIL4 %>% dplyr::filter(symbol==symbol1) %>% dplyr::arrange(score)
  scr2 <- head(scr,1)
  
  BESTSUM <- BESTSUM %>% 
      add_row(
        tunedate = lastdate,
        symbol = symbol1,
        group = scr2$group,
        score = scr2$score              
      )
} #symbol loop

#view(BESTSUM)
write_csv(BESTSUM,"bestsum.csv",append = FALSE,col_names = TRUE) # take out col-names for append

PREDTAB <- tibble(
  tunedate = Date(),
  symbol   = character(),
  group    = character(),
  dt       = character(),
  pproc    = character(),
  method   = character(),
  score    = double()  
)

# Now using BESTSUM get group dt and best method
#symrow <- 1
for(symrow in 1:nrow(SYMBOL_LIST)) { # symbol loop
  symbol1 <- SYMBOL_LIST$symbol[[symrow]]
  bestsum <- BESTSUM %>% dplyr::filter(symbol==symbol1)
  group1 <- bestsum$group
  scr <- DETAIL3 %>% dplyr::filter(symbol==symbol1,group==group1)
  
  for (dtrow in 1:nrow(scr)) { # datatype loop
    PREDTAB <- PREDTAB %>% 
      add_row(
        tunedate = lastdate,
        symbol = symbol1,
        group = group1,
        dt = scr$dt[[dtrow]],
        pproc = scr$pproc[[dtrow]],
        method = scr$method[[dtrow]],
        score = scr$score[[dtrow]]
      )
    #print(paste0(symbol1," ",group1," ",dt," ",method," ",score))
  } 
}

#view(PREDTAB)
write_csv(PREDTAB,"predtab.csv",append = FALSE,col_names = TRUE) # take out col-names for append

##########################################################
# do err predicts
if (file.exists("predtab.csv") == TRUE) {
  PREDTAB <- read_csv("predtab.csv",col_names = TRUE)
}

symrow <- 1
for(symrow in 1:nrow(PREDTAB)) { 
  symbol1 <- PREDTAB$symbol[[symrow]]
  # TODO Need datatype loop
  dt1 <- PREDTAB$dt[[symrow]]
  #dt1 <- "m"
  pproc1 = PREDTAB$pproc[[symrow]]
  method1 = PREDTAB$method[[symrow]]
  # get prior predicts and err
  #scr <- DETAIL %>% dplyr::filter(symbol==symbol1,dt==dt1,
  #         pproc==pproc,method==method) %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))
  scr <- DETAIL %>% dplyr::filter(symbol==symbol1 & dt==dt1 & pproc==pproc1 &
          method==method1) %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))
  
  #view(scr)

  in1 <- scr$error
  in1 <- in1[!is.na(in1)] # remove NAs
  price <- scr$price
  pred1 <- scr$predict
  target <- scr$target
  
  length(in1)
  length(pred1)
  
  # predict hybridcv of err
  usemod <- "aefnt" # hybrid drop the stl model requires Timeseries
  
  mod <- forecastHybrid::hybridModel(in1, models = usemod,
                     weights = "cv.errors",
                     verbose = TRUE,
                     errorMethod = "RMSE",
                     cvHorizon =  ppb,
                     #windowSize = 10 * ppb
                     windowSize = 5 * ppb
                     )
  
  mod
  accuracy(mod)
  
  mod_fit <- as_tibble(fitted(mod,individual = TRUE))
  #print(mod_fit)

  # write csv for debugging
  #in1v <- enframe(in1,name=NULL)
  #modl1_tbl <- dplyr::bind_cols(in1v,mod_fit)
  #write_csv(modl1_tbl,"modl1.csv",append = FALSE,col_names = TRUE)
  
  fc <- forecast::forecast(mod, h=ppb)
  
  print(fc$method)
   #View the point forecasts
  fc$mean
   #View the upper prediction interval
  fc$upper
   #View the lower prediction interval
  fc$lower

  # Plot the forecast
  plot(mod, type = "models")
  plot(mod, type = "fit")    
  plot(fc)

  #prediction is in fitted plus append mean
  predict <- fc$fitted
  length(predict)
  for (i in seq(length(fc$mean))) {
    predict <- append(predict,fc$mean[i])
  }
  length(predict)
  
  # replace any na's at beginning with input values
  for (i in seq(length(predict))) {
    if (!is.na(predict[i])) break;
    predict[i] <- in1[i]
  }

  length(pred1)
  length(predict)
  
 #return(predict)
  pred2 <- pred1 + predict
  error1 <- in1
  for (i in seq(ppb)) {
    error1 <- append(error1,NA)
  }
  error2 <- price - pred2
  
  PREDTMP <- tibble(
    symbol   = character(),
    dt       = character(),
    price    = double(),
    predict  = double(),
    error1    = double(),
    error2    = double(),    
  )
  PREDTMP <- PREDTMP %>% 
    add_row(
      symbol = symbol1,
      dt = dt1,
      price = price,
      predict = pred2,
      error1 = error1,
      error2 = error2
    )
  write_csv(PREDTMP,"predtmp.csv",append = FALSE,col_names = TRUE)
  view(PREDTMP)
  
  if (dt1 == "h")   hpred <- pred2
  if (dt1 == "hro") hpred <- pred2
  if (dt1 == "hr")  hpred <- pred2
 
  if (dt1 == "l")   lpred <- pred2
  if (dt1 == "lro") lpred <- pred2
  if (dt1 == "lr")  lpred <- pred2
  
  if (dt1 == "c")   cpred <- pred2
  if (dt1 == "cro") cpred <- pred2
  if (dt1 == "cr")  cpred <- pred2
  
  if (dt1 == "m")   mpred <- pred2
  if (dt1 == "mro") mpred <- pred2
  if (dt1 == "mr")  mpred <- pred2
}


#plotly test
  #library(plotly)
  #df <- data.frame(Date=index(AAPL),coredata(AAPL))
  #df <- tail(df, 30)
  clen <- TUNEBARS * ppb
  symbol <- "1WH3"
  msdataint <- load_data(symbol=symbol,intl=INTL,dec=2) 
  # TODO advance date (and length) by ppb
  date  <- request_data_l(msdataint=msdataint,type="date",bupts=0,len=clen+ppb)
  open  <- request_data_l(msdataint=msdataint,type="o",bupts=0,len=clen)
  high  <- request_data_l(msdataint=msdataint,type="h",bupts=0,len=clen)
  low   <- request_data_l(msdataint=msdataint,type="l" ,bupts=0,len=clen)
  close <- request_data_l(msdataint=msdataint,type="c",bupts=0,len=clen)
  mid   <- request_data_l(msdataint=msdataint,type="m",bupts=0,len=clen)
  pred <- NULL
  for (i in seq(ppb)) {
    #date  <- append(date,NA)
    open  <- append(open,NA)
    high  <- append(high,NA)
    low   <- append(low,NA)
    close <- append(close,NA)
    mid   <- append(mid,NA)
  }
  
  pred  <- append(pred,tail(PREDTMP$predict,clen+ppb))
  
  # advance date by 1 day skipping weekends using ppb
  # does not account for trading holidays
  
  #############################################################
  #TODO in readdata.R do ymd_hms on date !!!!!!!!!!!!!!!!!!
  #############################################################
  
  # Handle interpolate 
  datet <- ymd_hms(date,tz=Sys.timezone()) # tmp
  view(datet)
  len <- length(datet)
  datesv <- datet[len]
  
  if (ppb > 1) {
    lastdate <- datesv
    cnt <- 1
    for (i in seq(2:ppb-1)) { # do only if ppb > 1
      lastdate <- ymd_hms(lastdate + hours(x=cnt),tz=Sys.timezone())
      print(lastdate)
      datet <- append(datet,lastdate)
      cnt <= cnt + 1
    }
  }
  view(datet)  
  
  test2 <- datesv + days(1) # advance to next day
  print(wday(test2, label=TRUE))
  if (wday(test2, label=TRUE) == "Sat") { # skip Sat & Sun
    test2 <- datesv + days(3)
  }
  datet <- append(datet,test2)
  view(datet)
  
  date <- tail(datet,clen+ppb)
  view(date)
  
  dfa <- tibble(date,open,high,low,close,mid,hpred,lpred,cpred,mpred)

  # Deinterpolate - make function
  de_interpolate <- function(datatib=NA,intl=NA) {
    datatiba <- NULL
    ppb <- intl + 1
    
    if (intl==0) { # No interpolate
     datatiba <- datatib 
    }
    if (intl > 0) { # do deinterpolate
      if (nrow(datatib) %% ppb != 0) { # sanity check
        print(paste0("ERROR: deinterpolate length problem ", nrow(datatib), "ppb=",ppb))
        return(NULL)
      }
      for (i in seq(ppb,nrow(datatib),ppb)) {
        #print(i)
        tmp <- dplyr::slice(datatib,i)
        if (!is.na(tmp$date)){
          print(tmp$date)
          tmp$date <- substr(tmp$date,3,10)
          print(tmp$date)        
        }
        datatiba <- rbind(datatiba,tmp)
      }
    }
   return(datatiba)    
  }


  df <- de_interpolate(datatib=dfa,intl=1)
  
  TITLE <- paste0(symbol, " LB=",tail(df$date,1))
  
  fig <- df %>% plot_ly(x = df$date, type="ohlc",
                        open = df$open, close = df$close,
                        high = df$high, low = df$low)
                      
  
  fig <- fig %>% add_lines(x = df$date, y = df$hpred, line = list(color = 'purple', width = 0.75), inherit = F)
  fig <- fig %>% add_lines(x = df$date, y = df$lpred, line = list(color = 'purple', width = 0.75), inherit = F)
  fig <- fig %>% add_lines(x = df$date, y = df$cpred, line = list(color = 'red',    width = 1.25), inherit = F)
  fig <- fig %>% add_lines(x = df$date, y = df$mpred, line = list(color = 'green',  width = 1.25), inherit = F)
  

  fig <- fig %>% plotly::layout(title = TITLE, showlegend = F, xaxis = list(type = 'category'))
  
  fig
  
############## END 1 #######################################

#for each symbol
  #select best dt group
  # for each dt
    # do last predicts and create
    # date price target err
    # do offset lines
    # create chart
  #}
#}

# print open trades and summary (best next trade)
# print symbol table charts each has buy/sell text

#TODO:
# 1. pre-processing also check logs of ro rpm
# 2. 2nd predict
# 3. DETAIL bring to current without whole redo calc tunebars needed
# 4. offset lines
# 5. summary and charts
# 6. order placements

# Time of each method
#View(DETAIL2)
# view(scr)
# view(scr$etime)
# summarise(scr,mean(etime))

#  scr <- DETAIL %>% dplyr::filter(symbol=="1WH3", group=="ro",
#        method=="zlma") %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))

# scr2 <- scr %>% dplyr::summarise(mean(etime,na.rm=T))
#  score <- round(scr2[[1]],2*dec)  
#  score
 
# catchup - targets
  # will need to sel
  print("Catchup Targets to be updated - current last bars")
  detaila <- DETAIL # work on a copy for testing
  #colnames(detaila)
  val <- 522.00
  for(i in 1:nrow(detaila)) {  
    if (is.na(detaila$target[[i]])) { #those without targets
      # fill in target here
      detaila$target[i] = val # Yay - works !!!!!
      # will need to calc error1 also
     }
  }
  
  # show last 10
  scr5 <- detaila %>% dplyr::slice_tail( n=40)
  #view(scr5)

  ############## END 2 #######################################
  