# Trade tune and predict Engine

# Clear R studio workspace, plots, and console
if(!is.null(dev.list())) dev.off()
cat("\014") # Clear console
rm(list=ls()) # Clean work space

#############################################################
VNAME <- "teng1"
VERSION <- "V0.92"
VDATE <- "05/10/2023"
VAUTHOR <- "Larry Calvert"
#############################################################
# 04/30/2023 Testing combined L1 predict fulltune and catchup function
# 04/09/2023 Starting combined predict and catchup function
# 03/26/2023 adding L1_pred and L1_score
# 12/19/2022 changed method to char
# 12/21/2022 just before separating out bar predict as function -> detail tibble
#############################################################
GITPUSH <- FALSE
DEBUG <- FALSE
LOGLEVEL <- "DEBUG" # set level - one of "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
DATADIR <- "c:\\futures\\working" #msdata file directory

# set our directory 
BASEDIR <- normalizePath(getwd())
LOGFILE <- paste0(BASEDIR, "\\teng.log")

##############################################################
library ("conflicted")
library ("pracma") # for interp1
library ("log4r")
library ("dplyr")
#library ("tidyr") # only used for drop_na in filters
library ("readr")
library ("lubridate")
library ("tibble")
library ("purrr") # required for drop_cols function

#library("itsmr") # test37 only want fft filter overides forecast

library ("forecast")
library ("forecastHybrid") # needed for Hybrid predict
library ("beepr")
library ("marima") # test multi arima
library("plotly")
library("htmlwidgets")
library("TTR") # Technical Trading Rules zigzag ind

# TODO future:  forecastml and others
#library("fable")
#library("tidymodels")
#library("modeltime")
#library("timetk")

source("utility.R") # Larry's utilities
source("an_func7.R") # arma/tsnn prediction needed for pretty much everything
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
if (file.exists(LOGFILE) == TRUE) file.remove(LOGFILE) # delete LOGFILE
# set level - one of "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
logger <- create.logger(logfile = LOGFILE, level = LOGLEVEL)

##################### L1_main #########################################
logtitle <- "L1_main"
L1_START_TIME <- Sys.time()
log4r::info(logger, paste("******************************************************"))
log4r::info(logger, paste("******************************************************"))
log4r::info(logger, paste(logtitle,"Start",VNAME, VERSION, VDATE, L1_START_TIME))
########################################

writeLines(capture.output(sessionInfo()), "sessionInfo.log")

#TODO LRC get date format right
# check date format
#msdataint <- load_data(symbol="1WZ2",intl=INTL,dec=2)
#view(head(msdataint$date,n=10))
#view(head(msdataint$datet,n=10))

#################################################
####### L1_predict_main        ##################
#################################################
L1_DETAIL_FILE <- "detail.csv"
DATA_CHANGED <- FALSE

# get DETAIL
# if we have detail file read it check and catchup if need be
DETAIL <- DETAIL_LINES
if (file.exists(L1_DETAIL_FILE) == TRUE) {
  DETAIL <- read_csv(L1_DETAIL_FILE,col_names = TRUE)
  log4r::info(logger, paste(logtitle, L1_DETAIL_FILE, " read rows=",nrow(DETAIL))) 
} else {
  log4r::info(logger, paste(logtitle, L1_DETAIL_FILE, " not found")) 
}

#DETAIL <- L1_predict(DETAIL=DETAIL)

# function L1_predict(DETAIL=DETAIL)
logtitle <- "L1_predict"
detailw <- DETAIL_LINES # start with empty work file
  
# loop through symbols
symbol_list <- SYMBOL_LIST 
for(symrow in 1:nrow(symbol_list )) { # symbol loop
  symbol1 <- symbol_list$symbol[[symrow]]
  dec <- symbol_list$decimals[[symrow]]
  print(paste0("######### ", symbol1, " ##########"))

  ############## READ data ###########################################
  cline <- paste0(BASEDIR,"\\readms ", DATADIR, " ", symbol1, " OHLC")
  retval <- system(cline, minimize=TRUE, wait=TRUE)
  msdataint <- load_data(symbol=symbol1,intl=INTL,dec=dec) 
  ###############################################################
  lastddatew <- tail(msdataint$date,1) # last data date
  lastddate <- substr(lastddatew,3,10)

  # do group,dt,pproc,method loops here
  datatype_list <- DATATYPE_LIST 
  for (dtrow in 1:nrow(datatype_list)) { # datatype loop
    dt <- as.list(datatype_list$dt[[dtrow]])
    group1 <- datatype_list$group[[dtrow]]
    # hlcm in group loop
    for (col in 1:length(dt)) {  
      dt1 <- as.character(dt[col])
      
      # Pre Process loop
      pproc_list <- PPROC_LIST
      for(pprocrow in 1:nrow(pproc_list)) {        
        pproc1 <- pproc_list[[pprocrow,"PPROC"]]
       
        # Method loop
        method_list <- METHOD_LIST
        for(methrow in 1:nrow(method_list)) {          
          method1 <- method_list[[methrow,"METHOD"]]
    
          bubars <- 0
          # select this data from DETAIL file
          scr <- DETAIL %>% dplyr::filter(symbol==symbol1, group==group1,
                      dt==dt1,pproc==pproc1,method==method1) 
          nbr_rows <- nrow(scr)
            
          log4r::debug(logger, paste("******************************************"))
          log4r::debug(logger, paste( logtitle,symbol1,group1,dt1,pproc1,method1))
          log4r::debug(logger, paste( logtitle,"Want data rows=",(TOTALBARS +1) * ppb,"Found=",nbr_rows)) 
            
          if (nbr_rows < (TOTALBARS +1) * ppb) { #new tune
            bubars <- TOTALBARS + 1 # need current bar predict also
            scr <- DETAIL_LINES # reset scr
            log4r::info(logger, paste( logtitle,"not enough DETAIL - setting full tune - bubars=",bubars))
              
          } else {  
            lastsdatew <- tail(scr$date,1)
            lastsdate <- substr(lastsdatew,3,10)
            #view(tail(scr$date,ppb))
              
            #print(paste(" scr date=",lastsdate,"data date=",lastddate))
            log4r::debug(logger,paste( logtitle,"scr date=",lastsdate,"data date=",lastddate,"ppb=",ppb))
              
            # count bars back in data to last bar in DETAIL extract (scr)
            if (lastsdate != lastddate) {
               for(i in seq(from =1, to = nbr_rows, by = ppb)) {
                budatew <- msdataint$date[[nrow(msdataint)-i]]
                budate <- substr(budatew,3,10)
                #print(budate) 
                if (budate == lastsdate) {
                  bubars = i
                  break
                }
              }
            }
          }
          if (bubars == 0) {
            print("All caught up")
            log4r::info(logger,paste( logtitle,"All caught up")) 
          } else { # additional data has been added
            DATA_CHANGED <- TRUE
            #print(paste0("Need to predict bar(s)=",bubars))
            log4r::info(logger,paste( logtitle, "Need to predict bar(s)=",bubars))
              
            # last scr bar fill in target and error
            #view(tail(scr,3*ppb))
            #view(tail(msdataint,bubars*ppb))
              
            #fill in scr last bar's target 
            bupts <- (bubars-1)*ppb  
            #bupts <- bubars*ppb 
            in1date <- request_data_l(msdataint=msdataint,type="date",bupts=bupts,len=datalen)
            in1 <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts,len=datalen)
            #view(tail(in1date,2*ppb))
            #view(tail(in1,2*ppb))
              
            # fill in scr target
            for(i in seq(from = ppb-1, to = 0, by = -1)) {
              log4r::debug(logger,paste( logtitle,"scr date=",scr$date[nrow(scr)-i],scr$target[nrow(scr)-i]))
              log4r::debug(logger,paste( logtitle,"msdata date=",in1date[datalen-bupts-i],in1[datalen-bupts-i]))
              
              scr$target[nrow(scr)-i] = in1[datalen-bupts-i] # target
              log4r::debug(logger,paste( logtitle, "Target=",in1[datalen-bupts-i], "DataDate=",in1date[datalen-bupts-i]))
              
              ##########################################################
            }
            #view(tail(scr,3*ppb))
            bubars <- bubars - 1
   
           ######################################################           
            # do predicts
            #  scr <- L1_predict_bars(
            #  detailw=scr,
            #  symbol=symbol1,
            #  dt=dt1,
            #  pprocrow=pprocrow,
            #  methodrow=methodrow,
            #  bubars=bubars)
          
            # TODO pprocrow extract pprocs
            pproc <- pproc_list[[pprocrow,"PPROC"]]
            
            # extract filt and predict types
            PTYPE1 <- method_list[[methrow,"PTYPE1"]]
            PTYPE2 <- method_list[[methrow,"PTYPE2"]]          
            FTYPE1 <- method_list[[methrow,"FTYPE1"]]          
            PTYPE3 <- method_list[[methrow,"PTYPE3"]]
            PTYPE4 <- method_list[[methrow,"PTYPE4"]]          
            method <- method_list[[methrow,"METHOD"]]
          
            #logtitle <- "Level1 Predict"
            log4r::info(logger, paste(logtitle,symbol1,group1,dt1,pproc1,method1,bubars))
            print(paste(logtitle,symbol1,group1,dt1,pproc1,method1,bubars))
            
            # Tune bar loops here #############################
            for (tb in bubars:0) {
              bupts <-  tb * ppb
            
              # current bar
              in1date <- request_data_l(msdataint=msdataint,type="date",bupts=bupts,len=datalen)
              in1 <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts,len=datalen)
              # target bar
              target <- rep(NA,datalen+ppb) # target unknown on current bar 
              if (bupts >= ppb) target <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts-ppb,len=datalen+ppb)
            
              in1pp <- in1
              in1pp <- preproc(PPROC=pproc, input=in1pp)
            
              currentTs <- Sys.time() # time this section
            
              # TODO replace with method
              f1 <- filter_func(in1pp,FTYPE1)
              fita1 <- auto.arima(f1, stepwise=FALSE, approximation=FALSE) #slower more accurate
              fc <- forecast::forecast(fita1, h=2)
              
              pred2 <- fc$fitted
              for (i in seq(1:length(fc$mean))) {
                pred2 <- append(pred2,fc$mean[i])
              }
              
              predict <- pred2
              predict <- undo_preproc(PPROC=pproc, input=predict)
              predict <- round(predict, digits=dec)
              
              eltime <- Sys.time() - currentTs # end time section
     
              tunedates <- tail(in1date,ppb)
              in1s <- tail(in1,ppb)
              predicts <- tail(predict,ppb)
              targets <- tail(target,ppb)
              
            # add_rows of scr to detailw         
              for (i in 1: ppb) {
                scr <- scr %>%  # change to scr to append rows
                  add_row(
                    date = tunedates[i],
                    symbol = symbol1,
                    dt = dt1,
                    group = group1,
                    pproc = pproc1,
                    method = method1,
                    price = in1s[i],
                    predict = predicts[i],
                    target = targets[i],
                    etime = round(as.double(eltime/ppb),4)
                )
              }
            } # end bubar loop
          } # DATA_CHANGED 
          # add rows scr to detailw
          scr <- dplyr::slice_tail(scr, n=(TOTALBARS + 1) * ppb)
          log4r::debug(logger, paste(logtitle, "adding scr to detailw", "rows=",nrow(scr))) 
          detailw <- detailw %>% add_row(scr)
          log4r::debug(logger, paste(logtitle, "detailw", "rows=",nrow(detailw))) 
        } # method loop
      } #  pproc loop
    } # dt loop
  } # group dt loop
} # symbol loop


# TODO tmp write for debugging - take out
log4r::debug(logger, paste(logtitle, "DETAIL rows=",nrow(DETAIL),"detailw rows=",nrow(detailw))) 
log4r::info(logger, paste(logtitle, "writing", "detailw.csv", " rows=",nrow(detailw))) 
write_csv(detailw,"detailw.csv",append = FALSE,col_names = TRUE) # tmp take out


# TODO Move to utility at some point
drop_cols <- function(df, ...){
  df %>% 
    select(-one_of(map_chr(enquos(...), quo_name)))
}

if (DATA_CHANGED == TRUE) {
  # TODO change to DETAIL when working
  #log4r::info(logger, paste(logtitle, "writing", L1_DETAIL_FILE, " rows=",nrow(detailw))) 
  #write_csv(detailw,"detailw.csv",append = FALSE,col_names = TRUE) # tmp take out
  #DETAIL <- detailw
  #write_csv(DETAIL,L1_DETAIL_FILE,append = FALSE,col_names = TRUE)
  ################### SCORE ####################################
  # score detail and rank - later by symbol group
  # will need group menus on web page
  #############################################################
  #detailw <- detailw %>% drop_cols(sderr,error) # tmp
  detailw <- detailw %>% dplyr::mutate(etime = round(etime,4)) #tmp

  ##########################################################
  ############## MAIN level 1 score loop ##################
  logtitle <- "L1_score"
  log4r::info(logger, paste("****************** L1_SCORE start **************"))
  detailw <- detailw %>% dplyr::mutate(error1 = target - predict)
  # loop through symbols
  symbol_list <- SYMBOL_LIST 
  for(symrow in 1:nrow(symbol_list )) { # symbol loop
    symbol1 <- symbol_list$symbol[[symrow]]
    dec <- symbol_list$decimals[[symrow]]
    print(paste0("######### ", symbol1, " ##########"))
    
    datatype_list <- DATATYPE_LIST 
    for (dtrow in 1:nrow(datatype_list)) { # datatype loop
      dt <- as.list(datatype_list$dt[[dtrow]])
      group1 <- datatype_list$group[[dtrow]]
      # hlcm in group loop
      for (col in 1:length(dt)) {  
        dt1 <- as.character(dt[col])
        
        # Pre Process loop
        pproc_list <- PPROC_LIST
        for(pprocrow in 1:nrow(pproc_list)) {        
          pproc1 <- pproc_list[[pprocrow,"PPROC"]]
          
          # Method loop
          method_list <- METHOD_LIST
          for(methrow in 1:nrow(method_list)) {          
            method1 <- method_list[[methrow,"METHOD"]]
            
            scr <- detailw %>% dplyr::filter(symbol==symbol1, group==group1,
                                            dt==dt1,pproc==pproc1,method==method1) 
    
            # zscore calc
            scr <- scr %>% dplyr::mutate(mean=mean(error1,na.rm = TRUE))
            scr <- scr %>% dplyr::mutate(stdev=sd(error1, na.rm = TRUE))
            scr <- scr %>% dplyr::mutate(zscore1 = abs((error1 - mean) / stdev))
            
            scr <- scr %>% dplyr::mutate(zscore1m=mean(zscore1,na.rm = TRUE))
            zscore1 <- round( tail(scr$zscore1m,1), 4)
            
            paste( logtitle,symbol1,group1,dt1,pproc1,method1,zscore1)
            
            log4r::debug(logger, paste( logtitle,symbol1,group1,dt1,pproc1,method1,zscore1))
    
          }
        }
      }
    }     
    log4r::info(logger, paste("****************** L1_SCORE stop **************"))    
  }
}

logtitle <- "L1_main"
L1_STOP_TIME <- Sys.time()
L1_ELAPSED_TIME <- round(L1_STOP_TIME - L1_START_TIME, digits=2)
log4r::info(logger, paste(logtitle,"Stop ",VNAME, VERSION, VDATE, "Elapsed=", L1_ELAPSED_TIME )) 
log4r::info(logger, paste("******************************************************"))
log4r::info(logger, paste("******************************************************"))


#################################################
## Main Logic - to be replaced soon
#################################################

# Validate that we have data for each in SYMBOL_LIST
# creates datafiles symbol.txt - uses msread.exe
for(i in 1:nrow(SYMBOL_LIST)) {
  symbol <- SYMBOL_LIST[[i,1]]
  #cline <- paste0("C:\\Users\\Lcalv\\Documents\\R\\teng3\\readms ", DATADIR, " ", symbol, " OHLC")
  cline <- paste0(BASEDIR,"\\readms ", DATADIR, " ", symbol, " OHLC")
  retval <- system(cline, minimize=TRUE, wait=TRUE)
  #message(cline,": ", retval)
}


dec <- 2 # tmp move to symbol_list


# if we have detail file just read it - skip main loop
if (file.exists(L1_DETAIL_FILE) == TRUE) {
  DETAIL <- read_csv(L1_DETAIL_FILE,col_names = TRUE)
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
  print("Will do full tune")
  DETAIL <- pred_bars(
            symbol_list=SYMBOL_LIST,
            datatype_list=DATATYPE_LIST,
			      pproc_list=PPROC_LIST,
            method_list=METHOD_LIST,
            intl=INTL,
            nbr_bars=TOTALBARS + 1
            )
}

write_csv(DETAIL,L1_DETAIL_FILE,append = FALSE,col_names = TRUE)


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
  
  # test filtered
  f1 <- filter_func(in1,"mf") #mf
  fita1 <- auto.arima(f1, stepwise=FALSE, approximation=FALSE) #slower more accurate
  
  #fita1 <- auto.arima(in1, stepwise=FALSE, approximation=FALSE) #slower more accurate
  fc <- forecast::forecast(fita1, h=ppb)
  
  pred2 <- fc$fitted
  for (i in seq(1:length(fc$mean))) {
    pred2 <- append(pred2,fc$mean[i])
  }
  
  predict <- pred2
  #predict <- undo_preproc(PPROC=pproc, input=predict)
  predict <- round(predict, digits=dec)
  
  #Plen <- ppb
  #predictx <- an_predict2(f1,"nn")
  
  # stopped working figure put later
  # predict hybridcv of err
  #usemod <- "aefnt" # hybrid drop the stl model requires Timeseries
  #mod <- forecastHybrid::hybridModel(in1, models = usemod,
  #                   weights = "cv.errors",
  #                   verbose = TRUE,
  #                   errorMethod = "RMSE",
  #                   cvHorizon =  ppb,
  #                   #windowSize = 10 * ppb
  #                   windowSize = 5 * ppb
  #                   )
  
  #mod
  #accuracy(mod)
  
  #mod_fit <- as_tibble(fitted(mod,individual = TRUE))
  #print(mod_fit)

  # write csv for debugging
  #in1v <- enframe(in1,name=NULL)
  #modl1_tbl <- dplyr::bind_cols(in1v,mod_fit)
  #write_csv(modl1_tbl,"modl1.csv",append = FALSE,col_names = TRUE)
  
  #fc <- forecast::forecast(mod, h=ppb)
  
  #print(fc$method)
   #View the point forecasts
  #fc$mean
   #View the upper prediction interval
  #fc$upper
   #View the lower prediction interval
  #fc$lower

  # Plot the forecast
  #plot(mod, type = "models")
  #plot(mod, type = "fit")    
  #plot(fc)

  #prediction is in fitted plus append mean
  #predict <- fc$fitted
  #length(predict)
  #for (i in seq(length(fc$mean))) {
  #  predict <- append(predict,fc$mean[i])
  #}
  #length(predict)
  
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
  #view(PREDTMP)
  
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
  symbol <- "1WN3"
  desc <- "July Wheat" 
  msdataint <- load_data(symbol=symbol,intl=INTL,dec=2) 
  # TODO advance date (and length) by ppb
  date  <- request_data_l(msdataint=msdataint,type="date",bupts=0,len=clen+ppb)
  open  <- request_data_l(msdataint=msdataint,type="o",bupts=0,len=clen)
  high  <- request_data_l(msdataint=msdataint,type="h",bupts=0,len=clen)
  low   <- request_data_l(msdataint=msdataint,type="l" ,bupts=0,len=clen)
  close <- request_data_l(msdataint=msdataint,type="c",bupts=0,len=clen)
  mid   <- request_data_l(msdataint=msdataint,type="m",bupts=0,len=clen)
  vol   <- request_data_l(msdataint=msdataint,type="vol",bupts=0,len=clen)  
  pred <- NULL
  for (i in seq(ppb)) {
    #date  <- append(date,NA)
    open  <- append(open,NA)
    high  <- append(high,NA)
    low   <- append(low,NA)
    close <- append(close,NA)
    mid   <- append(mid,NA)
    vol   <- append(vol,NA) #TODO replace zero at end with NA for charting   
  }
  
  pred  <- append(pred,tail(PREDTMP$predict,clen+ppb))
  
  # advance date by 1 day skipping weekends using ppb
  # does not account for trading holidays
  
  #############################################################
  #TODO in readdata.R do ymd_hms on date !!!!!!!!!!!!!!!!!!
  #############################################################
  
  # Handle interpolate 
  datet <- ymd_hms(date,tz=Sys.timezone()) # tmp
  #view(datet)
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
  #view(datet)  
  
  test2 <- datesv + days(1) # advance to next day
  print(wday(test2, label=TRUE))
  if (wday(test2, label=TRUE) == "Sat") { # skip Sat & Sun
    test2 <- datesv + days(3)
  }
  datet <- append(datet,test2)
  #view(datet)
  
  date <- tail(datet,clen+ppb)
  #view(date)
  
  dfa <- tibble(date,open,high,low,close,mid,vol,hpred,lpred,cpred,mpred)

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
  
  df["vol"][df["vol"] == 0] <- NA # vol make all 0's NA:s
  
  #zzin <- tibble(df$hpred,df$lpred)
  zzin <- tibble(df$cpred)
  zz <- TTR::ZigZag( zzin, change = 0.08 ) # test

  lbdate <- df$date[nrow(df)-1]
  
  TITLE <- paste0(desc," ", symbol, " LB=", lbdate)
  
  fig <- df %>% plot_ly(x = df$date, type="ohlc",
                        open = df$open, close = df$close,
                        high = df$high, low = df$low,name = "Price")
                      
  
  fig <- fig %>% add_lines(x = df$date, y = df$hpred, legendgroup = 'group1', name="hpred", line = list(color = 'purple', width = 0.75), inherit = F, visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = df$lpred, legendgroup = 'group1', name="lpred", line = list(color = 'purple', width = 0.75), inherit = F, visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = df$cpred, legendgroup = 'group2', name="cpred", line = list(color = 'red',    width = 1.25), inherit = F)
  fig <- fig %>% add_lines(x = df$date, y = df$mpred, legendgroup = 'group2', name="mpred", line = list(color = 'green',  width = 1.25), inherit = F)
  
  # test zigzag
  fig <- fig %>% add_lines(x = df$date, y = zz, name="zz", line = list(color = 'black',  width = 2.25), inherit = F)
  
  #fig <- fig %>% config(displayModeBar = TRUE, displaylogo = FALSE,
  #                      modeBarButtonsToRemove =
  #          c('zoomIn2d','zoomOut2d',"zoom2d",'pan2d',"select","autoscale","lasso",
  #            "hoverCompareCartesian","hoverClosest") )
    
  #fig <- fig %>% plotly::layout(title = TITLE, showlegend = F, 
  #            hovermode = 'x',
  #            xaxis = list(type = 'category',rangeslider = list(visible = F)),
  #            yaxis = list(title = "Price"))
  
  # Add the text annotation
  fig <- fig %>% add_annotations(
    x=0.5, y=1.0,  align='bottom',
    xref = "paper", yref = "paper",
    showarrow = FALSE, font_size=24,
    text="https://larrycalvert.github.io/charts2/" 
    # enclose text in <b> text </b> for bold
  )
  
  fig <- fig %>% plotly::layout(title = TITLE, showlegend = F, 
              hovermode = 'x',
              xaxis = list(type = 'category',rangeslider = list(visible = F)),
              yaxis = list(title = "Price"))
  
  fig
  
  #######################################################
    # plot volume bar chart
  # https://plotly.com/r/candlestick-charts/
  # TODO na remove of vol
  #fig2 <- df %>% plot_ly(x = df$date, y = df$vol, type='bar', name = "Volume")
  #fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
  
  # Vol bar chart
  #fig2 <- df %>% plot_ly(x = df$date, y = df$vol, type='bar',
  #                       name = "Vol",color = I("blue")) 
  
  #Vol line chart
  fig2 <- df %>% plot_ly(x = df$date, y = df$vol, type = 'scatter',
                         mode = 'lines', legendgroup = 'group3', name = "Vol", color = I("blue"))
   
  fig2 <- fig2 %>% plotly::layout(title = TITLE, showlegend = F, 
                                  hovermode = 'x',
                                  xaxis = list(type = 'category',rangeslider = list(visible = F)),
                                  yaxis = list(title = "Vol"))                       
  
  fig2
  
  ########### direction
  direction <- df$cpred - df$mpred
  fig3 <- df %>% plot_ly(x = df$date, y = direction, type = 'scatter',
                         mode = 'lines', name = "Direct",color = I("black"))
  #color = ~direction, colors = c('blue','red'))
  #fig3 <- fig3 %>% layout(yaxis = list(title = "Acct"))
  fig3 <- fig3 %>% plotly::layout(title = TITLE, showlegend = F, 
                                  hovermode = 'x',
                                  xaxis = list(type = 'category',rangeslider = list(visible = F)),
                                  yaxis = list(title = "Direct"))                       
  
  fig3
  
  ###########################################################
  # plot acct balance mockup
  bal <- seq.int(from=0,to=10000,length.out=length(df$date))
  rnum <- runif(length(df$date), min = 200, max = 800)
  tot <- bal + rnum
  tot[1] <- 0 #Force zero start
  fig4 <- df %>% plot_ly(x = df$date, y = tot, type = 'scatter',
                 mode = 'lines', legendgroup = 'group3', name = "Acct.", color = I("black"))
                 #color = ~direction, colors = c('blue','red'))
  #fig3 <- fig3 %>% layout(yaxis = list(title = "Acct"))
  fig4 <- fig4 %>% plotly::layout(title = TITLE, showlegend = F, 
                                  hovermode = 'x',
                                  xaxis = list(type = 'category',rangeslider = list(visible = F)),
                                  yaxis = list(title = "Acct."))                       
  
  fig4
  
  ################# subplot combine ############
  fig5 <- subplot(fig, fig2, fig4, heights = c(0.8,0.1,0.1), nrows=3,
                 shareX = TRUE, titleY = TRUE)
  
  fig5 <- fig5 %>% config(displayModeBar = TRUE, displaylogo = FALSE,
                        modeBarButtonsToRemove =
                        c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", 
                        "select", "autoscale", "lasso","hoverCompareCartesian",
                        "hoverClosest", "toImage"))
  
  fig5 <- fig5 %>% plotly::layout(title = TITLE, showlegend = T,
                                  xaxis = list(title = "Date")) 
                                  #                              hovermode = 'x',
                                  #xaxis = list(type = 'category',rangeslider = list(visible = F)),
                                  #                              yaxis = list(title = "Price"))
  fig5
  
  ########## Plot ###################################
  
  ####### Copy to server  ####################
  #and to charts direcetory
  filename <- paste0("charts/",symbol, "-",lbdate,".html")

  htmlwidgets::saveWidget(
    widget = fig5, #the plotly object
    file = filename, #the path & file name
    selfcontained = TRUE #creates a single html file
  )
  
  # copy to web server
  if (0) { # not being used
    serverloc <- "Y:/Public/plotly/index.html" # tmp copy to index.html for now
    status <- file.copy(from=filename,to=serverloc,overwrite = TRUE)
    if (status == TRUE) {
      print(paste0("Copied ",filename, " to ",serverloc))
    }
  
    serverloc <- "Y:/Public/plotly/charts"
    status <- file.copy(from=filename,to=serverloc,overwrite = TRUE)
    if (status == TRUE) {
      print(paste0("Copied ",filename, " to ",serverloc))
    }
    files <-list.files(path = serverloc,pattern = "*.html")
    print(paste0("Files in: ",serverloc))
    print(files)
  }
  
  #########################
  # copy to local directory
  serverloc <- "charts/wheat.html" # tmp copy to index.html for now
  status <- file.copy(from=filename,to=serverloc,overwrite = TRUE)
  if (status == TRUE) {
    print(paste0("Copied ",filename, " to ",serverloc))
  }
  
  serverloc <- "charts"
  #status <- file.copy(from=filename,to=serverloc,overwrite = TRUE)
  if (status == TRUE) {
    print(paste0("Copied ",filename, " to ",serverloc))
  }
  files <-list.files(path = serverloc,pattern = "*.html")
  print(paste0("Files in: ",serverloc))
  print(files)
  
  ################## git push start ####################################
  if (GITPUSH == TRUE) {
    dirsv <- getwd()
    setwd(paste0(dirsv,"/charts"))
  
    cline <- paste0("git config --global user.email \"lcalvert@comcast.net\"")
    retval <- system(cline, minimize=FALSE, wait=TRUE)
    print(paste0(cline, " retval=",retval))
  
    cline <- paste0("git config --global user.name \"larrycalvert\"")
    retval <- system(cline, minimize=FALSE, wait=TRUE)
    print(paste0(cline, " retval=",retval))

    #cline <- paste0("git add index.html wheat.txt")
    #retval <- system(cline, minimize=FALSE, wait=TRUE)
    #print(paste0(cline, " retval=",retval))

    cline <- paste0("git add --update")
    retval <- system(cline, minimize=FALSE, wait=TRUE)
    print(paste0(cline, " retval=",retval))
  
    #cline <- paste0("git commit -a")
    #retval <- system(cline, minimize=FALSE, wait=TRUE)
    #print(paste0(cline, " retval=",retval))
  
    #cline <- paste0("git commit -m \"Wheat plot update 2\" wheat.html")
    #retval <- system(cline, minimize=FALSE, wait=TRUE)
    #print(paste0(cline, " retval=",retval))
  
    cline <- paste0("git commit -m \"Wheat plot update 3\"")
    retval <- system(cline, minimize=FALSE, wait=TRUE)
    print(paste0(cline, " retval=",retval))
  
    cline <- paste0("git push")
    retval <- system(cline, minimize=FALSE, wait=TRUE)
    print(paste0(cline, " retval=",retval))
  
    setwd(dirsv)
  } #end GITPUSH
############## git push end #######################################

  
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
  