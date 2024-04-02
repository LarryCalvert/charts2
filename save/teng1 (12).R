# Trade tune and predict Engine

# Clear R studio workspace, plots, and console
if (!is.null(dev.list())) dev.off()
cat("\014") # Clear console
rm(list=ls()) # Clean work space

#############################################################
VNAME <- "teng1"
VERSION <- "V0.987"
VDATE <- "08/23/2023"
VAUTHOR <- "Larry Calvert"
#############################################################
# 08/06/2023  Added fetch stock data
# 07/28/2023 Trades using adjusted predha and predla
# 07/02/2-23  Adding trade triggers and markers
# 06/01/2023 separate catchup and scoring working
# 05/14/2023 Working on separate catchup and scoring - Level 1
# 04/30/2023 Testing combined L1 predict fulltune and catchup function
# 04/09/2023 Starting combined predict and catchup function
# 03/26/2023 adding L1_pred and L1_score
# 12/19/2022 changed method to char
# 12/21/2022 just before separating out bar predict as function -> detail tibble
#############################################################
GITPUSH <- TRUE
L2FORCE <- TRUE # Force scoring and L2 predict and charts
L2ERRP <- TRUE # PREDICT L2 error
DEBUG <- FALSE
LOGLEVEL <- "INFO" # set level - one of "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
DATADIR <- "c:\\futures\\working" #msdata file directory

# set our directory 
BASEDIR <- normalizePath(getwd())
LOGFILE <- paste0(BASEDIR, "\\teng.log")

##############  Packages ##########################################
if (!require("pacman")) install.packages("pacman")
#library("conflicted")
#library("pracma") # for interp1
#library("log4r")
#library("dplyr")
#library("tidyr") # only used for drop_na in filters
pacman::p_load("conflicted","pracma","log4r","dplyr")

conflicts_prefer(dplyr::filter) #Todo check all refs and take out when done
conflicts_prefer(forecast::forecast)

#library("readr")
#library("lubridate")
#library("tibble")
#library ("purrr") # required for drop_cols function
#library("itsmr") # test37 only want fft filter overides forecast
pacman::p_load("readr","lubridate","tibble")

#library("forecast")
#library("forecastHybrid") # needed for Hybrid predict
#library("beepr")
#library("marima") # test multi arima
#library("plotly")
#library("htmlwidgets")
#library("TTR") # Technical Trading Rules zigzag ind
pacman::p_load("forecast","forecastHybrid","plotly","htmlwidgets","TTR")

# TODO future:  load as needed - forecastml and others
#library("fable")
#library("tidymodels")
#library("modeltime")
#library("timetk")
#library("ggplot2")
#library("tidyr")
# for an_func7
#library("KernSmooth")
#library("bestNormalize") # for bestnormalize
#library("Rlibeemd") # for emd predict
#library("spectral") # for fft_filter
pacman::p_load("ggplot2","tidyr","KernSmooth","bestNormalize","Rlibeemd","spectral")

# for seome filters
#library("smooth") # for only smoothCombine or es if needed
#library("mFilter")
#library("itsmr") # for smooth.fft
#library("pracma") # for movavg
#library("tidyr")
#library("ggplot2")
#library("Metrics")
#library("chatgpt")
#library("tidyquant") #Need to fetch stock data
pacman::p_load("smooth","mFilter","itsmr","pracma","Metrics","tidyquant")


source("utility.R") # Larry's utilities
source("an_func7.R") # arma/tsnn prediction needed for pretty much everything
source("readdata.R") # Read metastock and stock data
source("tables.R")
source("preproc.R")
source("predbars.R")
source("Multiplot.R") # tmp for predict2 and below


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
if (file.exists(LOGFILE) == TRUE) {
  file.remove(LOGFILE) # delete LOGFILE
}
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


######### TEST SECTION ######################
if (0) {
  # ChatGPT
  reset_chat_session(system_role = "You are a helpful assistant.")
  cat(ask_chatgpt("What do you think about R language?"))
  comment_code(code = clipr::read_clip(allow_non_interactive = TRUE))
  document_code(code = clipr::read_clip(allow_non_interactive = TRUE))
  explain_code(code = clipr::read_clip(allow_non_interactive = TRUE))
  find_issues_in_code(code = clipr::read_clip(allow_non_interactive = TRUE))
  optimize_code(code = clipr::read_clip(allow_non_interactive = TRUE))
  
  
  clen <- datalen
  symbol <- "1WN3"
  desc <- "July Wheat" 
  bupts <- 2 * ppb
  msdataint <- load_data(symbol=symbol,intl=INTL,dec=2) 
  date  <- request_data_l(msdataint=msdataint,type="date",bupts=bupts,len=clen)
  open  <- request_data_l(msdataint=msdataint,type="o",bupts=bupts,len=clen)
  high  <- request_data_l(msdataint=msdataint,type="h",bupts=bupts,len=clen)
  low   <- request_data_l(msdataint=msdataint,type="l" ,bupts=bupts,len=clen)
  close <- request_data_l(msdataint=msdataint,type="c",bupts=bupts,len=clen)
  mid   <- request_data_l(msdataint=msdataint,type="m",bupts=bupts,len=clen)
  vol   <- request_data_l(msdataint=msdataint,type="vol",bupts=bupts,len=clen)  

  hro <- request_data_l(msdataint=msdataint,type="hro",bupts=bupts,len=clen) 
  lro <- request_data_l(msdataint=msdataint,type="lro",bupts=bupts,len=clen)   
  cro <- request_data_l(msdataint=msdataint,type="cro",bupts=bupts,len=clen)  
  
  hr <- request_data_l(msdataint=msdataint,type="hr",bupts=bupts,len=clen) 
  lr <- request_data_l(msdataint=msdataint,type="lr",bupts=bupts,len=clen)   
  cr <- request_data_l(msdataint=msdataint,type="cr",bupts=bupts,len=clen)  
  
  # extend date for plotting
  #dateext <- append(date,rep(NA,ppb))
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
  
  dateext <- tail(datet,clen+ppb)
  #view(dateext)
  
  #### To here for data #########################################
  ################################################################
  
  ##################### test pproc #####################
  if (0) {
    cro <- request_data_l(msdataint=msdataint,type="cro",bupts=bupts,len=clen)  
  
    in1 <- close
    view(in1)
    pproc <- "none"

    #source("preproc.R")
    in1pp <- preproc(PPROC=pproc, input=in1)
    #view(in1pp)
  
    in1test <- undo_preproc(PPROC=pproc, input=in1pp)
    #view(in1test)
  
    check <- in1 - in1test
    maxval <- max(abs(check))
    print(paste("Testing pproc max err=",maxval))
  
    #source("an_func7.R")
  
    TITLEB <- "E"
    pred1 <- predict2(in1pp, ptype1="emd", ptype2="none", chart = FALSE)
  
    in1pred <- undo_preproc(PPROC=pproc, input=pred1)
    #view(in1pred)
  }
  ####################################################
  
  ####### robfilter test ############
  if (0) {
    library("robfilter")
    
    # extend date for plotting
    #dateext <- append(date,rep(NA,ppb))
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
    
    dateext <- tail(datet,clen+ppb)
    #view(dateext)
    
    #in1 <- high
    #y.rr <- lms.filter(in1,width=41,online=FALSE)
    #y <- y.rr$level$LMS
    
    #y.rr <- lqd.filter(in1,width=41,online=FALSE)    
    #y <- y.rr$level$LQD
    
    #y.rr <- lts.filter(in1,width=41,online=FALSE)
    #y <- y.rr$level$LTS
    
    # TODO looks promising
    #in1 <- high
    #y.rr <- robreg.filter(in1, width=41, method=c("RM", "LMS", "LTS", "DR", "LQD"))
    #view(y.rr)
    #plot(y.rr)
    #y <- y.rr$level$LQD
    
    #y.rf <- robust.filter(in1, width=23)
    #y.rf2 <- robust.filter(in1, width=21, trend="LMS", scale="QN", outlier="W")
    #view(y.rf)
    #view(y.rf2)
    
    # very promising !!!!!!!!!!!!
    in1 <- close
    in1.WRM <- wrm.filter(in1, weight.type=2, width=6)
    #plot(in1.WRM)

    y <- in1.WRM$level  
    #y <- y.rr$level$LQD
    #y <- y.rf$level
    #y <- y.rf2$level
    
    #res <- in1 - y
    Plen <- ppb
    f1 <- filter_func(y, "mstl")
    p1 <- predict2(f1, ptype1="arma", ptype2="nn", chart = FALSE) # need predict of y

    in1ext <- append(in1,rep(NA,ppb))
    resext <- in1ext - p1
    tib <- tibble(dateext,in1ext,p1,resext)
    
    g1 <- ggplot(tib, aes(x=dateext) ) + 
      geom_line(aes(y = in1ext), color = "black") + geom_point(aes(y = in1ext), color = "black") +
      geom_line(aes(y = p1), color="red") + geom_point(aes(y = p1), color="red") +
      geom_vline(xintercept = tail(date,1), color="red", linewidth=0.5)      
    suppressWarnings(print(g1))
    
    
    g2 <- ggplot(tib, aes(x=dateext)) + 
      geom_line(aes(y = resext), color = "black") + geom_point(aes(y = resext), color = "black")
    suppressWarnings(print(g2))    
    
    print(paste("zscore resid=",round(ZSCORE1(resext),2),"Predict=",round(tail(p1,1),2)))
 
  }  
    
  ####### boxfilter test ############
  if (0) {
    library("boxfilter")
    in1 <- close
    p1 <- boxclip(x=date,y=in1,width=10, height=5)
    y <- p1$filtered
    tib <- tibble(date,in1,y)
    #view(tib)
    #write.csv(tib,file = "tibtest.csv")
    
    ggplot(tib, aes(x=date)) + 
      geom_line(aes(y = in1), color = "black") + 
      geom_line(aes(y = y), color="red", linetype="twodash") 
    
  }
  
  ####### smooth package test ############
  if (0) {
    library("smooth")
    
    #y <- ts(rnorm(100,10,3),frequency=12)
    y <- close
    # CES with and without holdout
    auto.ces(y,h=2,holdout=TRUE)
    auto.ces(y,h=2,holdout=FALSE)
    # Selection between "none" and "full" seasonalities
    auto.ces(y,h=2,holdout=TRUE,
             models=c("n","f"),interval="p",level=0.8,ic="AIC")
    ourModel <- auto.ces(y,interval="sp")
    summary(ourModel)
    
    forecast::forecast(ourModel)
    plot(forecast::forecast(ourModel))
    
    p1 <- forecast::forecast(ourModel,h=2)
    p1ext <- p1$forecast
    datext <- date
    datext <- append(datext,rep(NA,ppb))
    in1ext <- rep(NA,ppb)
    in1ext <- append(in1ext,in1)    
    tib <- tibble(datext,in1ext,p1ext)
    #view(tib)
    #write.csv(tib,file = "tibtest.csv")
    
    ggplot(tib, aes(x=datext)) + 
      geom_line(aes(y = in1ext), color = "black") + 
      geom_line(aes(y = p1ext), color="red", linetype="twodash") 
    
  }
  
  ####### kfino test ############
  # Kalman filter with impulse noised outliers
  if (0) {
    library("kfino")
    data(spring1)
    
    # --- Without Optimisation on parameters
    param2<-list(m0=41,
                 mm=45,
                 pp=0.5,
                 aa=0.001,
                 expertMin=30,
                 expertMax=75,
                 sigma2_m0=1,
                 sigma2_mm=0.05,
                 sigma2_pp=5,
                 K=2,
                 seqp=seq(0.5,0.7,0.1))
    
    resu2<-kfino_fit(datain=spring1,
                    Tvar="dateNum",Yvar="Poids",
                    param=param2,
                    doOptim=FALSE,
                    verbose=TRUE)
                 
    str(resu2$detectOutlier)    
     
    kfino_plot(resuin=resu2,typeG="quanti",
               Tvar="Day",Yvar="Poids",Ident="IDE")
    
    # --- With Optimisation on parameters
    param3<-list(m0=NULL,
                 mm=NULL,
                 pp=NULL,
                 aa=0.001,
                 expertMin=10,
                 expertMax=45,
                 sigma2_m0=1,
                 sigma2_mm=0.05,
                 sigma2_pp=5,
                 K=2,
                 seqp=seq(0.5,0.7,0.1))
    resu3<-kfino_fit(datain=merinos1,
                     Tvar="dateNum",Yvar="Poids",
                     doOptim=TRUE,param=param3,
                     verbose=TRUE)
    
    kfino_plot(resuin=resu3,typeG="quali",
               Tvar="Day",Yvar="Poids",Ident="IDE")  
    
    kfino_plot(resuin=resu3,typeG="prediction",
               Tvar="Day",Yvar="Poids",Ident="IDE")
    
    #-------------------------------
    datenum <- as.numeric(date)
    in1 <- close
    tib <- tibble(date,datenum,in1)
    
    param3<-list(m0=NULL,
                 mm=NULL,
                 pp=NULL,
                 aa=0.001,
                 expertMin=min(in1),
                 expertMax=max(in1),
                 sigma2_m0=1,
                 sigma2_mm=0.05,
                 sigma2_pp=5,
                 K=2,
                 seqp=seq(0.5,0.7,0.1))
    
    resu3<-kfino_fit(datain=tib,
                     Tvar="datenum",Yvar="in1",
                     doOptim=TRUE,param=param3,
                     verbose=TRUE)
    
    kfino_plot(resuin=resu3,typeG="quali",
               Tvar="datenum",Yvar="in1",Ident="date")  
    
    kfino_plot(resuin=resu3,typeG="prediction",
               Tvar="datenum",Yvar="in1",Ident="date")
    
  }
  
  ########## test EMD #########
  if (0) {

    library("Rlibeemd") # for emd predict

    theme_set(theme_minimal())
    TITLEB <- "E"
    Plen <- 2
    BACKTEST <- FALSE # an_predict2 

    in1 <- low
    title <- paste("Low","LB=",tail(date,1))
    p1 <- emd_predict2(in1, ptype1="arma", ptype2="arma",chart = TRUE,title = title)
    
    in1ext <- append(in1,rep(NA,ppb))
    resext <- in1ext - p1
    tib <- tibble(dateext,in1ext,p1,resext)
    g1 <- ggplot(tib, aes(x=dateext) ) + 
      geom_line(aes(y = in1ext), color = "black") + geom_point(aes(y = in1ext), color = "black") +
      geom_line(aes(y = p1), color="red") + geom_point(aes(y = p1), color="red") +
      geom_vline(xintercept = tail(date,1), color="red", linewidth=0.5) +
      labs(title=title, x ="Date", y = "Price")      
    suppressWarnings(print(g1))
    
    
    g2 <- ggplot(tib, aes(x=dateext)) + 
      geom_line(aes(y = resext), color = "black") + geom_point(aes(y = resext), color = "black") +
      labs(title=title, x ="Date", y = "Resid")         
    suppressWarnings(print(g2))    
    
    print(paste("zscore resid=",round(ZSCORE1(resext),2),"Predict=",round(tail(p1,1),2)))
    
  ################################################################
  }
  
  ####### Filter test ##################################
  if (0) {
    library(smooth)
    
    m1 <- adam(in1, h=2,model="FFF",forecast=ppb)
    adamSummary <- summary(m1)
    xtable(adamSummary)
    p01 <- forecast::forecast(m1,h=2)
    p02 <- data.frame(p01)
    
    #y <- in1
    #a1 <- adam(y,h=ppb,holdout=TRUE)
    #e1 <- es(y,h=ppb,holdout=TRUE)
    #g1 <- gum(y,h=ppb,holdout=TRUE)
    #c1 <- auto.ces(y,h=ppb,holdout=TRUE)
    #a2 <- auto.ssarima(y,h=ppb,holdout=TRUE)
    

    #f1 <- smoothCombine(in1,h=ppb,initial = "optimal")
    
    #f1 <- filter_func(in1,"aspline")
    #f1 <- round(f1,4)
    
    # create p1 length of f1 with all NAs
    #extend date,in1,f1 to length of p1 
    datext <- date
    datext <- append(datext,rep(NA,ppb))
    in1ext <- rep(NA,ppb)
    in1ext <- append(in1ext,in1)

    tib <- tibble(datext,in1ext,p1)
    #view(tib)
    #write.csv(tib,file = "tibtest.csv")

    ggplot(tib, aes(x=date)) + 
      geom_line(aes(y = in1), color = "black") + 
      geom_line(aes(y = p1), color="red", linetype="twodash") 
  }
  
}
########### END TEST #######################


#TODO LRC get date format right
# check date format
#msdataint <- load_data(symbol="1WZ2",intl=INTL,dec=2)
#view(head(msdataint$date,n=10))
#view(head(msdataint$datet,n=10))

#################################################
####### L1_predict_main        ##################
#################################################

# TODO add menu here

L1_DETAIL_FILE <- "L1_detail.csv"
DATA_CHANGED <- FALSE

# get DETAIL
# if we have detail file read it check and catchup if need be
DETAIL <- DETAIL_LINES
if (file.exists(L1_DETAIL_FILE) == TRUE) {
  DETAIL <- read_csv(L1_DETAIL_FILE,col_names = TRUE)
  log4r::info(logger, paste(logtitle, L1_DETAIL_FILE, " read rows=",nrow(DETAIL))) 
} else {
  cat(paste(L1_DETAIL_FILE, " not found - will do full tune\n"))
  log4r::info(logger, paste(logtitle, L1_DETAIL_FILE, " not found")) 
}


logtitle <- "L1_predict"
detailw <- DETAIL_LINES # start with empty work file

# loop through symbols
symbol_list <- SYMBOL_LIST 
for(symrow in 1:nrow(symbol_list )) { # symbol loop
  symbol1 <- symbol_list$symbol[[symrow]]
  desc1 <- symbol_list$desc[[symrow]]
  source <- symbol_list$source[[symrow]]  
  dec <- symbol_list$decimals[[symrow]]
  
  #cat("\014") # Clear console 
  cat(paste0("######### ", symbol1, " ##########\n"))

  FOUND <- FALSE
  ############## READ MS data ###########################################
  if (source == "msdata") {
    cline <- paste0(BASEDIR,"\\readms ", DATADIR, " ", symbol1, " OHLC")
    retval <- system(cline, minimize=TRUE, wait=TRUE)
    msdataint <- load_data(symbol=symbol1,intl=INTL,dec=dec) 
    FOUND <- TRUE
  }
  if (source == "tq") {
    # source("readdata.R")
    #symbol1 <- "META"
    #dec=2
    msdata <- tq_get(symbol1) 
    STOCK_FILE <- paste0(symbol1,".txt")
    message = FALSE
    write_csv(msdata, STOCK_FILE, append = FALSE, col_names = FALSE) 
    message = TRUE
    msdataint <- load_data2(symbol=symbol1,intl=INTL,dec=dec) 
    FOUND <- TRUE    
  }
  if (FOUND == FALSE) {
    print(paste("Invalid data source ",symrow,symbol1,source))
    UNDECLARED() # makeshift stop script
  }
  
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
          FULLTUNE <- FALSE
          
          # select this data from DETAIL file
          scr <- DETAIL %>% dplyr::filter(symbol==symbol1, group==group1,
                      dt==dt1,pproc==pproc1,method==method1) 
          nbr_rows <- nrow(scr)
            
          log4r::debug(logger, paste("******************************************"))
          log4r::debug(logger, paste( logtitle,symbol1,group1,dt1,pproc1,method1))
          log4r::debug(logger, paste( logtitle,"Want data rows=",(TOTALBARS +1) * ppb,"Found=",nbr_rows)) 
            
          if (nbr_rows < (TOTALBARS +1) * ppb) { #new tune
            FULLTUNE <- TRUE
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
            cat(paste( logtitle,symbol1,desc1,group1,dt1,pproc1,method1,"bars=",nbr_rows/ppb,"All caught up\n"))
            log4r::info(logger,paste( logtitle,symbol1,desc1,group1,dt1,pproc1,method1,"bars=",nbr_rows/ppb,"All caught up")) 
          } else { # additional data has been added
            DATA_CHANGED <- TRUE
            #print(paste0("Need to predict bar(s)=",bubars))
            log4r::info(logger,paste( logtitle, "Need to predict bar(s)=",bubars))
            
            # Skip target update on FULLTUNE
            if (FULLTUNE == FALSE) {
              # last scr bar fill in target and error
              bupts <- bubars*ppb 
            
              targdate <- request_data_l(msdataint=msdataint,type="date",bupts=bupts-ppb,len=datalen)
              target <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts-ppb,len=datalen)
      
              # fill in scr target
              for(i in seq(from = ppb-1, to = 0, by = -1)) {
                log4r::debug(logger,paste(logtitle, scr$date[nrow(scr)-i],
                  scr$target[nrow(scr)-i],
                  target[length(target)-i]))
              
                scr$target[nrow(scr)-i] = target[length(target)-i]
                scr$error[nrow(scr)-i] = round(scr$target[nrow(scr)-i] - scr$predict[nrow(scr)-i],dec)
              }
              
              bubars <- bubars - 1 # no need to predict this one - already done  
            } # end update target

                        
           ######################################################           
            # do predicts

            # TODO pprocrow extract pprocs
            pproc <- pproc_list[[pprocrow,"PPROC"]]
            
            # extract filt and predict types
            PTYPE1 <- method_list[[methrow,"PTYPE1"]]
            PTYPE2 <- method_list[[methrow,"PTYPE2"]]          
            FTYPE1 <- method_list[[methrow,"FTYPE1"]]          
            PTYPE3 <- method_list[[methrow,"PTYPE3"]]
            PTYPE4 <- method_list[[methrow,"PTYPE4"]]          
            method <- method_list[[methrow,"METHOD"]]
          
            #cat("\014") # Clear console 
            
            log4r::debug(logger, paste(logtitle,symbol1,desc1,group1,dt1,pproc1,method1,bubars))
            cat(paste(logtitle,symbol1,desc1,group1,dt1,pproc1,method1,"bars=",bubars,"\n"))
            sectionTs <- Sys.time() # time this section of pproc method
            
            # Tune bar loops here #############################
            for (tb in bubars:0) {
              cat(".")
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
              Plen <- ppb #TODO fix this
              
              pred1 <- predict2(in1pp, ptype1=PTYPE1, ptype2=PTYPE2, chart = FALSE)
              
              # TODO replace with method
              f1 <- filter_func(pred1,FTYPE1)
              #fita1 <- auto.arima(f1, stepwise=FALSE, approximation=FALSE) #slower more accurate
              #fc <- forecast::forecast(fita1, h=2)
              
              #pred2 <- fc$fitted
              #for (i in seq(1:length(fc$mean))) {
              #  pred2 <- append(pred2,fc$mean[i])
              #}
              
              pred2 <- predict2(f1, ptype1=PTYPE3, ptype2=PTYPE4, chart = FALSE)
          
              predict <- undo_preproc(PPROC=pproc, input=pred2)
              predict <- round(predict, digits=dec)
              
              eltime <- Sys.time() - currentTs # end time section
     
              tunedates <- tail(in1date,ppb)
              in1s <- tail(in1,ppb)
              predicts <- tail(predict,ppb)
              targets <- tail(target,ppb)
              
            # add_rows of scr to detailw         
              for (i in 1: ppb) {
                scr <- scr %>%  
                  add_row(
                    date = tunedates[i],
                    symbol = symbol1,
                    desc = desc1,
                    dt = dt1,
                    group = group1,
                    pproc = pproc1,
                    method = method1,
                    price = in1s[i],
                    predict = predicts[i],
                    target = targets[i],
                    error = round(targets[i] - predicts[i],dec),
                    etime = round(as.double(eltime/ppb),4)
                )
              }
            } # end bubar loop
            secteltime <- round(Sys.time() - sectionTs,0) # end time section
            cat("\r                                                                    \r") # ending printed dots
            cat(paste(logtitle,symbol1,desc1,group1,dt1,pproc1,method1,"bars=",bubars,"time=",secteltime,"seconds\n"))
            log4r::info(logger,paste(logtitle,symbol1,desc1,group1,dt1,pproc1,method1,"bars=",bubars,"time=",secteltime,"seconds"))
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


if (L2FORCE == TRUE) DATA_CHANGED <- TRUE 

if (DATA_CHANGED == TRUE) {
  DETAIL <- detailw
  log4r::info(logger, paste(logtitle, "writing", L1_DETAIL_FILE, "rows=", nrow(DETAIL))) 
  write_csv(DETAIL, L1_DETAIL_FILE, append = FALSE, col_names = TRUE) 
  
  # score each group LRC
  # L1_score1.cs summarizes by pproc method
  # DETAIL3 summarizes by group
  
  L1_SCORE1_FILE <- "L1_score1.csv"
  
  L1_SCORE1_LINE <- tibble(
    #tunedate = Date(),
    symbol   = character(),
    group    = character(),
    dt       = character(),
    pproc    = character(),
    method   = character(),
    etime    = double(),
    mean     = double(),
    sd       = double(),
    zscore   = double()  
  )
  
  L1_SCORE1 <- L1_SCORE1_LINE
  ##########################################################
  ############## MAIN level 1 score loop ##################
  logtitle <- "L1_score"
  tunedate <- date()
  log4r::info(logger, paste("****************** L1_SCORE start **************"))
  #DETAIL <- DETAIL %>% dplyr::mutate(error1 = target - predict)
  
  ################ okotly fig list for within loop
  PLOTLIST <- list()


  # loop through symbols
  symbol_list <- SYMBOL_LIST 
  for(symrow in 1:nrow(symbol_list )) { # symbol loop
    symbol1 <- symbol_list$symbol[[symrow]]
    dec <- symbol_list$decimals[[symrow]]
    minmove <- symbol_list$minmove[[symrow]]
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
          etime <- 0
          for(methrow in 1:nrow(method_list)) {          
            method1 <- method_list[[methrow,"METHOD"]]
            
            scr <- DETAIL %>% dplyr::filter(symbol==symbol1, group==group1,
                                            dt==dt1,pproc==pproc1,method==method1) 
    
            # cumulate etime
            scr <- scr %>% dplyr::mutate(etimetot=cumsum(etime))
            etime <- tail(scr$etimetot,1) / ppb
            
            # zscore calc
            #scr <- scr %>% dplyr::mutate(mean=mean(error,na.rm = TRUE))
            #scr <- scr %>% dplyr::mutate(stdev=sd(error, na.rm = TRUE))
            #scr <- scr %>% dplyr::mutate(zscore1 = abs((error - mean) / stdev))
       
            #scr <- scr %>% dplyr::mutate(zscore1m=mean(zscore1,na.rm = TRUE))
            #scr <- scr %>% dplyr::mutate(zscore1v=var(zscore1,na.rm = TRUE))
            #scr <- scr %>% dplyr::mutate(zscore1c=(zscore1m + zscore1v) / 2)              
            #zscore <- round( tail(scr$zscore1c,1), 4)
            
            #write_csv(scr, "scr.csv", append = FALSE, col_names = TRUE) # tmp for debug
            
            # Revised scoring 06/17/2023
            errorna <- scr$error[!is.na(scr$error)] # remove NAs
            mean1   <- round(mean(errorna), dec * 2) 
            sd1     <- round(mean(sd(errorna)), dec * 2)

            # New zscore test
            #errorrange <- max(abs(errorna)) - min(abs(errorna))
            
            # Calculate the mean absolute error
            #mean_error <- mean(abs(errors))
                        # Convert mean error to a percentage
            #error_percent <- mean_error * 100
            #zscore <- error_precent
            
            # Calculate coefficient of variation (CV)
            #cv <- sd(scr$error[!is.na(scr$error)]) / mean(scr$price[!is.na(scr$price)])
            #zscore <- cv
            
            line1   <- "Score: zscore and max"
            
            zscore <- mean(abs(ZSCORE(scr$error))) # mean or RMSE of ZSCORE of error
            
            zscore <- zscore + MAXE(zscore)/2
            
            #zscore <- sd(scr$error/minmove,na.rm=T)
            #zscore <- smape(na.omit(scr$target),scr$predict)
            #zscore <- rmse(na.omit(scr$target),scr$predict)
            
            #zscore <- mase(na.omit(scr$target),scr$predict,step_size = 1) # star
                         
            zscore  <- round( zscore, dec * 2)                                         

            #zscore  <- round( sd1, dec * 2)  ######### TMP TAKE OUT - TEST ###
            #var1    <- round(var(zscorev), dec * 2)
            
            L1_SCORE1 <- L1_SCORE1 %>% 
              add_row(
                #tunedate = date(),
                symbol = symbol1,
                group = group1,
                dt = dt1,
                pproc = pproc1,
                method = method1,
                etime = etime,
                mean  = mean1,
                sd    = sd1,
                zscore = zscore                
              )
           log4r::debug(logger, paste( logtitle,symbol1,group1,dt1,pproc1,method1,zscore))
          }
        }
      }
    }
  }

  log4r::info(logger, paste(logtitle, "writing", L1_SCORE1_FILE, "rows=", nrow(L1_SCORE1))) 
  write_csv(L1_SCORE1, L1_SCORE1_FILE, append = FALSE, col_names = TRUE) # tmp take out
  log4r::info(logger, paste("****************** L1_SCORE stop **************"))    

  # accum score by group and method
  L1_SCORE2_FILE <- "L1_score2.csv"
  
  L1_SCORE2_LINE <- tibble(
    #tunedate = Date(),
    symbol   = character(),
    group    = character(),
    dt       = character(),
    pproc    = character(),
    method   = character(),
    etime    = double(),
    mean     = double(),
    sd       = double(),
    zscore   = double()  
  )
  
  L1_SCORE2 <- L1_SCORE2_LINE

  # select best score (lowest) by datatype within group
  
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
        scr <- L1_SCORE1 %>% dplyr::filter(symbol==symbol1,group==group1,
                               dt==dt1) %>% dplyr::arrange(zscore)
        
        ### TEST arrange by sd rather than zscore
        #scr <- L1_SCORE1 %>% dplyr::filter(symbol==symbol1,group==group1,
        #                     dt==dt1) %>% dplyr::arrange(sd)

                
        scr2 <- head(scr,1) # want lowest score 
        
        L1_SCORE2 <- L1_SCORE2 %>% 
          add_row(
            #tunedate = lastdate,
            symbol   = symbol1,
            group    = group1,
            dt       = dt1,
            pproc    = scr2$pproc,
            method   = scr2$method,
            etime    = scr2$etime,
            mean     = scr2$mean,
            sd       = scr2$sd,            
            zscore    = scr2$zscore              
          )
      } # datatype loop    
    } # group loop
  } #symbol loop
  
  #view(L1_SCORE2)
  write_csv(L1_SCORE2,L1_SCORE2_FILE,append = FALSE,col_names = TRUE)
  
  ##################################################################
  # select best score (lowest) by datatype within group
  L1_SCORE3_FILE <- "L1_score3.csv"
  
  # accum score by group and method
  L1_SCORE3_LINE <- tibble(
    #tunedate = Date(),
    symbol   = character(),
    desc     = character(),    
    filename = character(),
    group    = character(),
    etime    = double(),
    zscore    = double()  
  )
  
  L1_SCORE3 <- L1_SCORE3_LINE
  
  # select best score (lowest) by datatype within group
  
  #symrow <- 1
  for(symrow in 1:nrow(SYMBOL_LIST)) { # symbol loop
    symbol1 <- SYMBOL_LIST$symbol[[symrow]]
    filename1 <- SYMBOL_LIST$filename[[symrow]]
    desc1 <- SYMBOL_LIST$desc[[symrow]]
    #dtrow <- 1
    for (dtrow in 1:nrow(DATATYPE_LIST)) { # datatype loop
      dt <- as.list(DATATYPE_LIST$dt[[dtrow]])
      group1 <- DATATYPE_LIST$group[[dtrow]]
      # hlcm in group loop
      accum <- 0
      cnt <- 0
      etime <- 0
      #col1 <- 1
      for (col1 in 1:length(dt)) {  
        dt1 <- as.character(dt[col1])
        
        scr <- L1_SCORE2 %>% dplyr::filter(symbol==symbol1,group==group1,dt==dt1)
        accum <- accum + scr$zscore
        cnt <- cnt + 1
        etime <- etime + scr$etime
      } # datatype loop 
      
      L1_SCORE3 <- L1_SCORE3 %>% 
        add_row(
          #tunedate = lastdate,
          symbol = symbol1,
          desc = desc1,
          filename = filename1,          
          group = group1,
          etime = etime,
          zscore = round(accum / cnt, 2*dec)              
        )
    } # group loop
  } #symbol loop
  #view(L1_SCORE3)
  write_csv(L1_SCORE3, L1_SCORE3_FILE, append = FALSE,col_names = TRUE) # take out col-names for append

  ##################################################################
  # select best score (lowest) by Symbol just sort L1_score4
  
  L1_SCORE4_FILE <- "L1_score4.csv"

 
  L1_SCORE4 <- L1_SCORE3 %>% dplyr::arrange(zscore)
  #view(L1_SCORE4)
  write_csv(L1_SCORE4, L1_SCORE4_FILE, append = FALSE,col_names = TRUE)
  #####################################################################
  
  # score5 best per symbol
  # select best score (lowest) by symbol
  L1_SCORE5 <- L1_SCORE3_LINE  
  
  #symrow <- 1
  for(symrow in 1:nrow(SYMBOL_LIST)) { # symbol loop
    symbol1 <- SYMBOL_LIST$symbol[[symrow]]
  
    scr <- L1_SCORE4 %>% dplyr::filter(symbol==symbol1) %>% 
      dplyr::arrange(zscore) %>% dplyr::slice_head(n=1)
    
    L1_SCORE5 <- L1_SCORE5 %>% 
        add_row(
          #tunedate = lastdate,
          symbol = scr$symbol,
          desc = scr$desc,          
          filename = scr$filename,
          group = scr$group,
          etime = scr$etime,
          zscore = scr$zscore              
        )
  } #symbol loop
  L1_SCORE5_FILE <- "L1_score5.csv"
  #view(L1_SCORE5)
  write_csv(L1_SCORE5, L1_SCORE5_FILE, append = FALSE,col_names = TRUE)
  #####################################################################
  
  #score6 arrange score5 by zscore to get best symbol list
  L1_SCORE6 <- L1_SCORE3_LINE  
  L1_SCORE6 <- L1_SCORE5 %>% dplyr::arrange(zscore)
  
  L1_SCORE6_FILE <- "L1_score6.csv"
  #view(L1_SCORE6)
  write_csv(L1_SCORE6, L1_SCORE6_FILE, append = FALSE,col_names = TRUE)
  
  
  
  # Now get best predicts and plot
  if (0) {
  selsymbols <- L1_SCORE5 # in symbol table order 
  #selsymbols <- L1_SCORE6 # in best predict order 
  for(symrow in 1:nrow(selsymbols)) { # symbol loop
    symbol1 <- selsymbols$symbol[[symrow]]
    
    # vol to detail ?
    
    # select group
    # for each datatype in group
    # select best pproc and method
    # select those from detail
    # get date predict target erroe vol
    
    
    scr <- L1_SCORE4 %>% dplyr::filter(symbol==symbol1) %>% 
      dplyr::arrange(zscore) %>% dplyr::slice_head(n=1)
  }
  } #if
  
} # end score
  
logtitle <- "L1_main"
L1_STOP_TIME <- Sys.time()
L1_ELAPSED_TIME <- round(L1_STOP_TIME - L1_START_TIME, digits=2)
log4r::info(logger, paste(logtitle,"Stop ",VNAME, VERSION, VDATE, "Elapsed=", L1_ELAPSED_TIME )) 
log4r::info(logger, paste("******************************************************"))
log4r::info(logger, paste("******************************************************"))


#################################################
## Main Logic - to be replaced soon
#################################################


selsymbols <- L1_SCORE5 # in symbol table order 
#selsymbols <- L1_SCORE6 # in best predict order 

# Now using BESTSUM get group dt and best method
#symrow <- 1
for(symrow in 1:nrow(selsymbols)) { # symbol loop
  symbol1 <- selsymbols$symbol[[symrow]]
  zscore_symbol <- selsymbols$zscore[[symrow]]
  # get desc
  #scr2 <- SYMBOL_LIST %>% dplyr::filter(symbol==symbol1)
  #desc <- scr2$desc
  desc1 <- selsymbols$desc[[symrow]]
  serverfile <- selsymbols$filename[[symrow]]
  
  group1 <- selsymbols$group[[symrow]]
  scr <- L1_SCORE2 %>% dplyr::filter(symbol==symbol1,group==group1)
  
  PREDTAB <- tibble(
    symbol   = character(),
    desc     = character(),
    group    = character(),
    dt       = character(),
    pproc    = character(),
    method   = character(),
    zscore    = double()  
  )
  
  for (dtrow in 1:nrow(scr)) { # datatype loop
    PREDTAB <- PREDTAB %>% 
      add_row(
        symbol = symbol1,
        desc   = desc1,
        group = group1,
        dt = scr$dt[[dtrow]],
        pproc = scr$pproc[[dtrow]],
        method = scr$method[[dtrow]],
        zscore = scr$zscore[[dtrow]]
      )
    #print(paste0(symbol1," ",group1," ",dt," ",method," ",score))
  } 
#}

#view(PREDTAB)
write_csv(PREDTAB,"predtab.csv",append = FALSE,col_names = TRUE) # take out col-names for append

##########################################################
# do err predicts
#if (file.exists("predtab.csv") == TRUE) {
#  PREDTAB <- read_csv("predtab.csv",col_names = TRUE)
#}

for(predrow in 1:nrow(PREDTAB)) { 
  symbol1 <- PREDTAB$symbol[[predrow]]
  desc1 <- PREDTAB$desc[[predrow]]
  group1  <- PREDTAB$group[[predrow]]
  zscoret <- round(PREDTAB$zscore[[predrow]],dec)
  line_symbol <- paste(symbol1,zscore)
  print(line_symbol)
  
  dt1 <- PREDTAB$dt[[predrow]]
  pproc1 = PREDTAB$pproc[[predrow]]
  method1 = PREDTAB$method[[predrow]]
  # get prior predicts and err
  #scr <- DETAIL %>% dplyr::filter(symbol==symbol1,dt==dt1,
  #         pproc==pproc,method==method) %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))
  scr <- DETAIL %>% dplyr::filter(symbol==symbol1 & dt==dt1 & pproc==pproc1 &
          method==method1) %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))
  
  #view(scr)

  in1 <- scr$error
  in1 <- in1[!is.na(in1)] # remove NAs

  Plen = ppb
  print(paste("Predicting error for",symbol1,dt1,pproc1,method1))
  log4r::debug(logger, paste( logtitle,"Predicting error for",symbol1,dt1,pproc1,method1))

  ########### L2 ERROR PREDICT #########################
  pred2 <- scr$predict
  
  if (L2ERRP == TRUE ) {
    # test filtered
    #f1 <- filter_func(in1,"mstl") #zlma
    f1 <- filter_func(in1,"zlma") #zlma
    
    #pred2a <- predict2(f1, ptype1="arma", ptype2="nn", chart = FALSE)
  
    pred2a <- predict2(in1, ptype1="hybridcv", ptype2="none", chart = FALSE) # slow
   
    #pred2a <- predict2(in1, ptype1="hybridw", ptype2="hybridw", chart = FALSE) # slow
    
    #pred2a <- predict2(in1, ptype1="nn", ptype2="none", chart = FALSE) # slow
    
    #pred2a <- predict2(in1, ptype1="arma", ptype2="nn", chart = FALSE) # faster
  
    #pred2a <- predict2(in1, ptype1="emd", ptype2="emd", chart = FALSE)
    #pred2a <- predict2(in1, ptype1="none", ptype2="none", chart = FALSE)
  
    pred2 <- round(scr$predict + pred2a, dec)
  }
  error2 <- round(scr$target - pred2, dec)
  
  PREDTMP <- tibble(
    symbol   = character(),
    desc     = character(),    
    group    = character(),
    dt       = character(),
    price    = double(),
    target  = double(),
    predict1  = double(),
    predict2  = double(),    
    error1    = double(),
    error2    = double(),    
  )
  
  PREDTMP <- PREDTMP %>% 
    add_row(
      symbol = symbol1,
      desc   = desc1, 
      group = group1,
      dt = dt1,
      price = scr$price,
      target = scr$target,
      predict1 = scr$predict,
      predict2 = pred2,      
      error1 = scr$error,
      error2 = error2
    )
  
  #view(PREDTMP)
  #zscore1 <- round(mean(ZSCORE(scr$error)),dec)
  #zscore2 <- round(mean(ZSCORE(error2)),dec)
  #zscore1 <- round(mean(abs(ZSCORE(scr$error))),dec)
  zscore1 <- mean(abs(ZSCORE(scr$error)))
  zscore1 <- zscore1 + MAXE(zscore1)/2  
  zscore1 <- round(zscore1,dec)
  
  #zscore2 <- round(mean(abs(ZSCORE(error2))),dec)  
  zscore2 <- mean(abs(ZSCORE(error2)))
  zscore2 <- zscore2 + MAXE(zscore2)/2
  zscore2 <- round(zscore2,dec)
  
  print(paste("L2",symbol1,"-",dt1,"zscore1=",zscore1,"zscore2=",zscore2))
  log4r::info(logger, paste("L2",symbol1,"-",dt1,"zscore1=",zscore1,"zscore2=",zscore2))
  
  filename <- paste0("L2_",symbol1,"-",dt1,".csv")
  #print(filename)
  #write_csv(PREDTMP,filename,append = FALSE,col_names = TRUE)
  #write_csv(PREDTMP,"predtmp.csv",append = FALSE,col_names = TRUE)

  # take best predict
  zscore <- zscore2
  if (zscore1 < zscore2)
  {
    pred2 <- scr$predict
    zscore <- zscore1
  }    
  
  if (dt1 == "h" || dt1 == "hro" || dt1 == "hr")
  {
    hpred <- pred2
    lineh <- paste("hpred ",dt1,pproc1,method1,zscore1,zscore2)
  }  

  if (dt1 == "l" || dt1 == "lro" || dt1 == "lr")
  {
    lpred <- pred2
    linel <- paste("lpred ",dt1,pproc1,method1,zscore1,zscore2)
  }  
  
  if (dt1 == "c" || dt1 == "cro" || dt1 == "cr")
  {
    cpred <- pred2
    linec <- paste("cpred ",dt1,pproc1,method1,zscore1,zscore2)
  }  
  
  if (dt1 == "m" || dt1 == "mro" || dt1 == "mr")
  {
    mpred <- pred2
    linem <- paste("mpred ",dt1,pproc1,method1,zscore1,zscore2)
  }  
  
}


#plotly test
  #library(plotly)
  #df <- data.frame(Date=index(AAPL),coredata(AAPL))
  #df <- tail(df, 30)
  clen <- TUNEBARS * ppb
  symbol <- symbol1
  #desc <- "July Wheat" 
  #msdataint <- load_data(symbol=symbol,intl=INTL,dec=2) 
  
  ############## READ MS data ###########################################
  # find source for data by looking up symbol
  sym <- SYMBOL_LIST %>% dplyr::filter(symbol==symbol1)
  source <- sym$source
  FOUND <- FALSE
  
  if (source == "msdata") {
    msdataint <- load_data(symbol=symbol1,intl=INTL,dec=dec) 
    FOUND <- TRUE
  } 
  if (source == "tq") {
      msdataint <- load_data2(symbol=symbol1,intl=INTL,dec=dec)
      FOUND <- TRUE      
  }
  
  if (FOUND == FALSE) {
    print(paste("Invalid data source ",symrow,symbol1,source))
    UNDECLARED() # makeshift stop script
  }
  ###############################################################
  
  
  
  # TODO advance date (and length) by ppb
  if (group1 == "actual") {
    date  <- request_data_l(msdataint=msdataint,type="date",bupts=0,len=clen+ppb)
    open  <- request_data_l(msdataint=msdataint,type="o",bupts=0,len=clen)
    high  <- request_data_l(msdataint=msdataint,type="h",bupts=0,len=clen)
    low   <- request_data_l(msdataint=msdataint,type="l" ,bupts=0,len=clen)
    close <- request_data_l(msdataint=msdataint,type="c",bupts=0,len=clen)
    mid   <- request_data_l(msdataint=msdataint,type="m",bupts=0,len=clen)
    vol   <- request_data_l(msdataint=msdataint,type="vol",bupts=0,len=clen) 
  }
  if (group1 == "ro") {
    date  <- request_data_l(msdataint=msdataint,type="date",bupts=0,len=clen+ppb)
    open  <- request_data_l(msdataint=msdataint,type="oro",bupts=0,len=clen)
    high  <- request_data_l(msdataint=msdataint,type="hro",bupts=0,len=clen)
    low   <- request_data_l(msdataint=msdataint,type="lro" ,bupts=0,len=clen)
    close <- request_data_l(msdataint=msdataint,type="cro",bupts=0,len=clen)
    mid   <- request_data_l(msdataint=msdataint,type="mro",bupts=0,len=clen)
    vol   <- request_data_l(msdataint=msdataint,type="vol",bupts=0,len=clen) 
  }
  if (group1 == "rpm") {
    date  <- request_data_l(msdataint=msdataint,type="date",bupts=0,len=clen+ppb)
    open  <- request_data_l(msdataint=msdataint,type="or",bupts=0,len=clen)
    high  <- request_data_l(msdataint=msdataint,type="hr",bupts=0,len=clen)
    low   <- request_data_l(msdataint=msdataint,type="lr" ,bupts=0,len=clen)
    close <- request_data_l(msdataint=msdataint,type="cr",bupts=0,len=clen)
    mid   <- request_data_l(msdataint=msdataint,type="mr",bupts=0,len=clen)
    vol   <- request_data_l(msdataint=msdataint,type="vol",bupts=0,len=clen) 
  }
  
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
  
  # TODO - check this !!!!!!!!!
  #pred  <- append(pred,tail(PREDTMP$predict,clen+ppb))
  pred <- PREDTMP$predict2
  
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
          #print(tmp$date)
          tmp$date <- substr(tmp$date,3,10)
          #print(tmp$date)        
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
  zz <- TTR::ZigZag( zzin, change = 0.08 ) # test was 0.08 for close

  lbdate <- df$date[nrow(df)-1]
  
  TITLE <- paste0(desc1," ", symbol1, " LB=", lbdate, " ", group1, " Zscore=", zscore)
  
  # Build up zscore annotation
  ann_lines <- paste(line1,lineh,linel,linec,linem,sep="<br>")
  #print(ann_lines)

  ########### bew trade triggers - OLD  
  #testing direction and buy/sell
  #buy <- ifelse(df$cpred > df$mpred, 1, 0)
  #sell <- ifelse(df$cpred < df$mpred, 1, 0)
  
  ########### bew direction ########################
  # TODO enhance and handle close near the open !!!!!!!!!
  dir <- sign(df$close - df$open)
  
  df <- df %>% mutate(dir) # add direction to df

  minmove <- 0.25
  dollars <- 12.50
  
  # TODO LOOP through methods and adjusts here 
  ############# TODO LOOP through methods and adjusts here  #############
  if (0) {
    source("tables.R")  
    #view(METHOD_LIST)
    
    for (i in seq(1:nrow(TRADE_METHOD_LIST)) ) {
      trade_method <- TRADE_METHOD_LIST$method[[i]]
      mdesc <- TRADE_METHOD_LIST$desc[[i]]
      en_bors <- TRADE_METHOD_LIST$en_bors[[i]]
      print(paste(i,trade_method,mdesc,en_bors))
      
      maxp <- tune_trades("MAXP",df,trade_method,en_bors,en_ordtype,ex_bors,ex_ordtype,hadja,ladja)
      minl <- tune_trades("MINL",df,trade_method,en_bors,en_ordtype,ex_bors,ex_ordtype,hadja,ladja)
    }
    
    do_trades(df,maxp)
    do_chart("filename")
    do_trades(df,minl)
    do_chart("filename")
  } ##### END methods
  
  
  ######################################
  ###### HIGHS - sells #################
  ######################################
  sell <- rep(NA,length(df$hpred)) # SELL markers
  
  hadj <- -0.08
  
  input <- df$hpred
  # do standardize
  m1 <<- mean(input)
  sd1 <<- std(input)
  output <- (input - m1) / sd1
  hpreda <- output + (hadj * output)
  input <- hpreda #undo standardize
  output <- input * sd1 + m1
  hpreda <- output  
  tmp <- floor(hpreda / minmove)
  hpreda <- tmp * minmove
  hpreda <- round(hpreda,dec)
  
  df <- df %>% mutate(hpreda)  
 
  # check sell
  #len <- length(df$high) - 1
  #for (i in 1:len) {
  #  if (df$high[i] > df$hpreda[i] && df$close[i] < df$hpreda[i] ) {
   #   sell[i] <- df$hpreda[i]
  #  }
  #}
  #sell[length(df$hpreda)] <- hpreda[length(df$hpreda)] # always use last marker
  
  ######################################
  ###### Lows - Buys ###################
  ######################################
  buy <- rep(NA,length(df$lpred)) # Buy markers

  ladj <- -0.04 
  input <- df$lpred
  # do standardize
  m1 <<- mean(input)
  sd1 <<- std(input)
  stand <- (input - m1) / sd1
  lpreda <- stand + (ladj * abs(stand))
  input <- lpreda #undo standardize
  output <- input * sd1 + m1
  lpreda <- output  
  tmp <- floor(lpreda / minmove)
  lpreda <- tmp * minmove
  lpreda <- round(lpreda,dec)
  
  #df <- df %>% select(-lpreda)
  df <- df %>% mutate(lpreda)  
  
  
  BUY <- "Buy"
  SELL <- "Sell"
  PS <- "ProtStop"
  ORDTYPE <- "Stop"
  method <- "M1"
  position <- 0 # plus = long  minus = short 
  tradenbr <- 0
  rundate <- as.character(Sys.Date())
  
  #source("tables.R") ################ TAKE OUT ##########
  tradew <- TRADE_LINES # trades - start with empty work file

  # TRADES - Stepping through bar by bar
  len <- length(df$low) - 1 # HAS NA at end for alignment
  
  # setp through bar by bar doing trades
  for (i in 1:len) {
    
    # tmp for now - will get replaaced soon
    #if (df$low[i] < df$lpreda[i] && df$close[i] > df$lpreda[i] ) {
      #buy[i] <- df$lpreda[i]
    #}
    
    #if (df$high[i] > df$hpreda[i] && df$close[i] < df$hpreda[i] ) {
      #sell[i] <- df$hpreda[i]
    #}
    
    # check current Position
    if (position == 0) { # No positions held 

      if (df$low[i] < df$lpreda[i] && df$close[i] > df$lpreda[i] ) { # Buy stop filled
        print(paste0("Entry: Buy Stop ",df$lpreda[i]," ",df$date[i]))
        buy[i] <- df$lpreda[i]
        position <- position + 1
        tradenbr <- tradenbr + 1
        
        tradew <- tradew %>%  
          add_row(
            symbol   = symbol1,
            rundate  = rundate,
            method   = method,
            hadj     = hadj,
            ladj     = ladj,
            tradenbr = tradenbr, 
            #entry
            endate  = df$date[i],  
            enbors   =  BUY,
            enord   = ORDTYPE,  
            enprice  = df$lpreda[i]
          )
      }
      
      if (df$high[i] > df$hpreda[i] && df$close[i] < df$hpreda[i] ) { # Sell stop filled
        print(paste0("Entry: Sell Stop ",df$hpreda[i]," ",df$date[i]))
        sell[i] <- df$hpreda[i]        
        position <- position - 1
        tradenbr <- tradenbr + 1  
        
        tradew <- tradew %>%  
          add_row(
            symbol   = symbol1,
            rundate  = rundate,            
            method   = method,
            hadj     = hadj,
            ladj     = ladj,
            tradenbr = tradenbr,            
            #entry
            endate  = df$date[i],  
            enbors   = SELL,
            enord   = ORDTYPE,  
            enprice  = df$hpreda[i]
          )
    
      }
    } # position == 0
    else if (position > 0) { # Currently LONG - look for sell limit exit
      if (df$high[i] > df$hpreda[i] && df$close[i] < df$hpreda[i] ) { # Sell stop filled
        #get entry
        enprice <- tradew$enprice[tradenbr]        
        exprice  = df$lpreda[i]
        expl <- exprice - enprice
        exdol <- expl * dollars          
        sell[i] <- df$hpreda[i]  
        
        #exit
        twlen = nrow(tradew)
        tradew$exdate[twlen]  = df$date[i]
        tradew$exbors[twlen]   = SELL
        tradew$exord[twlen]   = ORDTYPE
        tradew$exprice[twlen]  = exprice
        tradew$expl[twlen] = expl
        tradew$exdol[twlen] = exdol
        if (exdol > 0) tradew$wins[twlen] = 1
        else tradew$losses[twlen] = 1        
        
        #tradew <- tradew %>%  
        #  add_row(
        #    symbol   = symbol1,
        #    rundate  = rundate,            
        #    method   = method,
        #    hadj     = hadj,
        #    ladj     = ladj,
        #    tradenbr = tradenbr,            
            #exit
        #    exdate  = df$date[i],  
        #    exbors   = SELL,
        #    exord   = ORDTYPE,  
        #    exprice  = exprice,
        #    expl = expl,
        #    exdol = exdol    
        #  )
        position <- position - 1
        print(paste(df$date[i],"Exit: Sell Stop",df$lpreda[i],expl,exdol))         
      }
    } # end LONG
    else if (position < 0) { # Currently SHORT - look for buy stop exit
      if (df$low[i] < df$lpreda[i] && df$close[i] > df$lpreda[i] ) { # Buy stop filled
        #get entry
        enprice <- tradew$enprice[tradenbr]
        exprice  = df$lpreda[i]
        expl <- enprice - exprice 
        exdol <- expl * dollars           
        buy[i] <- exprice
        
        #exit
        twlen = nrow(tradew)        
        tradew$exdate[twlen]  = df$date[i]
        tradew$exbors[twlen]   = BUY
        tradew$exord[twlen]   = ORDTYPE
        tradew$exprice[twlen]  = exprice
        tradew$expl[twlen] = expl
        tradew$exdol[twlen] = exdol    
        if (exdol > 0) tradew$wins[twlen] = 1
        else tradew$losses[twlen] = 1
        
        #tradew <- tradew %>%  
        #  add_row(
        #    symbol   = symbol1,
        #    rundate  = rundate,            
        #    method   = method,
        #    hadj     = hadj,
        #    ladj     = ladj, 
        #    tradenbr = tradenbr,
            #exit
        #    exdate  = df$date[i],  
        #    exbors   =  BUY,
        #   exord   = ORDTYPE,  
        #    exprice  = exprice,
        #    expl  = expl,
        #    exdol =  exdol            
        #   )
          position <- position + 1
          print(paste(df$date[i],"Exit: Buy Stop ",df$lpreda[i],expl,exdol))          
      }
    } # end SHORT
  } # end bar by bar trade step though
  
  # TODO Now do summary will need for buys and sells for maxpl and minloss

  tradew <- tradew %>% mutate(wintot = cumsum(replace_na(wins,0)))
  tradew <- tradew %>% mutate(losstot = cumsum(replace_na(losses,0)))
  tradew <- tradew %>% mutate(pltot = cumsum(replace_na(expl,0)))
  tradew <- tradew %>% mutate(pldtot = cumsum(replace_na(exdol,0)))
  
  buy[length(df$lpreda)] <- lpreda[length(df$lpreda)] # always use last marker
  sell[length(df$hpreda)] <- hpreda[length(df$hpreda)] # always use last marker
  
  filename <- paste0("L2_",symbol1,"-trades.csv")
  write_csv(tradew, filename, append = FALSE, col_names = TRUE)
  
  ########## Trade summary ###########
  twlen = nrow(tradew) 
  trade_sum <- TRADE_SUMS
  trade_sum <- trade_sum %>%  
    add_row(
      symbol   = symbol1,
      rundate  = rundate,            
      method   = method,
      hadj     = hadj,
      ladj     = ladj, 
      pltot	   = tradew$pltot[twlen], 
      pldtot    = tradew$pldtot[twlen], 
      wintot   = tradew$wintot[twlen], 
      losstot  = tradew$losstot[twlen], 
    )
  
  filename <- paste0("L2_",symbol1,"-tradesum.csv")
  write_csv(trade_sum, filename, append = FALSE, col_names = TRUE)
  
  ########### END new triggers ###################
  
 # buy <- crossover(df$cpred, df$mpred)
  #sell <- crossunder(df$cpred, df$mpred)
  
  df <- df %>% mutate(buy)
  df <- df %>% mutate(sell)  
  #################################
  
  
  #print(paste0("openl=",length(df$open)," buyl1=",length(df$buy)))
  
  filename <- paste0("L2_",symbol1,"-atmp.csv")
  write_csv(df,filename,append = FALSE,col_names = TRUE)
  
  fig <- df %>% plot_ly(x = df$date, type="ohlc",
                        open = df$open, close = df$close,
                        high = df$high, low = df$low,name = "Price") 
  
  #fig <- fig %>% add_lines(x = df$date, y = df$hpred, name="hpred", line = list(color = 'black', width = 0.75), inherit = F) #legendgroup = 'group1', visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = df$hpreda, name="hpreda", line = list(color = 'purple', width = 1.00), inherit = F) 
  
  #fig <- fig %>% add_lines(x = df$date, y = df$lpred, name="lpred", line = list(color = 'black', width = 0.75), inherit = F) #, visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = df$lpreda, name="lpreda", line = list(color = 'purple', width = 1.00), inherit = F) 

  fig <- fig %>% add_lines(x = df$date, y = df$cpred, name="cpred", line = list(color = 'red',    width = 2.00), inherit = F, legendgroup = 'group1', visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = df$mpred, name="mpred", line = list(color = 'green',  width = 2.00), inherit = F, legendgroup = 'group1', visible = "legendonly")
  
  # test zigzag
  #fig <- fig %>% add_lines(x = df$date, y = zz, name="zz", line = list(color = 'black',  width = 2.25), inherit = F)
  
  # TODO buy/sell markers - TODO gibing warnings
  #fig <- add_markers(fig, x = df$date[df$sell == 1], y = df$hpred[df$sell == 1],
  #                   name = "Sell", marker = list(color = "red"))
  msize <- 10 # marker size
  fig <- add_markers(fig,
          x = df$date[!is.na(df$sell)],
          y = df$hpreda[!is.na(df$sell)],
          name = "Sell", marker = list(color = "red",size=msize))
  
  fig <- add_markers(fig,
           x = df$date[!is.na(df$buy)],
           y = df$lpreda[!is.na(df$buy)],
           name = "Buy", marker = list(color = "green",size=msize))
  
  #fig <- add_markers(fig, x = df$date[df$buy == 1], y = df$lpred[df$buy == 1],
  #          name = "Buy", marker = list(color = "green"))
  

 
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
    x=0.5, y=1.0, # align='bottom',
    xref = "paper", yref = "paper",
    showarrow = FALSE, font_size=24,
    text="https://larrycalvert.github.io/charts2/" 
    # enclose text in <b> text </b> for bold
  )
  
  fig <- fig %>% add_annotations(
    x=0.0, y=0.0,  align='left',
    xref = "paper", yref = "paper",
    showarrow = FALSE, font_size=24,
    text=ann_lines 
  )
  
  fig <- fig %>% plotly::layout(title = TITLE, showlegend = F, 
              hovermode = 'x',
              xaxis = list(type = 'category',rangeslider = list(visible = F)),
              yaxis = list(title = "Price"))
  
  suppressWarnings(print(fig))
  
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
  #fig5 <- subplot(fig, fig2, fig4, heights = c(0.8,0.1,0.1), nrows=3,
  #               shareX = TRUE, titleY = TRUE)
  
  fig5 <- fig
  
  fig5 <- fig5 %>% config(displayModeBar = TRUE, displaylogo = FALSE,
                        modeBarButtonsToRemove =
                        c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", 
                        "select", "autoscale", "lasso","hoverCompareCartesian",
                        "hoverClosest"))#, "toImage"))
  
  fig5 <- fig5 %>% plotly::layout(title = TITLE, showlegend = T,
                                  xaxis = list(title = "Date")) 
                                  #                              hovermode = 'x',
                                  #xaxis = list(type = 'category',rangeslider = list(visible = F)),
                                  #                              yaxis = list(title = "Price"))
  suppressWarnings(print(fig5))

  
  #PLOTLIST <- plotly_build(fig5) # test

  
  #plotly_build(fig5) # test
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
    #serverloc <- serverfile
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
  serverloc <- paste0("charts/",serverfile) # tmp copy for  index.html for now
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
  #print(paste0("Files in: ",serverloc))
  #print(files)
  } # symbol loop

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

    # Adding test menu
    cline <- paste0("git add menu.html menu.csv")
    #cline <- paste0("git add corn.html oats.html")
    retval <- system(cline, minimize=FALSE, wait=TRUE)
    print(paste0(cline, " retval=",retval))
    
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

############## END teng1.R #######################################
  