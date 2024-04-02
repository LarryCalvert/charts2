# Trade tune and predict Engine
# Clear R studio workspace, plots, and console
if (!is.null(dev.list())) dev.off()
cat("\014") # Clear console
rm(list=ls()) # Clean work space
if (0) sink("console.log", append=FALSE, split=TRUE) # for screen and log

#############################################################
VNAME <- "teng1"
VERSION <- "V0.28"
VDATE <- "02/02/2023"
VAUTHOR <- "Larry Calvert"

#############################################################
# 11/20/2023 Keeping only 1 tune method's orders nearing trade summary
# 11/05/2023 More tradesim and adj
# 10/14/2023  Trade Simulator & Trade Systems
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
L2ERRP  <- FALSE # PREDICT L2 error
DEBUG   <- FALSE

options(show.error.locations = TRUE)

LOGLEVEL <- "INFO" # set level - one of "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
DATADIR <- "c:\\futures\\working" #msdata file directory

# set our directory 
BASEDIR <- normalizePath(getwd())
LOGFILE <- paste0(BASEDIR, "\\teng.log")

##############  Packages ##########################################
if (!require("pacman")) install.packages("pacman")
#library("tidyr") # only used for drop_na in filters
pacman::p_load("conflicted","pracma","log4r","dplyr")

conflicts_prefer(dplyr::filter) #Todo check all refs and take out when done
conflicts_prefer(forecast::forecast)
conflicts_prefer(plotly::layout)

#library ("purrr") # required for drop_cols function
#library("itsmr") # test37 only want fft filter overides forecast
pacman::p_load("readr","lubridate","tibble")

#library("beepr")
#library("marima") # test multi arima
pacman::p_load("forecast","forecastHybrid","plotly","htmlwidgets","TTR")

# TODO future:  load as needed - forecastml and others
#library("fable")
#library("tidymodels")
#library("modeltime")
#library("timetk")
pacman::p_load("ggplot2","tidyr","KernSmooth","bestNormalize","Rlibeemd","spectral")

#library("chatgpt")
pacman::p_load("smooth","mFilter","itsmr","pracma","Metrics","tidyquant")

pacman::p_load("rjson","tidyjson","stringr") # for web scrape barchart.com

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

# current date
getCurrentDate <- function() {
  currentDate <- format(Sys.Date(), "%y%m%d")
  return(currentDate)
}

# Call the function to get the current date
rundate <- getCurrentDate()
#print(rundate)

######## TEST #################
#source("test.R") 
#############################

################ run scrape.py on barchart.com

# Need convert price function
convert_price <- function(price)
{
  parts <- strsplit(price,"-")
  part1 <- parts[[1]][1]
  part2a <- as.numeric(parts[[1]][2]) / 8
  part2b <- strsplit(as.character(part2a), "\\.")
  part2  <- tail(part2b[[1]], 1)
  converted <- paste0(part1,".",part2)
  #print(converted)
  return(converted)
}
  
bc_scrape <- function(symbolscrape = NULL) {
  #symbolscrape = "1WH24" # metastock notation
  symbolscrapem = str_replace(symbolscrape,'1','Z')
  
  cmdline = paste0("python ","./scrape.py ",symbolscrapem)
  #cat(cmdline)
  stat = system(cmdline)
  #if (stat == 0) {
  json_file = paste0(symbolscrapem,".json")
  json_data <- fromJSON(file=json_file)
  json_data2 <- json_data$scraped_text$data %>% spread_all
  #view(json_data2)
  #nrow(json_data2)
  date <- c()
  open <- c()
  high <- c()
  low <- c()
  close <- c()
  vol <- c()
  openint <- c()
  cnt <- c()    
  for(i in 1:nrow(json_data2)) {
    date[i] = json_data2$tradeTime[i]
    date[i] = format(as.Date(date[i], "%m/%d/%Y"), "%Y%m%d") 
    open[i] = convert_price(json_data2$openPrice[i])
    high[i] = convert_price(json_data2$highPrice[i])
    low[i]  = convert_price(json_data2$lowPrice[i])
    close[i] = convert_price(json_data2$lastPrice[i])
    vol[i] = json_data2$volume[i]
    openint[i] = json_data2$openInterest[i]      
    cnt[i] = i
  }
  prices <- tibble(open,high,low,close,vol,openint,date,cnt)
  #view(prices)
    
  prices_filename <- paste0(symbolscrape,".txt")
  write_csv(prices, prices_filename, append = FALSE, col_names = FALSE)  
}

# test
#symbol1 = "1WH24"
#bc_scrape(symbolscrape=symbol1)
#msdataint <- load_data(symbol=symbol1,intl=INTL,dec=2) 

if (0) {  
  #############################################
  pacman::p_load("httr","httr2","rvest","stringr")
  
  # ----------------------------
  #   from:
  #   https://cloud-tencent-com.translate.goog/developer/ask/sof/108061529?_x_tr_sl=zh-CN&_x_tr_tl=en&_x_tr_hl=en&_x_tr_pto=sc
  #   
  #   https://cloud.tencent.com/developer/ask/sof/108061529
  #   
  #   The token needs to be sent as a named x-xsrf-tokenrequest header, not by passing into parameters:
  #     Also, the token value may change across sessions, so you need to get it in a cookie. 
  #   After this, convert the data into data frame and get the result:
  #  Mod
  geturl='https://www.barchart.com/futures/quotes/ZWH24/price-history/historical'
  apiurl='https://www.barchart.com/proxies/core-api/v1/historical/get'
  
  # insert
  # https://bookdown.org/f_lennert/workshop-ukraine/advanced-rvest.html
  user_a <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 12_0_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36")
  pg <- session(geturl, user_a)
  pg$response$request$options$useragent
  cookies <- pg$response$cookies
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, cookies$name)))
  
  pg <- 
    pg %>% GET(apiurl,
             config = httr::add_headers(`x-xsrf-token` = token)
    )
  data_raw <- httr::content(pg$response)
  data <- 
    purrr::map_dfr(
      data_raw$data,
      function(x){
        as.data.frame(x$raw)
      }
    )
  
  
  pg <- html_session(geturl)
  # failing - header ????
  
  cookies <- pg$response$cookies
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, cookies$name)))
  pg <- 
    pg %>% rvest:::request_GET(apiurl,
      config = httr::add_headers(`x-xsrf-token` = token)
    )
  data_raw <- httr::content(pg$response)
  data <- 
    purrr::map_dfr(
      data_raw$data,
      function(x){
        as.data.frame(x$raw)
      }
    )
  
  # httr2 approach
  geturl='https://www.barchart.com/futures/quotes/ZWH24/price-history/historical'
  apiurl='https://www.barchart.com/proxies/core-api/v1/historical/get'
  
  req <- request(geturl) %>% req_headers(
    "referer"="https://www.barchart.com/futures/quotes/ZWH24/price-history/historical",
    'user-agent'= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.119 Safari/537.36'
  )
  #req |> req_dry_run() # show req for testing 

  #resp <- req |> req_perform()
  resp <- req %>% req_perform()
  resp
  t1 <- resp_headers(resp, filter="Set-Cookie") # want XSRF-TOKEN
  t1
  
  t2 <- t1[[2]]

  ####### TOKEN ###################
  # Your input string
  input_string <- t2
  
  # Remove "XSRF-TOKEN=" from the string
  modified_string <- gsub("XSRF-TOKEN=", "", input_string, fixed = TRUE)
  
  # Truncate to the first semicolon
  modified_string <- substr(modified_string, 1, regexpr(";", modified_string) - 1)
  
  # Print the result
  print(modified_string)
  #t3 <- paste0("'", modified_string, "'")
  t3 <- paste0("'",modified_string,"'")
  
  req2 <- request(apiurl) %>% req_headers(
    'accept' = 'application/json',
    'accept-encoding' = 'gzip, deflate, br',
    'accept-language' = 'en-US,en;q=0.9',
    'user-agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.85 Safari/537.36',
    'x-xsrf-token' = t3
    ) %>% req_body_json(list(
    "symbol" = "ZWH24",
    "fields" = "tradeTime.format(m/d/Y),openPrice,highPrice,lowPrice,lastPrice,volume,openInterest",
    "type" = "eod",
    "orderBy" = "tradeTime",
    "orderDir" = "desc",
    "limit" = "100",
    "raw" = "1"
    )
  )
  
  #req2 |> req_dry_run() # show req for testing
  req2 %>% req_dry_run() # show req for testing 
  
  #resp2 <- req2 |> req_perform()
  resp2 <- req2 %>% req_perform()
  
  # https://stackoverflow.com/questions/73774349/scrape-live-updating-value-from-html-in-r
  # https://stackoverflow.com/questions/63313383/rvest-wont-return-data
  
  # URL to scrape
  url <- "https://www.barchart.com/futures/quotes/ZWH24/price-history/historical"

  html <- read_html(url) # read webpage

  t1 <- html %>% html_elements(xpath="//div")
  
  t1 <- html %>% html_element(xpath = '//script[@id="bc-dynamic-config"]')
  
  t2 <- html %>% html_nodes(xpath='//*[@id="main-content-column"]/div')
 
  t2 <- html %>% html_nodes(xpath='//*[@id="main-content-column"]/div/*')
 
  t2 <- html %>% html_nodes(xpath='//*[@id="main-content-column"]/div/div[4]')
  
  t3 <- t2 %>% html_table()
  
  print(t3)
  
  # now for the table
  t1 <- html %>%
    html_element(xpath = '//script[@id="bc-dynamic-config"]') %>%
    html_text() %>%
    jsonlite::parse_json()
  
  sink("t1.txt")
  print(t1)
  sink()
  
  
  ######## test only
  # url <- "https://www.barchart.com/proxies/core-api/v1/historical/get"
  url <- "https://www.barchart.com/futures/quotes/ZWH24/price-history/historical"
  
  html <- read_html(url) # read webpage
  
  t1 <- html %>%
    html_element(xpath = '//script[@id="bc-dynamic-config"]') %>%
    html_text() %>%
    jsonlite::parse_json() %>%
    getElement('currentSymbol') %>%
    getElement('lastPrice')
  print(t1)
  
  t1 <- html %>% html_nodes("div") %>% 
    html_text()
  
  t1 <- html %>%
    html_element(xpath = '//script[@id="bc-dynamic-config"]') %>%
    html_text() %>%
    jsonlite::parse_json()
  
  t2 <- t1 %>% getElement('highPrice')
  
  t2 <- t1 %>% html_nodes("highPrice")
    
  sink(file="t1b.txt")
  print(t1)
  sink()
  
  
  t2 <- t1 %>% getElement('currentSymbol')
  # print(t2)
  t3 <- t2 %>% getElement('highPrice')
  print(t3)
  
  
    #t2 <- html %>% html_nodes(xpath='//*[@id="main-content-column"]/div/div[4]')
  
    t2 <- html %>% html_nodes(xpath='//*[@id="/ZWH24/price-history/historical"]')
    print(t2)
    
    t2 <- t1 %>% getElement('currentSymbol')
    
    #%>% getElement('lastPrice')
  print(t1)
  
  # %>%
  #   getElement('currentSymbol') %>%
  #   getElement('lastPrice')
  # print(t1)
  
  # works for lastPrice
  t1 <- html %>%
    html_element(xpath = '//script[@id="bc-dynamic-config"]') %>%
    html_text() %>%
    jsonlite::parse_json() %>%
    getElement('currentSymbol') %>%
    getElement('lastPrice')
  print(t1)
  
  #https://stackoverflow.com/questions/73774349/scrape-live-updating-value-from-html-in-r
  read_html(url) %>%
    html_element(xpath = '//script[@id="bc-dynamic-config"]') %>%
    html_text() %>%
    jsonlite::parse_json() %>%
    getElement('currentSymbol') %>%
    getElement('Lastprice')

  
  pacman::p_load("RSelenium","magrittr")
  
  # Open firefox and extract source
  rD <- rsDriver(browser = "chrome", verbose = TRUE)
  remDr <- rD[["client"]]
  remDr$navigate(URL)
  html <- remDr$getPageSource()[[1]]
  
  # Extract table from source
  DF <- read_html(html) %>% 
    html_nodes("table") %>% 
    `[[`(3) %>% 
    html_table %>% data.frame
  
  # Close connection
  remDr$close()
  
  #-------------------------------------------
  # https://github.com/abhimotgi/dataslice/blob/master/R/Web%20Scraping%20with%20RVest%20Part%204.R
  page = read_html(url)
  
  tstr <- "bc-datatable ng-isolate-scope"
  #nodes <- page %>% html_element(tstr)
  nodes <- page %>% html_node(tstr)
  print(nodes)
  
  t1 <- nodes %>% .[1]
  #table <- t1 %>% html_table(fill = TRUE)
  print(t1)
  #print(table)
  
  #table = page %>% html_nodes("table") 
      # %>% .[2] %>% 
    #html_table(fill = TRUE) %>% .[[1]]
  #view(table)
  
  
  webpage <- read_html(url)
  table_nodes <- html_nodes(webpage, "table")
  price_table <- html_table(table_nodes)[[2]]
  table_nodes
  html_table(table_nodes)[[1]]
  html_table(table_nodes)[[2]]
  html_table(table_nodes)[[3]]
  price_table
  
  
# close getting tables - where is OHLC?
  data <- url %>% 
    read_html() %>%
    html_elements("table") %>% 
    html_table()
  
  
  data <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table() %>%
    as_tibble()
  
  
  webpage <- read_html(url)
  
  # Sending GET request
  req <- request(url)
  #req |> req_dry_run()
  req %>% req_dry_run()  
}
################################################
               
               
#TODO LRC get date format right
# check date format
#msdataint <- load_data(symbol="1WZ2",intl=INTL,dec=2)
#view(head(msdataint$date,n=10))
#view(head(msdataint$datet,n=10))

#################################################
####### L1_predict_main        ##################
#################################################

# TODO add menu here

# structure returned from trade_simulator
# TODO: will get moved to tables
TRADE_SUMMARY_LINE <- tibble(
  # add run date
  symbol    = character(),
  tsys      = character(),
  tsrow     = integer(),
  bsc       = character(),
  hadj      = double(),
  ladj      = double(),
  win       = integer(),
  lose      = integer(),   
  plmm      = double(),    # P/L
  pldol     = double(),
  riskmm    = double(),  # risk buy/sell min. all in minmove
  pos       = integer(),
)

# create best summary file if it doesn't exist
BEST_SUMMARY <- TRADE_SUMMARY_LINE 
best_sum_filename <- paste0("$BEST.csv")
#if (file.exists(best_sum_filename) == FALSE) {
  write_csv(BEST_SUMMARY, best_sum_filename, append = FALSE, col_names = TRUE)  
#}


L1_DETAIL_FILE <- "$Detail.csv"
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
  if (source == "bc") { # webscrape barchart.com
    bc_scrape(symbolscrape = symbol1)
    msdataint <- load_data(symbol=symbol1,intl=INTL,dec=dec) 
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
  #write_csv(L1_SCORE1, L1_SCORE1_FILE, append = FALSE, col_names = TRUE) # tmp take out
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
  #write_csv(L1_SCORE2,L1_SCORE2_FILE,append = FALSE,col_names = TRUE)
  
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
  #write_csv(L1_SCORE3, L1_SCORE3_FILE, append = FALSE,col_names = TRUE) # take out col-names for append

  ##################################################################
  # select best score (lowest) by Symbol just sort L1_score4
  
  L1_SCORE4_FILE <- "L1_score4.csv"

 
  L1_SCORE4 <- L1_SCORE3 %>% dplyr::arrange(zscore)
  #view(L1_SCORE4)
  #write_csv(L1_SCORE4, L1_SCORE4_FILE, append = FALSE,col_names = TRUE)
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
  L1_SCORE5_FILE <- paste0(symbol1,"-score5.csv")
  #view(L1_SCORE5)
  write_csv(L1_SCORE5, L1_SCORE5_FILE, append = FALSE,col_names = TRUE)
  #####################################################################
  
  #score6 arrange score5 by zscore to get best symbol list
  L1_SCORE6 <- L1_SCORE3_LINE  
  L1_SCORE6 <- L1_SCORE5 %>% dplyr::arrange(zscore)
  
  L1_SCORE6_FILE <- "L1_score6.csv"
  #view(L1_SCORE6)
  #write_csv(L1_SCORE6, L1_SCORE6_FILE, append = FALSE,col_names = TRUE)
  
  
  
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
filename <- paste0(symbol1,"-predtab.csv")
write_csv(PREDTAB,filename,append = FALSE,col_names = TRUE) # take out col-names for append

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
  if (DEBUG) print(line_symbol)
  
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
  #print(paste("Predicting error for",symbol1,dt1,pproc1,method1))
  #log4r::debug(logger, paste( logtitle,"Predicting error for",symbol1,dt1,pproc1,method1))

  ########### L2 ERROR PREDICT #########################
  pred2 <- scr$predict
  
  if (L2ERRP == TRUE ) {
    # test filtered
    #f1 <- filter_func(in1,"mstl") #zlma
    f1 <- filter_func(in1,"zlma") #zlma
    
    pred2a <- predict2(f1, ptype1="arma", ptype2="nn", chart = FALSE)
  
    # was this one - using arma for speed for dev right now
    #pred2a <- predict2(in1, ptype1="hybridcv", ptype2="none", chart = FALSE) # slow
   
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
  
  #print(paste("L2",symbol1,"-",dt1,"zscore1=",zscore1,"zscore2=",zscore2))
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
  if (source == "bc") {
    msdataint <- load_data(symbol=symbol1,intl=INTL,dec=dec) 
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
      if (DEBUG) print(lastdate)
      datet <- append(datet,lastdate)
      cnt <= cnt + 1
    }
  }
  #view(datet)  
  
  test2 <- datesv + days(1) # advance to next day
  if (DEBUG) print(wday(test2, label=TRUE))
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
  
  ######### adjust prediction by adj and round to minmoves
  pred_adj <- function(pred=NULL,adj=NULL,minmove=NULL)
  {
    input <- pred
    # do standardize
    m1 <<- mean(input)
    sd1 <<- std(input)
    predz <- (input - m1) / sd1
    preda <- predz + (adj * predz)
    tmp <- preda * sd1 + m1 #undo standardize
    tmp2 <- floor(tmp / minmove)
    tmp3 <- tmp2 * minmove
    preda <- round(tmp3,dec)
    return(preda)
  }
  
  adj <- +0.30 
  hpreda1 <- pred_adj(pred=df$hpred, adj=adj, minmove=minmove)
  df <- df %>% mutate(hpreda1)
  adj <- -0.30 
  hpredb1 <- pred_adj(pred=df$hpred, adj=adj, minmove=minmove)
  df <- df %>% mutate(hpredb1)
  #write.csv(df,file="tmp.csv")
  
  # TODO - move to tables.r when finished
  
  ######## Methods and trades #######################################

  # create trades
  TRADE_LINES <- tibble(
    symbol    = character(),
    rundate  = character(),
    method   = character(),
    hadj     = double(),
    ladj     = double(),  
    tradenbr = numeric(), 
    #entry
    endate    = character(), #Sould be date but de-interplate must be returning char
    enbors    = character(),
    enord     = character(),  
    enprice   = double(),
    #exit
    exdate   = character(),
    exbors   = character(),
    exord   = character(),  
    exprice   = double(),
    expl     = double() ,  # Profit/loss in minmove --  to get dollars times by symbol's dollars
    exdol      = double(),
    wins      = numeric(),
    losses   = numeric()
  )
  
  # trade summary buy sell 
  TRADE_SUMS <- tibble(
    symbol    = character(),
    rundate  = character(),
    method   = character(),
    hadj     = double(),
    ladj     = double(),  
    enbors    = character(),
    pltot	    = double() ,  # Profit/loss in minmove --  to get dollars times by symbol's dollars
    pldtot    = double(),
    wintot    = numeric(),
    losstot   = numeric() 
  )
  
  ###############################################
  cat("###############################################\n")
  cat("########## New Trade Simulator ################\n")
  cat("###############################################\n")

  ORDER_LINE <- tibble(
    symbol    = character(),
    tsys      = character(),    
    hadj      = double(),
    ladj      = double(),     
    bsc       = character(),    
    date      = character(),
    dir       = double(),
    bors      = character(), # Buy or Sell     
    mls       = character(), # Market, Limit, Stop
    type      = character(), # Entry, Protstop, Target
    pos       = integer(), 
    price     = double(),
    filled    = logical(),
    quant     = integer(), 
    riskmm    = double(),
    #txt       = character(), # describe for user    
    #open      = double(),
    #high      = double(),
    #low       = double(),
    #close     = double() 
  )
  
  TRADE_LINE <- tibble(
    symbol    = character(),
    tsys      = character(),    # maybe rownbr into method tables ?????
    tsrow     = integer(),    
    hadj      = double(),
    ladj      = double(),   
    endate    = character(),    
    enbors    = character(), # Entry    
    enprice   = double(),
    entype    = character(),
    enquant   = integer(),
    psprice   = double(), # initial protective stop
    riskmm    = double(),  # psprice - psprice - entry risk as offset from entry price in minmove    
    #riskmm    = double(),    # risk - offset from entry
    #riskdol   = double(),    # risk in dollars
    exdate    = character(),     
    exbors    = character(), # Exit
    exprice   = double(),
    extype    = character(), # exit type PSI, PSA, Target    
    plmm      = double(),    # P/L
    pldol     = double(),
    win       = integer(),
    lose      = integer(),
    pos       = integer(),    
    #bint      = integer()    # bars in trade
    bars      = integer()    # bars in trade    
  )
  

  place_order <- function(orders=NULL,symbol=NULL,tm=NULL,hadj=NULL,ladj=NULL,
                    bar=NULL,bors=NULL,mls=NULL,type=NULL,position=NULL,quant=NULL,price=NULL)
  {

    # Create text
    txt = "place_order Fix this ???????"
    
    if (bors == "Buy" && mls == "Stop") {
      txt = paste("If price drops below",price,"place a buy",
                    quant, "stop order at",price)
    }
    if (bors == "Sell" && mls == "Stop") {
      txt = paste("If price rises above",price,"place a sell",
                   quant, "stop order at",price)
    }

    #print(paste("   place_order", type, bors, mls, "q=", quant, price, txt))
    if (DEBUG) print(paste("  ORDER:", type, txt))
    
    orders <- orders %>%  
      add_row(
        symbol  = symbol,
        tsys    = tm$name,
        hadj    = hadj,
        ladj    = ladj,        
        bsc     = tm$bsc,
        date    = bar$date,
        dir     = bar$dir,
        price   = price,        
        bors    = bors,
        mls     = mls,
        type    = type,
        pos     = position,
        quant   = quant,
        filled  = FALSE
        #txt    = txt,
        #open    = bar$open,
        #high    = bar$high,
        #low     = bar$low,
        #close   = bar$close,        
      )
     return(orders)
  }
  
  # orders are omly those orders to be checked
  # trades is the complete TRADE list 2 dim matrix (row, col) as we modify an existing row to add exit
  # NOTE: after entry place Target order then PSA order
  check_filled <- function(position=NULL,orders=NULL,trades=NULL,bar=NULL)
  {
    if (is.na(bar$high)) return(list(position,orders,trades)) # skip if future bar predict
    
    #print(paste("check_filled() start","orders=",nrow(orders),"trades=",nrow(trades)))
    nbrtrades <- nrow(trades)
    
    # Now we check entry
    for (orow in 1:nrow(orders)) { # will be more than 1 entry order if Comb
      order = orders[orow, ] # get order
      
      if (order$type == "Entry" && order$bors == "Buy" && order$mls == "Stop") {
        if (order$price < bar$close && order$price > bar$low) { # filled
          # starts a new trade              
          position = position + order$quant # adjust position Buy  
          orders$pos[orow] = position          
          orders$filled[orow] = TRUE # set filled - thanks chatGPT for format
          riskmm  = round( (order$price - bar$low) / minmove, 2)
          orders$riskmm[orow] = riskmm
          if (DEBUG) print(paste( "  FILLED:", order$type, order$bors, order$quant, order$price))
          trades <- trades %>%  # Starts a new trade
          add_row(
            symbol  = order$symbol,
            tsys    = order$tsys,
            hadj    = order$hadj,
            ladj    = order$ladj,
            endate  = order$date,
            enbors  = order$bors,
            enprice = order$price,
            entype  = order$type,
            enquant = order$quant,
            riskmm  = riskmm,
            pos     = position,
            bars    = 1
          )
        } # filled
      } # ENTRY Buy Stop
        
      if (order$type == "Entry" && order$bors == "Sell" && order$mls == "Stop") {
        if (order$price > bar$close && order$price < bar$high) { # filled
           # starts a new trade
          position = position - order$quant # adjust position (Sell)
          orders$pos[orow] = position
          orders$filled[orow] = TRUE # set filled - thanks chatGPT for format
          riskmm  = round( (bar$high - order$price) / minmove,2)
          orders$riskmm[orow] = riskmm
          if (DEBUG) print(paste( "  FILLED:", order$type, order$bors, order$quant, order$price))
          trades <- trades %>%  # Starts a new trade
          add_row(
            symbol  = order$symbol,
            tsys    = order$tsys,
            date    = order$date,
            hadj    = order$hadj,
            ladj    = order$ladj,
            enbors  = order$bors,
            enprice = order$price,
            entype  = order$type,
            enquant = order$quant,            
            riskmm  = riskmm,
            pos     = position,            
            bars    = 1
          )
        } # filled
      } # ENTRY Sell Stop
      
      # initial protective stop - calc risk and save in trade along with price
      # should never be hit as we have fore knowledge of high and low over tune set
      if (order$type == "PSI") {
        trades[nbrtrades,"psprice"] <- order$price  # save off initial prot stop  - thanks chatGPT      
        #print(paste("Processing fill PSI",order$bors, order$price ))
#        trades[nbrtrades,"riskmm"]  <- abs(trades[nbrtrades,"enprice"] - trades[nbrtrades,"psprice"]) # risk - offset from entry
#        trades[nbrtrades,"riskdol"] <- (trades[nbrtrades,"riskmm"] / minmove) * dollars # risk in dollars
        #trades[nbrtrades,"enrisk"]  <- abs(trades[nbrtrades,"enprice"] - trades[nbrtrades,"psprice"]) # risk - offset from entry
      } # PSI
      
      target_reached = FALSE
      
      # Target - add check hit logic
      if (order$type == "Target") {
        #print(paste("Processing fill Target",order$bors, order$price ))
        if (trades[nbrtrades,"enbors"] == "Buy" && order$bors == "Sell" &&
            order$price < bar$high && bar$close <= order$price) { # Target reached and triggered 
          target_reached = TRUE
          position = position - order$quant
          orders$pos[orow] = position          
          orders$filled[orow] = TRUE # set filled - thanks chatGPT for format          
          trades[nbrtrades,"pos"] <- position          
          trades[nbrtrades,"exdate"]  <- bar$date
          trades[nbrtrades,"exbors"]  <- order$bors
          trades[nbrtrades,"exprice"] <- order$price
          trades[nbrtrades,"extype"]  <- order$type           
          trades[nbrtrades,"plmm"]    <- order$price - trades[nbrtrades,"enprice"] # Profit
          trades[nbrtrades,"pldol"]   <- (trades[nbrtrades,"plmm"] / minmove) * dollars # Profit in dollars
          trades[nbrtrades,"win"] <-0
          trades[nbrtrades,"lose"] <-0
          trades[nbrtrades,"pos"] <- position          
          if (trades[nbrtrades,"pldol"] > 0) trades[nbrtrades,"win"] <-1
          if (trades[nbrtrades,"pldol"] < 0) trades[nbrtrades,"lose"] <-1  
          if (DEBUG) print(paste( "  FILLED:", order$type, order$bors, order$price, "$",trades[nbrtrades,"pldol"]))
          if (DEBUG) print(paste( "  TRADE COMPLETE:",
                       trades[nbrtrades,"endate"], 
                       trades[nbrtrades,"enbors"],
                       trades[nbrtrades,"enquant"],
                       trades[nbrtrades,"enprice"],
                       trades[nbrtrades,"exdate"],
                       trades[nbrtrades,"exbors"],
                       trades[nbrtrades,"exprice"],
                       "$",trades[nbrtrades,"pldol"],
                       trades[nbrtrades,"win"],
                       trades[nbrtrades,"lose"]))
          #position = position - order$quant
        }
      } # Target
      
      if (order$type == "PSA") {
        #print(paste("Processing fill PSA",target_reached,order$bors, order$price))
        # trades[nbrtrades,"enbors"] == "Buy"
        if (target_reached == FALSE && order$bors == "Sell" && order$type == "Stop" &&
            bar$low <= order$price && bar$close <= order$price) { # PROT STOP reached and triggered 
          position = position - order$quant
          orders$pos[orow] = position      
          orders$filled[orow] = TRUE # set filled - thanks chatGPT for format
          trades[nbrtrades,"pos"] <- position            
          trades[nbrtrades,"exdate"]  <- bar$date
          trades[nbrtrades,"exbors"]  <- order$bors
          trades[nbrtrades,"exprice"] <- order$price
          trades[nbrtrades,"extype"]  <- order$type          
          trades[nbrtrades,"plmm"]    <- order$price - trades[nbrtrades,"enprice"] # Profit
          trades[nbrtrades,"pldol"]   <- (trades[nbrtrades,"plmm"] / minmove) * dollars # Profit in dollars
          trades[nbrtrades,"win"] <-0
          trades[nbrtrades,"lose"] <-0
          trades[nbrtrades,"pos"] <- position          
          if (trades[nbrtrades,"pldol"] > 0) trades[nbrtrades,"win"] <-1
          if (trades[nbrtrades,"pldol"] < 0) trades[nbrtrades,"lose"] <-1          
          if (DEBUG) print(paste( "  FILLED:", order$type, order$bors, order$price, "$",trades[nbrtrades,"pldol"]))
          if (DEBUG) print(paste( "  TRADE COMPLETE:",
                       trades[nbrtrades,"endate"], 
                       trades[nbrtrades,"enbors"],
                       trades[nbrtrades,"enquant"],
                       trades[nbrtrades,"enprice"],
                       trades[nbrtrades,"exdate"],
                       trades[nbrtrades,"exbors"],
                       trades[nbrtrades,"exprice"],
                       "$",trades[nbrtrades,"pldol"],
                       trades[nbrtrades,"win"],
                       trades[nbrtrades,"lose"]))          
          #position = position - order$quant          
        }
      } # PSA
    } # end orders
    #print(paste("check_filled() end  ", "orders=",nrow(orders),"trades=",nrow(trades)))
    return(list(position,orders,trades))
  } 
  

  # TRADE methods
  ##############################
  # all orders stop orders - keeps Prot stop at entry low/buy or high/sell if multi-bar
  # This required some thought
  # ORDERS are our complete list of orders for this trading method run across the tune set - only added to
  # orders will be list of orders for this one bar and check filled uses this
  # TRADES are our complete list of orders - the last one is created on a new trade and modified when
  #   we exit that trade
  m1 <- function(ORDERS=NULL,TRADES=NULL,symbol=NULL,tm=NULL,df=NULL, rownbr=NULL, preda=NULL, hadj=NULL,
                 ladj=NULL,position=NULL, desc=NULL)  
  {

    orders = ORDER_LINE # limited to this 1 bar

    drows  <- nrow(df)
    bar    <- df[rownbr, ]  # Get the current row
    dir    <- bar$dir # bar direction
    #date   <- df$date[rownbr]
    hpreda <- preda$hpreda[rownbr]
    lpreda <- preda$lpreda[rownbr]  
    cpreda <- preda$cpreda[rownbr]
    mpreda <- preda$mpreda[rownbr]
    
    open   <- df$open[rownbr]
    high   <- df$high[rownbr]
    low    <- df$low[rownbr]
    close  <- df$close[rownbr]
    mid    <- df$mid[rownbr]

    if (DEBUG) cat("\n")
    if (DEBUG) cat(paste(symbol,df$date[rownbr],tm$name,tm$bsc,hadj,ladj,"start",
                    "pos=",position, "orders=",nrow(ORDERS), "trades=",nrow(TRADES),"\n"))
    #if (DEBUG) print(paste(" ","OHLCM", open, high, low, close, mid))
    
    # Entries    
    if (position == 0) { # not in a trade - try for entry
      if (tm$bsc == "Buy") {
         orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                      bar=bar,bors="Buy",mls="Stop",type="Entry",position=position,quant=1,price=lpreda)
      }
      if (tm$bsc == "Sell") {
        orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                      bar=bar,bors="Sell",mls="Stop",type="Entry",position=position,quant=1,price=lpreda)
      }
      if (tm$bsc == "Comb") {
        orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                      bar=bar,bors="Buy",mls="Stop",type="Entry",position=position,quant=1,price=lpreda)
        orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                      bar=bar,bors="Sell",mls="Stop",type="Entry",position=position,quant=1,price=lpreda)
      }      

      TSR <- check_filled(position=position,orders=orders,trades=TRADES,bar=bar)
      position <- TSR[[1]] #tried to return a tibble - couldn't get it to work
      orders   <- TSR[[2]]
      TRADES   <- TSR[[3]]
      ORDERS <- ORDERS %>% add_row(orders) # save ORDERS
      
      # if filled  Initial Prot stop  initial target      
      if (position != 0) { 
        #print(paste(df$date[rownbr],"Entry filled - setting initial ProtStop and target"))
        orders = ORDER_LINE # reset orders
        
        if (position > 0) {
           orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                          bar=bar,bors="Sell",mls="Stop",type="Target",position=position,quant=1,price=hpreda) 
           orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                          bar=bar,bors="Sell",mls="Stop",type="PSI",position=position,quant=1,price=bar$low)           
        }
        if (position < 0) {
          orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                          bar=bar,bors="Buy",mls="Stop",type="Target",position=position,quant=1,price=lpreda) 
          orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                          bar=bar,bors="Buy",mls="Stop",type="PSI",position=position,quant=1,price=bar$high)
        }
        # check filled for prot stop and target
        TSR <- check_filled(position=position,orders=orders,trades=TRADES,bar=bar)
        position <- TSR[[1]] #tried to return a tibble - couldn't get it to work
        orders   <- TSR[[2]]
        TRADES   <- TSR[[3]]
        ORDERS <- ORDERS %>% add_row(orders) # save ORDERS        
      }
    } # end position == 0
    else { # position <> 0 - in a trade
      # now place prot stop and target orders ProtSTop moved to predicted low 
      #print(paste(df$date[rownbr],"Holding a position - setting new ProtStop and target"))
      TRADES$bars[nrow(TRADES)] <- TRADES$bars[nrow(TRADES)] + 1# get current trade
      if (position > 0) {
        orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                      bar=bar,bors="Sell",mls="Stop",type="Target",position=position,quant=1,price=hpreda)
        if (lpreda < TRADES$enprice[nrow(TRADES)]) {
          orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                    bar=bar,bors="Sell",mls="Stop",type="PSA",position=position,quant=1,price=TRADES$enprice[nrow(TRADES)]) 
        }
        else {
          orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                bar=bar,bors="Sell",mls="Stop",type="PSA",position=position,quant=1,price=lpreda)
        }
      }
      if (position < 0) {
        orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                      bar=bar,bors="Buy",mls="Stop",type="Target",position=position,quant=1,price=lpreda)
        if (hpreda > TRADES$enprice[nrow(TRADES)]) {
          orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                                bar=bar,bors="Sell",mls="Stop",type="PSA",position=position,quant=1,price=TRADES$enprice[nrow(TRADES)]) 
        }
        else {
          orders <- place_order(orders=orders,symbol=symbol,tm=tm,hadj=hadj,ladj=ladj,
                      bar=bar,bors="Buy",mls="Stop",type="PSA",position=position,quant=1,price=hpreda)
        }
      }
      
      TSR <- check_filled(position=position,orders=orders,trades=TRADES,bar=bar)
      position <- TSR[[1]] #tried to return a tibble - couldn't get it to work
      orders   <- TSR[[2]]
      TRADES   <- TSR[[3]]
      ORDERS <- ORDERS %>% add_row(orders) # save ORDERS 
    }

    if (DEBUG) cat(paste(symbol,df$date[rownbr],tm$name,tm$bsc,hadj,ladj,"end   ",
                    "pos=",position, "orders=",nrow(ORDERS), "trades=",nrow(TRADES)))
    return(list(position,ORDERS,TRADES))
  }
  

  TRADE_METHOD_LIST <- tribble(
    ~func, ~name, ~bsc,  ~desc,
    m1, "M1", "Buy",  "no change protective stop",
    #m1, "M1", "Sell",  "no change protective stop",
    #m1, "M1", "Comb",  "no change protective stop",    

    #m2, "Buy",  "M2 move protective stop to previous low",
    #m2, "Sell", "M2 move protective stop to previous low",
    #m2, "Comb", "M2 move protective stop to previous low",
    
    #m3, "Buy",  "M2 move protective stop to predicted mid",
    #m4, "Buy",  "M2 move protective stop to predicted low",    
  )
  

  # run a trade system with these parms and create a summary
  # Does not keep ORDERS and TRADES unless we write it to a file
  # needs to return Trade summary (used to find the best)
  # desc <- TRUE/FALSE make FalSE during tuning but TRUE during final best run
  # tsrow is row nbr in TRADE_METHOD_LIST
  run_trade_system <- function(symbol=NULL,tsrow=NULL, df=NULL, hadj=NULL,ladj=NULL)
  {
    
    tm <- TRADE_METHOD_LIST[tsrow, ] # get that selected row 
    bsc <- tm$bsc[tsrow]
    desc <- tm$desc[tsrow]
    name <- tm$name[tsrow]
    
    #TODO move to end and show p/l also
    #print(paste(symbol,name,bsc,hadj,ladj))
    
    ##### now create adjusted predicts using hadj and ladj
    hpreda <- pred_adj(pred=df$hpred, adj=hadj, minmove=minmove)
    lpreda <- pred_adj(pred=df$lpred, adj=ladj, minmove=minmove)
    cpreda <- pred_adj(pred=df$cpred, adj=0, minmove=minmove)
    mpreda <- pred_adj(pred=df$mpred, adj=0, minmove=minmove)
    preda  <- tibble(hpreda,lpreda,cpreda,mpreda)

    ORDERS <- ORDER_LINE
    TRADES <- TRADE_LINE
   
    position = 0 #Start with not holding a position
    
    # this steps through bar by bar:
    for (rownbr in 1:nrow(df)) { # LRC LRC NBR of bars 1:5 to limit
      TSR <- tm$func[[1]](ORDERS=ORDERS,TRADES=TRADES,symbol=symbol, tm=tm, df=df, rownbr=rownbr, 
                   preda=preda, hadj=hadj, ladj=ladj, position=position)

      position <- TSR[[1]] #tried to return a tibble - couldn't get it to work
      ORDERS   <- TSR[[2]]
      TRADES   <- TSR[[3]]
    }

    if (DEBUG) cat("\n")
    if (DEBUG) print(paste("TRADES:",df$date[1],"to",df$date[nrow(df)]))
    pldol = 0
    plmm = 0
    win = 0
    lose = 0
    riskmm = 999999
    
    for (rownbr in 1:nrow(TRADES)) { 
      if (DEBUG) print(paste( rownbr,
                   TRADES[rownbr,"endate"], 
                   TRADES[rownbr,"enbors"],
                   TRADES[rownbr,"enquant"],
                   TRADES[rownbr,"enprice"],
                   TRADES[rownbr,"exdate"],
                   TRADES[rownbr,"exbors"],
                   TRADES[rownbr,"exprice"],
                   "$",TRADES[rownbr,"pldol"],
                   TRADES[rownbr,"win"],
                   TRADES[rownbr,"lose"]))         
      if (!is.na(TRADES[rownbr,"plmm"]) ) plmm = plmm + TRADES[rownbr,"plmm"]
      if (!is.na(TRADES[rownbr,"pldol"]) ) pldol = pldol + TRADES[rownbr,"pldol"]
      if (!is.na(TRADES[rownbr,"win"]) ) win = win + TRADES[rownbr,"win"]
      if (!is.na(TRADES[rownbr,"lose"]) ) lose = lose + TRADES[rownbr,"lose"]
      if (!is.na(TRADES[rownbr,"riskmm"]) && TRADES[rownbr,"riskmm"] <  riskmm) riskmm = TRADES[rownbr,"riskmm"]        
    }
    
    position = TRADES$pos[nrow(TRADES)]  
    openstat = "No open trades"
    lors = ""
    if (is.na(TRADES$exprice[nrow(TRADES)])) {
      openstat="Holding a Position"
      if (TRADES$enbors[nrow(TRADES)] == "Buy") lors = paste("Long",TRADES$enquant[nrow(TRADES)])
      if (TRADES$enbors[nrow(TRADES)] == "Sell") lors = paste("Short",TRADES$enquant[nrow(TRADES)])
    }
    if (DEBUG) print( paste("Nbr Orders", nrow(ORDERS), "Nbr Trades", nrow(TRADES),"wins",win,
                 "losers", lose, "   P/L $", pldol, openstat, lors))

    #filename <- paste0("L3_",symbol,"-orders.csv")
    #write_csv(ORDERS, filename, append = FALSE, col_names = TRUE)    
    #view(ORDERS)
    
    #filename <- paste0("L3_",symbol,"-trades.csv")
    #write_csv(TRADES, filename, append = FALSE, col_names = TRUE)     
    #view(TRADES)
    

    TSL = TRADE_SUMMARY_LINE
    TSL <- TSL %>%  # Starts a new trade
      add_row(
        symbol  = symbol,
        tsys    = tm$name,
        bsc     = tm$bsc,
        tsrow   = tsrow,
        hadj    = hadj,
        ladj    = ladj,
        win     = as.integer(win),      
        lose    = as.integer(lose),
        plmm    = as.double(plmm),
        pldol   = as.double(pldol),
        riskmm  = as.double(riskmm),
        pos     = position
        #TODO calc risk from trades here
      )

    #view(TSL)
    #print(paste(symbol,name,bsc,hadj,ladj,pldol))
    
    return(list(position, ORDERS, TRADES, TSL)) 
  }

 
  
  # cycles through all trade system adj and inc parms
  # pass in row # for above trade trade method iist table
  tune_trade_system <- function(tsrow=NULL)
  {
    # trade control
    adj=0.30 # LRC std dev range +-0.30
    inc=0.01 # increment
    
    maxdots <- 60 # max nbr dots per line
    
    TRADE_SUMMARY <- TRADE_SUMMARY_LINE # accum all
    pldol <- 0
    cnt <- 0

    # header line
    cat(sprintf(paste("symbol","method","bsc","hadj","ladj","riskmm","Win","Lose","Pos","P/L","\n")))

    ########## run 0 0 first for compare
    TSR <- run_trade_system(symbol=symbol,tsrow=tsrow, df=df, hadj=0,ladj=0)
    position = TSR[[1]] #tried to return a tibble - couldn't get it to work
    pldol <- TSR[[4]]$pldol #in 4th item returned
    tm <- TRADE_METHOD_LIST[tsrow, ] # get that selected row 
    bsc <- tm$bsc[tsrow]
    desc <- tm$desc[tsrow]
    name <- tm$name[tsrow]
    ntrades <- TSR[[4]]$win + TSR[[4]]$lose
    win <- TSR[[4]]$win    
    lose <- TSR[[4]]$lose    
    riskmm <- TSR[[4]]$riskmm     
    cat("\r                                                                    \r") 
    cat(paste(symbol,name,bsc,0,0,riskmm,win,lose,position,pldol))          
    cat("\n")          
    ######################
    
    for (hadj in seq(adj, -adj, by=-inc)) {
      for (ladj in seq(adj, -adj, by=-inc)) {
        hadja <- round(hadj,2)
        ladja <- round(ladj,2)        
        ORDERS <- ORDER_LINE # for just the method and parms
        TRADES <- TRADE_LINE # for just the method and parms
        # now run trade system with these parms 
        TSR <- run_trade_system(symbol=symbol,tsrow=tsrow, df=df, hadj=hadja,ladj=ladja)
        position = TSR[[1]] #tried to return a tibble - couldn't get it to work
        ORDERS <- ORDERS %>% add_row(TSR[[2]])
        TRADES <- TRADES %>% add_row(TSR[[3]])    
        TRADE_SUMMARY <- TRADE_SUMMARY %>% add_row(TSR[[4]]) 
        
        # print if pldol increased
        tm <- TRADE_METHOD_LIST[tsrow, ] # get that selected row 
        bsc <- tm$bsc[tsrow]
        desc <- tm$desc[tsrow]
        name <- tm$name[tsrow]
        ntrades <- TSR[[4]]$win + TSR[[4]]$lose
        win <- TSR[[4]]$win    
        lose <- TSR[[4]]$lose    
        riskmm <- TSR[[4]]$riskmm    
        
        #sink() # turn off - do not write dots to log file
        cat(".")
        cnt <- cnt + 1
        if (TSR[[4]]$pldol > pldol) {
          pldol <- TSR[[4]]$pldol
          cat("\r                                                                    \r") 
          #cat(paste(symbol,name,bsc,hadja,ladja,ntrades,position,pldol))
          LINE <- paste(symbol,name,bsc,hadja,ladja,riskmm,win,lose,position,pldol)
          write(LINE,file="console.log",append=TRUE)
          cat(LINE)
          cat("\n")
          cnt <- 0
        }
        if (cnt > maxdots) { # control number of dots per line
          cat("\r                                                                    \r") 
          cnt <- 0
        }
        #sink("console.log", append=TRUE, split=TRUE) # for screen and log
        
        ####### Sanity check - should not be "Entry" unless position == 0 12/16/2023 buy only
        if (position < 0) {
          filename <- paste0("tst-",symbol,"-orders.csv")
          write_csv(ORDERS, filename, append = FALSE, col_names = TRUE)    
          view(ORDERS)
          
          filename <- paste0("tst-",symbol,"-trades.csv")
          write_csv(TRADES, filename, append = FALSE, col_names = TRUE)     
          view(TRADES)
          
          ORDERS_FILLED <- ORDERS %>% dplyr::filter(filled==TRUE)
          filename <- paste0("tst-",symbol,"-orders-filled.csv")
          write_csv(ORDERS_FILLED, filename, append = FALSE, col_names = TRUE)
          view(ORDERS_FILLED)
          
          UNDECLARED()
        } # end sanity  
        
      }
    }
    
    # find best - highest profit 
    bestsorted <- TRADE_SUMMARY %>% dplyr::arrange(desc(pldol))
    filename <- paste0(symbol,"-summary.csv")
    write_csv(bestsorted, filename, append = FALSE, col_names = TRUE)        

    #best <- TRADE_SUMMARY %>% dplyr::arrange(desc(pldol)) %>% dplyr::slice_head(n=1)
    best <- head(bestsorted,1) # get the best
    filename <- paste0(symbol,"-best.csv")
    write_csv(best, filename, append = FALSE, col_names = TRUE)     
    
    
    #view(TRADE_SUMMARY)
    #view(best)
    
    return(best)   # return best trade summary
  }
  
  ###############################################################
  ######## Top level in trade system tune #######################
  # Cycle through trade methods
  ###############################################################
  #BEST_SUMMARY <- TRADE_SUMMARY_LINE # TODO move this and def to outisde of symbol loop
  for (tsrow in 1:nrow(TRADE_METHOD_LIST)) {
     best <- tune_trade_system(tsrow)
    #view(best)
    BEST_SUMMARY <- BEST_SUMMARY %>% add_row(best) 
  }
  # Find best in BEST_SUMMARY highest profit and lowest loss change append
  #BEST_SUMMARY <- BEST_SUMMARY %>% dplyr::arrange(desc(pldol))
  #best_sum_filename <- paste0(symbol,"_BEST.csv")
  write_csv(BEST_SUMMARY, best_sum_filename, append = TRUE, col_names = FALSE)     
  
  # run_trade_system with best and plot
  # Run Best
  tsrow <- best$tsrow
  hadj <- best$hadj
  ladj <- best$ladj    
  ORDERS <- ORDER_LINE # for just the method and parms
  TRADES <- TRADE_LINE # for just the method and parms
  
  # now run trade system with these parms 
  TSR <- run_trade_system(symbol=symbol,tsrow=tsrow, df=df, hadj=hadj,ladj=ladj)
  position = TSR[[1]] #tried to return a tibble - couldn't get it to work
  ORDERS <- TSR[[2]]
  TRADES <- TSR[[3]]
  TRADE_SUMMARY <- TSR[[4]]
  
  filename <- paste0(symbol,"-orders.csv")
  write_csv(ORDERS, filename, append = FALSE, col_names = TRUE)
  
  filename <- paste0(symbol,"-trades.csv")
  write_csv(TRADES, filename, append = FALSE, col_names = TRUE)
  
  # write just filled orders
  ORDERS_FILLED <- ORDERS %>% dplyr::filter(filled==TRUE)
  filename <- paste0(symbol,"-orders-filled.csv")
  write_csv(ORDERS_FILLED, filename, append = FALSE, col_names = TRUE)
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # TODO plot trades lokk at yrade summary want accum wins losses
  
  TITLE2 <- paste0(symbol1, " ", desc1, " ", lbdate, " ", group1, " Zscore=", zscore)
  TITLE2 <- paste0(TITLE2,
    "<br>",    TRADE_SUMMARY$tsys, 
    " ",       TRADE_SUMMARY$bsc,                         
    " hadj=",  TRADE_SUMMARY$hadj,
    " ladj=",  TRADE_SUMMARY$ladj,
    " win=",   TRADE_SUMMARY$win,
    " lose=",  TRADE_SUMMARY$lose,
    " P/L= $", TRADE_SUMMARY$pldol   
    )
  
 
  # cumsum of P/L
  TRADES <- TRADES %>% mutate(acctbal = cumsum(pldol))
  
  # do our best adjsued predicts
  hpreda <- pred_adj(pred=df$hpred, adj=hadj, minmove=minmove)
  lpreda <- pred_adj(pred=df$lpred, adj=ladj, minmove=minmove)
  cpreda <- pred_adj(pred=df$cpred, adj=0, minmove=minmove)
  mpreda <- pred_adj(pred=df$mpred, adj=0, minmove=minmove)
  preda  <- tibble(hpreda,lpreda,cpreda,mpreda)
  

   ######### NEW CHART #######################
  #plotly chart with ohlc and oreders and trades in hovertext also accy bal chart
  #plot_chart <- function(df=NULL,ORDERS=NULL,TRADES=NULL)
 
  #print(paste("1870", summary(warnings())))

  ############ step through data create hovertext #############
  nl <- "<br>" #newline in hovertext 
  htext <- "" # line per bar hovertext
  
  for(i in 1:nrow(df)) {
    hline <- "" # this bar's hovertext

    ########## ORDERS ##########################################
    hline <- paste0(hline,
                #"<span style='color:black'>",
                "----Orders---")
    orders <- ORDERS %>% dplyr::filter(date == df$date[i]) 
    for (orow in 1:nrow(orders)) { # will be more than 1 entry order if Comb
      order = orders[orow, ] # get order
      hline <- paste0(hline,nl,
                  order$type," ",
                  order$bors," ",
                  order$quant," ",
                  order$mls," ",
                  "at ",order$price)
      if (order$filled == TRUE) {
        hline <- paste0(hline," pos=",order$pos)
        hline <- paste0(hline," Filled")
      }
    }
    
    ############## trade exits for this date #############
    trades <- TRADES %>% dplyr::filter(exdate == df$date[i]) 
    if (nrow(trades) > 0) {
      hline <- paste0(hline,nl,"----COMPLETED TRADES---")      
      for (extrow in 1:nrow(trades)) { 
        hline <- paste0(hline,nl,
                  trades$entype, " ",
                  trades$endate, " ",
                  trades$enprice, " ",
                  trades$extype, " ",
                  trades$exdate, " ",
                  trades$exprice, " ",
                  "bars= ", trades$bars                  
                  ) 
      } # end trade exits
    } else {
      ############# trade entries for this date ###################
      trades <- TRADES %>% dplyr::filter(endate==df$date[i]) 
      if (nrow(trades) > 0) {
        hline <- paste0(hline,nl,"----TRADE Entry---")
        for (entrow in 1:nrow(trades)) { 
          hline <-paste0(hline,nl,
            trades$entype, " ",
            trades$endate, " ",
            trades$enprice, " "
          ) 
        }
      } # end trade entry
    } # end trades
    htext[i] <- hline # add this bar's hovertext in to htext
  }
  

  msize = 8 # marker size
  prev_bal = 0 # previous acct bal
  prev_date = df[0]$date
  
  fig <- plot_ly() # Create the bar chart with trades p/l
  fig2 <- plot_ly(type="bar", x = df$date, showlegend = F) # acct bar chart and cumulate

   # Add OHLC bars
  fig <- fig %>% add_trace(type = "ohlc",
                           x =     df$date,
                           open =  df$open,
                           high =  df$high,
                           low =   df$low,
                           close = df$close,
                           name = "Price",
                           showlegend = F,
                           text = htext, hoverinfo = htext)
  

  fig <- fig %>% add_lines(x = df$date, y = df$hpred, name="hpred", line = list(color = 'black', width = 0.75), visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = preda$hpreda, name="hpreda", line = list(color = 'purple', width = 1.00), visible = "legendonly") 

    
  fig <- fig %>% add_lines(x = df$date, y = df$lpred, name="lpred", line = list(color = 'black', width = 0.75), visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = preda$lpreda, name="lpreda", line = list(color = 'purple', width = 1.00), visible = "legendonly")
  
  fig <- fig %>% add_lines(x = df$date, y = df$cpred, name="cpred", line = list(color = 'red', width = 1.00), legendgroup = 'group1', visible = "legendonly")
  fig <- fig %>% add_lines(x = df$date, y = df$mpred, name="mpred", line = list(color = 'green', width = 1.00), legendgroup = 'group1', visible = "legendonly")
  
  
  # Add trades as lines
  # also create acct p/l bar chart
  for (i in 1:nrow(TRADES)) {
    trade <- TRADES[i, ]
    color <- ifelse(trade$enbors == "Buy", "green", "red") # lines and markers
    color2 <- ifelse(trade$pldol >= 0, "black", "red") # barchart p/l
    
    # text and price for exit/end markers 
    text = paste(
      "Entry: ", trade$endate, trade$enprice, trade$enbors,trade$enquant)
    if (!is.na(trade$exprice)) {  
      text = paste(text,"<br>Exit:  ", trade$exdate, trade$exprice, trade$exbors,trade$enquant,
                   "<br>P/L: $", trade$pldol,"  bars=", trade$bars)
      exdate = trade$exdate
      exprice = trade$exprice
    } else {
      text = paste0(text, "<br>Open Position", "bars=", trade$bars)
      exdate = df$date[nrow(df)]
      exprice = df$close[nrow(df)]
    }
   
    ############# add lines per trade TODO use close if na in exprice
    fig <- fig %>% add_trace(type = "scatter", mode = "lines",
                             x = c(trade$endate, exdate),
                             y = c(trade$enprice, exprice), showlegend = F,
                             line = list(color = color, width = 2))
    
   
    #fig <- fig %>% add_markers(x = exdate, y = exprice,
    #                           name = paste("Trade", i),
    #                           showlegend = F,
    #                           marker = list(color = color, size = msize),
    #                           text = text, hoverinfo = text)
    
    ############# acct bar chart and cumulative line
    fig2 <- fig2 %>% add_bars(x = exdate, y = trade$pldol,
                              marker = list(color = color2), showlegend = F)
    #fig2 <- fig2 %>% add_bars(x = exdate, y = trade$acctbal,
    #                          marker = list(color = "black"), showlegend = F)
    
    fig2 <- fig2 %>% add_trace(type = "scatter", mode = "lines",
                             x = c(prev_date, exdate),
                             y = c(prev_bal, trade$acctbal), showlegend = F,
                             line = list(color = color2, width = 2))
    
    text <- paste("Trade", i, "$",trade$acctbal)
    fig2 <- fig2 %>% add_markers(x = exdate, y = trade$acctbal,
                               showlegend = F,
                               marker = list(color = "black", size = msize),
                               #hoverinfo = text
                               )
    
    
    #print(paste(prev_bal,trade$acctbal))
    prev_date <- exdate
    prev_bal <- trade$acctbal
  }
  
  ########### next bar orders hovertext #############
  fig <- fig %>% add_markers( x = df$date[nrow(df)], y = df$mid[nrow(df)-1],
                             name = "Next Orders",
                             showlegend = F,
                             marker = list(color = "black", size = 10),
                             text = htext[length(htext)], hoverinfo = htext[length(htext)])
  
  ########### Draw arrow to next orders #################
  fig <- fig %>% add_annotations( x = df$date[nrow(df)], y = df$mid[nrow(df)-1],
    xref = "x", yref = "y",
    showarrow = TRUE, font_size=24,
    text="Next Orders"
  )
  
  fig <- fig %>% plotly::layout(showlegend = F, 
                                hovermode = 'x',
                                xaxis = list(type = 'category', rangeslider = list(visible = F)),
                                yaxis = list(title = "Price"))
  
  fig <- fig %>% add_annotations(
    x=0.5, y=0.97, # align='bottom',
    xref = "paper", yref = "paper",
    showarrow = FALSE, font_size=24,
    text="© 2024 by Larry Calvert 1 bar ahead order triggers. Use at your own risk."
    #"<b>© 2024 by Larry Calvert Use at your own risk https://larrycalvert.github.io/charts2/menu</b>" 
    # copywrite symbol enclose text in <b> text </b> for bold
  )


  fig2 <- fig2 %>% plotly::layout(showlegend = T, 
                                hovermode = 'x',
                                xaxis = list(type = 'category', rangeslider = list(visible = F)),
                                yaxis = list(title = "Account"))
  
 
   ################# subplot combine ############
  fig5 <- subplot(fig, fig2, heights = c(0.8,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE)
  

  fig5 <- fig5 %>% config(displayModeBar = TRUE, displaylogo = FALSE,
                          modeBarButtonsToRemove =
                            c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", 
                              "select", "autoscale", "lasso","hoverCompareCartesian",
                              "hoverClosest")
                          ) #toImage"
  
  fig5 <- fig5 %>% plotly::layout(title = TITLE2, showlegend = T,
                                  xaxis = list(title = "Date"))
  
  
  print(fig5) 
  
  print(paste("2101", "Warnings", summary(warnings())))
  

  cat("###############################################\n")
  cat("######## END New Trade Simulator ##############\n")
  cat("###############################################\n")
##################################################
  

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

    # adding meta and dis
    #cline <- paste0("git add meta.html dis.html")
    #cline <- paste0("git add nvda.html")
    #cline <- paste0("git add mmm.html cost.html")
    # cline <- paste0("git add smci.html")
    # retval <- system(cline, minimize=FALSE, wait=TRUE)
    # print(paste0(cline, " retval=",retval))
     
    # Adding test menu
    #cline <- paste0("git add menu.html menu.csv")
    #cline <- paste0("git add corn.html oats.html")
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
print(paste("END","WARNINGS", summary(warnings())))
#sink() # turns off console logging
############## END teng1.R #######################################
  