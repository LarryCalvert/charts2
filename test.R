
######### TEST SECTION ######################
if (0) { # Inter active brokers - not connecting
  pacman::p_load("IBrokers")
  tws <- twsConnect()
  twsFuture("YM","ECBOT","200809")
  reqContractDetails(tws, twsEquity("QQQQ"))
  reqMktData(tws, twsEquity("QQQQ"))
}

if (0) { # trades with arrows and markers - from chat GPT
  # Function to generate random OHLC data for demonstration
  generate_random_ohlc <- function(n_bars = 40) {
    set.seed(123) # For reproducibility
    data <- data.frame(Date = seq(Sys.Date() - n_bars, by = "1 day", length.out = n_bars),
                       Open = runif(n_bars, min = 100, max = 150),
                       High = runif(n_bars, min = 150, max = 200),
                       Low = runif(n_bars, min = 50, max = 100),
                       Close = runif(n_bars, min = 100, max = 150))
    return(data)
  }
  
  # Generate random OHLC data for 40 bars
  ohlc_data <- generate_random_ohlc(40)
  df <- tibble(ohlc_data)
  
  # Function to generate random trade data for demonstration
  generate_random_trades <- function(n_trades = 6) {
    set.seed(456) # For reproducibility
    trade_dates <- sample(df$Date, n_trades, replace = FALSE)
    entry_prices <- runif(n_trades, min(df$Low), max(df$High))
    exit_prices <- runif(n_trades, min(df$Low), max(df$High))
    trade_directions <- sample(c("Buy", "Sell"), n_trades, replace = TRUE)
    
    trades <- data.frame(EntryDate = trade_dates,
                         EntryPrice = entry_prices,
                         ExitDate = trade_dates + sample(1:10, n_trades, replace = TRUE),
                         ExitPrice = exit_prices,
                         TradeDirection = trade_directions)
    
    #trades <- tibble(EntryDate = trade_dates,
    #                 EntryPrice = entry_prices,
    #                 ExitDate = as.Date(as.Date(trade_dates) + sample(1:3, n_trades, replace = TRUE)),
    #                 ExitPrice = exit_prices,
    #                 TradeDirection = trade_directions)
    return(trades)
  }
  
  # Generate random trade data for 6 trades
  trade_data <- generate_random_trades(6)
  
  # Create the stock bar chart with trades
  fig <- plot_ly()
  
  # Add OHLC bars
  fig <- fig %>% add_trace(type = "ohlc",
                           x = df$Date,
                           open = df$Open,
                           high = df$High,
                           low = df$Low,
                           close = df$Close,
                           name = "OHLC")
  
  # Add trades as markers
  for (i in 1:nrow(trade_data)) {
    trade <- trade_data[i, ]
    color <- ifelse(trade$TradeDirection == "Buy", "green", "red")
    fig <- fig %>% add_markers(x = trade$EntryDate, y = trade$EntryPrice,
                               name = paste("Entry", i),
                               marker = list(color = color, symbol = "triangle-up"),
                               text = paste("Entry Price: ", trade$EntryPrice, "<br>Date: ", trade$EntryDate),
                               hoverinfo = "text")
    fig <- fig %>% add_markers(x = trade$ExitDate, y = trade$ExitPrice,
                               name = paste("Exit", i),
                               marker = list(color = color, symbol = "triangle-down"),
                               text = paste("Exit Price: ", trade$ExitPrice, "<br>Date: ", trade$ExitDate),
                               hoverinfo = "text")
    fig <- fig %>% add_trace(type = "scatter", mode = "lines",
                             x = c(trade$EntryDate, trade$ExitDate),
                             y = c(trade$EntryPrice, trade$ExitPrice),
                             line = list(color = color, width = 2))
  }
  
  # Customize layout
  fig <- fig %>% plotly::layout(title = "Stock Bar Chart with Trades",
                                xaxis = list(title = "Date"),
                                yaxis = list(title = "Price"),
                                showlegend = TRUE)
  
  # Display the plot
  fig 
}

if (0) { # plotly from chat GPT - xts has problems but includes lines and markers
  library(quantmod)
  library(TTR)
  library(plotly)
  
  # Fetch end-of-day data for the stock 'META'
  getSymbols("META", from = Sys.Date() - 40, to = Sys.Date(), src = "yahoo")
  
  # Calculate 2 moving averages (e.g., 10-day and 20-day)
  ma_short <- SMA(Cl(META), n = 10)
  ma_long <- SMA(Cl(META), n = 20)
  
  # Create buy signals based on moving average crossover
  buy_signals <- ifelse(ma_short > ma_long, 1, 0)
  buy_signals <- diff(buy_signals)
  buy_entries <- which(buy_signals == 1) # Positions of buy entries
  
  # Get prices for buy entries and exits
  buy_entries_prices <- Cl(META)[buy_entries]
  buy_exits <- which(buy_signals == -1) # Positions of buy exits
  buy_exits_prices <- Cl(META)[buy_exits]
  
  # Prepare OHLC data for plotly chart
  ohlc_data <- data.frame(Date = index(META),
                          Open = Op(META),
                          High = Hi(META),
                          Low = Lo(META),
                          Close = Cl(META))
  
  # Create OHLC chart using plotly
  ohlc_chart <- plot_ly(data = ohlc_data, type = "ohlc", mode = "lines+markers") %>%
    add_lines(x = ~Date, y = ~ma_short, name = "Short MA", line = list(color = "blue")) %>%
    add_lines(x = ~Date, y = ~ma_long, name = "Long MA", line = list(color = "red")) %>%
    layout(title = "OHLC Chart with Moving Average Crossover",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Price"))
  
  # Add buy trade signals to the chart
  for (i in 1:min(length(buy_entries), length(buy_exits))) {
    entry_index <- buy_entries[i]
    exit_index <- buy_exits[i]
    
    entry_price <- buy_entries_prices[i]
    exit_price <- buy_exits_prices[i]
    
    if (entry_index < exit_index) {
      ohlc_chart <- ohlc_chart %>%
        add_trace(x = c(index(META)[entry_index], index(META)[exit_index]),
                  y = c(entry_price, exit_price),
                  type = "scatter",
                  mode = "lines+markers",
                  line = list(color = "green"),
                  name = paste("Trade", i),
                  text = paste("Entry: ", entry_price, "<br>Exit: ", exit_price),
                  hoverinfo = "text")
    }
  }
  
  # Display the OHLC chart with trade signals
  ohlc_chart
  
} # end plotly from chat GPT

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
