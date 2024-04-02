# Read msdata
# Larry Calvert
# 01/18/2023
# read txt file and create tibble
# using lubridate

# Original Metastock data
load_data <- function(symbol=NA,intl=NA,dec=NA) {
  ppb <- intl + 1
  
  filename <- paste0(symbol,".txt")
  msdata <- read_csv(filename, 
                     col_names =  c("open","high","low","close","oi","vol","date","nbr"), cols(
                       open = col_double(),
                       high = col_double(),
                       low = col_double(),
                       close = col_double(),
                       oi = col_number(),
                       vol = col_number(),
                       date = col_character(),
                       nbr = col_number()
                     ))
  
  # create mid signal Todo change to mutate!
  msdata <- within(msdata, mid <- (high + low) * 0.5) # mid calc
  
  tail(msdata,n=10)
  
  orglen <- length(msdata$nbr) # full length of uninterpolated from AMiBroker
  wanted_len <- orglen * ppb
  
  if (intl == 0) { #No Interpolate
    open <- round( msdata$open, digits=dec)
    high <- round( msdata$high, digits=dec)
    low <-  round( msdata$low, digits=dec)
    close <- round( msdata$close, digits=dec)
    mid <- round( msdata$mid, digits=dec)
    vol <- round( msdata$vol, digits=0)
    oi <- round( msdata$oi, digits=0)
    intnbr <- round( msdata$nbr, digits=0)
    
    ticker <- NULL
    date <- NULL
    tmpticker <- as.character(symbol)
    
    for (i in 1:orglen) {
      #tmpticker <- msdata$ticker[i]
      tmpdate <- msdata$date[i]
      todo <-1
      if (i < orglen) todo <- ppb
      for (j in 1:todo) {
        ticker <- append(ticker,tmpticker)
        #date <- append(date,paste0(tmpdate, ":", toString(j)))
        date <- append(date,paste0(tmpdate)) # 05-14-2019
      }
    }
  }
  
  if (intl > 0) { # we interpolate
    xf <- seq(1,orglen,by= (1 / ppb))
    open <- round( interp1(msdata$nbr, msdata$open, xf, method='spline'), digits=dec)
    high <- round( interp1(msdata$nbr, msdata$high, xf, method='spline'), digits=dec)
    low <- round( interp1(msdata$nbr, msdata$low, xf, method='spline'), digits=dec)
    close <- round( interp1(msdata$nbr, msdata$close, xf, method='spline'), digits=dec)
    mid <- round( interp1(msdata$nbr, msdata$mid, xf, method='spline'), digits=dec)
    vol <- round( interp1(msdata$nbr, msdata$vol, xf, method='spline'), digits=0)
    oi <- round( interp1(msdata$nbr, msdata$oi, xf, method='spline'), digits=0)
    intnbr <- round( interp1(msdata$nbr, msdata$nbr, xf, method='spline'), digits=0)
    
    # do ticker column and date column
    ticker <- NULL
    date <- NULL
    direct <- NULL
    tmpticker <- as.character(symbol)
    
    for (i in 1:orglen) {
      #tmpticker <- msdata$ticker[i]
      tmpdate <- msdata$date[i]
      todo <-1
      if (i < orglen) todo <- ppb
      for (j in 1:todo) {
        ticker <- append(ticker,tmpticker)
        #date <- append(date,paste0(tmpdate, ":0", toString(j), ":00:00"))
        date <- append(date,paste0(tmpdate, " 0", toString(j), ":00:00"))      
      }
    }
  }
  
  # LRC here so far
  datet <- ymd_hms(date,tz=Sys.timezone())
  date <- datet
  
  # Create relative to Open
  oro <- round(open - open, dec)
  hro <- round(high - open, dec)
  lro <- round(low - open, dec)
  cro <- round(close - open, dec)
  mro <- round(mid - open, dec)
  
  #create relative to last mid
  mp <- dplyr::lag(mid, n = 1L) # mid prev
  or <- round(open - mp, dec)
  hr <- round(high - mp, dec)
  lr <- round(low - mp, dec)
  cr <- round(close - mp, dec)
  mr <- round(mid - mp, dec)
  
  
  #tmp <- cbind(ticker,date,open,high,low,close,mid,vol,oi,intnbr,
  #             hro,lro,cro,mro,or,hr,lr,cr,mr) 
  #as_tibble(tmp)
  
  tmp <- tibble(ticker,date,open,high,low,close,mid,vol,oi,intnbr,
               oro,hro,lro,cro,mro,or,hr,lr,cr,mr) 
  return(tmp)
}

request_data_l <- function(msdataint=NA, type=NA, bupts=NA, len=NA) {
  
  type <- tolower(type)
  dlen <- length(msdataint$ticker) - bupts #03/27/2019 added ppb
  
  #start <- dlen - LEN + 1 #2019-08-28 changed as
  # datalen+ppb was not returnning right length
  start <- dlen - len + 1
  msdataints <- msdataint[start:dlen,]
  
  if (type == "date") {
    return(tail(msdataints$date,len))
  }
  
  if (type == "ticker") {
    return(tail(msdataints$ticker,len))
  }

  if (type == "vol") {
    return(tail(msdataints$vol,len))
  }
  
  if (type == "o") {
    return(as.numeric(tail(msdataints$open,len)))
  }
  if (type == "h") {
    return(as.numeric(tail(msdataints$high,len)))
  }
  if (type == "l") {
    return(as.numeric(tail(msdataints$low,len)))
  }  
  if (type == "c") {
    return(as.numeric(tail(msdataints$close,len)))
  }
  if (type == "m") {
    return(as.numeric(tail(msdataints$mid,len)))
  }
  if (type == "oro") {
    return(as.numeric(tail(msdataints$oro,len)))
  }  
  if (type == "hro") {
    return(as.numeric(tail(msdataints$hro,len)))
  }
  if (type == "lro") {
    return(as.numeric(tail(msdataints$lro,len)))
  }  
  if (type == "cro") {
    return(as.numeric(tail(msdataints$cro,len)))
  }
  if (type == "mro") {
    return(as.numeric(tail(msdataints$mro,len)))
  }
  
  if (type == "or") {
    return(as.numeric(tail(msdataints$or,len)))
  } 
  if (type == "hr") {
    return(as.numeric(tail(msdataints$hr,len)))
  }
  if (type == "lr") {
    return(as.numeric(tail(msdataints$lr,len)))
  }  
  if (type == "cr") {
    return(as.numeric(tail(msdataints$cr,len)))
  }
  if (type == "mr") {
    return(as.numeric(tail(msdataints$mr,len)))
  }  
}

# stock datal
load_data2 <- function(symbol=NA,intl=NA,dec=NA) {
  ppb <- intl + 1
  
  filename <- paste0(symbol,".txt")
  msdata <- read_csv(filename, 
                    col_names =  c("symbol","date","open","high","low","close","vol","adjusted"), cols(
						symbol = col_character(),
                       date = col_character(),						
                       open = col_double(),
                       high = col_double(),
                       low = col_double(),
                       close = col_double(),
                       vol = col_number()
                     ))
					 

  # create mid signal Todo change to mutate!
  msdata <- within(msdata, mid <- (high + low) * 0.5) # mid calc
  
  # append row number
	msdata <- msdata %>%  mutate(oi=NA)
  msdata <- msdata %>%  mutate(nbr=row_number())	
  
  #print(tail(msdata,n=10))
  
  orglen <- length(msdata$nbr) # full length of uninterpolated from AMiBroker
  wanted_len <- orglen * ppb
  
  if (intl == 0) { #No Interpolate
    open <- round( msdata$open, digits=dec)
    high <- round( msdata$high, digits=dec)
    low <-  round( msdata$low, digits=dec)
    close <- round( msdata$close, digits=dec)
    mid <- round( msdata$mid, digits=dec)
    vol <- round( msdata$vol, digits=0)
    oi <- round( msdata$oi, digits=0)
    intnbr <- round( msdata$nbr, digits=0)
    
    ticker <- NULL
    date <- NULL
    tmpticker <- as.character(symbol)
    
    for (i in 1:orglen) {
      #tmpticker <- msdata$ticker[i]
      tmpdate <- msdata$date[i]
      todo <-1
      if (i < orglen) todo <- ppb
      for (j in 1:todo) {
        ticker <- append(ticker,tmpticker)
        #date <- append(date,paste0(tmpdate, ":", toString(j)))
        date <- append(date,paste0(tmpdate)) # 05-14-2019
      }
    }
  }
  
  if (intl > 0) { # we interpolate
    xf <- seq(1,orglen,by= (1 / ppb))
    open <- round( interp1(msdata$nbr, msdata$open, xf, method='spline'), digits=dec)
    high <- round( interp1(msdata$nbr, msdata$high, xf, method='spline'), digits=dec)
    low <- round( interp1(msdata$nbr, msdata$low, xf, method='spline'), digits=dec)
    close <- round( interp1(msdata$nbr, msdata$close, xf, method='spline'), digits=dec)
    mid <- round( interp1(msdata$nbr, msdata$mid, xf, method='spline'), digits=dec)
    vol <- round( interp1(msdata$nbr, msdata$vol, xf, method='spline'), digits=0)
    #oi <- round( interp1(msdata$nbr, msdata$oi, xf, method='spline'), digits=0)
    intnbr <- round( interp1(msdata$nbr, msdata$nbr, xf, method='spline'), digits=0)
    
    # do ticker column and date column
    ticker <- NULL
    date <- NULL
    direct <- NULL
    tmpticker <- as.character(symbol)
    
    for (i in 1:orglen) {
      #tmpticker <- msdata$ticker[i]
      tmpdate <- msdata$date[i]
      todo <-1
      if (i < orglen) todo <- ppb
      for (j in 1:todo) {
        ticker <- append(ticker,tmpticker)
        #date <- append(date,paste0(tmpdate, ":0", toString(j), ":00:00"))
        date <- append(date,paste0(tmpdate, " 0", toString(j), ":00:00"))      
      }
    }
  }
  
  # LRC here so far
  datet <- ymd_hms(date,tz=Sys.timezone())
  date <- datet
  
  # Create relative to Open
  oro <- round(open - open, dec)
  hro <- round(high - open, dec)
  lro <- round(low - open, dec)
  cro <- round(close - open, dec)
  mro <- round(mid - open, dec)
  
  #create relative to last mid
  mp <- dplyr::lag(mid, n = 1L) # mid prev
  or <- round(open - mp, dec)
  hr <- round(high - mp, dec)
  lr <- round(low - mp, dec)
  cr <- round(close - mp, dec)
  mr <- round(mid - mp, dec)
  
  
  #tmp <- cbind(ticker,date,open,high,low,close,mid,vol,oi,intnbr,
  #             hro,lro,cro,mro,or,hr,lr,cr,mr) 
  #as_tibble(tmp)
  
  #tmp <- tibble(ticker,date,open,high,low,close,mid,vol,oi,intnbr,
  #            oro,hro,lro,cro,mro,or,hr,lr,cr,mr) 
			
			# dropping oi in stocks
 tmp <- tibble(ticker,date,open,high,low,close,mid,vol,intnbr,
               oro,hro,lro,cro,mro,or,hr,lr,cr,mr) 			   
  return(tmp)
}
