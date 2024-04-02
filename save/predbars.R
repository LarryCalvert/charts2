##############################
# Main Predict loop
# Larry Calvert
# 02/02/2022
##############################

# Original 01/30/2023
pred_bars <- function(
  symbol_list=NA,
  datatype_list=NA,
  pproc_list=NA,
  method_list=NA,
  intl=NA,
  nbr_bars=NA)
{
  ptm <- proc.time()
  totalcnt <- 0

  detail <- DETAIL_LINES
  
  print(paste0("Predicting ", nbr_bars, " Bars"))
  # loop through symbols
  for(symrow in 1:nrow(symbol_list)) { # symbol loop
    symbol <- symbol_list$symbol[[symrow]]
    dec <- symbol_list$decimals[[symrow]]
    msdataint <- load_data(symbol=symbol,intl=intl,dec=dec)
    print(paste0("######### ", symbol, " ##########"))
      
    # datatype grouping loop
    #print(datatype_list)
      
    for (dtrow in 1:nrow(datatype_list)) { # datatype loop
      dt <- as.list(datatype_list$dt[[dtrow]])
      group <- datatype_list$group[[dtrow]]
      # hlcm in group loop
      for (col in 1:length(dt)) {  
        dt1 <- as.character(dt[col])
          
        print(paste0(symbol," ",dt1))
          
        # Tune bar loops here #############################
        for (tb in nbr_bars:0) {
          bupts <-  tb * ppb
            
          # current bar
          in1date <- request_data_l(msdataint=msdataint,type="date",bupts=bupts,len=datalen)
          in1 <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts,len=datalen)
          # target bar
          target <- rep(NA,datalen+ppb) # target unknown on current bar 
          if (bupts >= ppb) target <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts-ppb,len=datalen+ppb)
            
		      # Pre Process loop
          for(pprocrow in 1:nrow(pproc_list)) {        
		       pproc <- pproc_list[[pprocrow,"PPROC"]]
		   
		       in1pp <- in1
           in1pp <- preproc(PPROC=pproc, input=in1pp)
			
            # Method loop
            for(methrow in 1:nrow(method_list)) {          
              PTYPE1 <- method_list[[methrow,"PTYPE1"]]
              PTYPE2 <- method_list[[methrow,"PTYPE2"]]          
              FTYPE1 <- method_list[[methrow,"FTYPE1"]]          
              PTYPE3 <- method_list[[methrow,"PTYPE3"]]
              PTYPE4 <- method_list[[methrow,"PTYPE4"]]          
              method <- method_list[[methrow,"METHOD"]]
              
              currentTs <- Sys.time() # time this section
              
              #if (PTYPE1 != "none") pred1 <- predict2(in1pp, ptype1=PTYPE1, ptype2=PTYPE2, wsize=wsize,
              #        chart = TRUE, maintitle = TITLEB, title = toupper(DATASTYPE), xstart = xstart)
              
              f1 <- filter_func(in1pp,FTYPE1)
              fita1 <- auto.arima(f1, stepwise=FALSE, approximation=FALSE) #slower more accurate
              fc <- forecast::forecast(fita1, h=2)
              #print(fc$method)
              
              # View the point forecasts
              #fc$mean
              # View the upper prediction interval
              #fc$upper
              # View the lower prediction interval
              #fc$lower
              
              # Plot the forecast
              #plot(fc)
              #prediction is in fitted plus append mean
              pred2 <- fc$fitted
              for (i in seq(1:length(fc$mean))) {
                pred2 <- append(pred2,fc$mean[i])
              }
              
              predict <- pred2
              predict <- undo_preproc(PPROC=pproc, input=predict)
              predict <- round(predict, digits=dec)
            
              eltime <- Sys.time() - currentTs # end time section
            
              #xstart=0
              #pred2 <- predict2(as.ts(f1), ptype1="arma", ptype2=PTYPE4, wsize=wsize,
              #    chart = TRUE, maintitle = TITLEB, title = toupper(price), xstart = xstart)
              
              # chart in1 and predict    
              in1ppa <- in1
              for (i in seq(1:length(fc$mean))) {
                in1ppa <- append(in1ppa,NA)
              }
              
              if ( (totalcnt %% 10) == 0 ) {
                plot(in1ppa, type="l")
                points(predict, col="red", pch="*")
              }
              
              # calc errors
              err <- target - predict
              err <- round(err, digits=dec)
              sderr <- round(STDE(err), digits=dec * 2) 
              
              tunedates <- tail(in1date,ppb)
              in1s <- tail(in1,ppb)
              predicts <- tail(predict,ppb)
              targets <- tail(target,ppb)
              errs <- tail(err,ppb)
              sderrs <- tail(sderr,ppb)     
              
            for (i in 1: ppb) {
              detail <- detail %>%  
                add_row(
                  date = tunedates[i],
                  symbol = symbol,
                  dt = dt1,
                  group = group,
				          pproc = pproc,
                  method = method,
                  price = in1s[i],
                  predict = predicts[i],
                  target = targets[i],
                  error = errs[i],
                  sderr = sderr[i],
                  etime = as.double(eltime/ppb)
                )
              }
              totalcnt <- totalcnt + 1
              if (DEBUG == TRUE) { # debug
                print(paste0(totalcnt, " ", symbol, " ", tail(in1date,1), " ", 
                     dt1, " tb=", tb, " meth=", method, " sderr=", sderr))
              }
		        } # end preprocess loop
          } # end method loop
        } # end tune bar loop
      } # datatype individual loop
    } # datatype group loop
  } # symbol loop
  print(paste0("Total count = ",totalcnt))
  print(proc.time() - ptm) # Total time
  return(detail)
} 

predict_level1 <- function(
  detail=NA,
  symbol_list=NA,
  datatype_list=NA,
  pproc_list=NA,
  method_list=NA,
  intl=NA)
{
  ptm <- proc.time()
  totalcnt <- 0

  detailw <- DETAIL_LINES
  
  print(paste0("Predicting ", nbr_bars, " Bars"))
  # loop through symbols
  for(symrow in 1:nrow(symbol_list)) { # symbol loop
    symbol <- symbol_list$symbol[[symrow]]
    dec <- symbol_list$decimals[[symrow]]
    msdataint <- load_data(symbol=symbol,intl=intl,dec=dec)
    print(paste0("######### ", symbol, " ##########"))
      
    # datatype grouping loop
    #print(datatype_list)
      
    for (dtrow in 1:nrow(datatype_list)) { # datatype loop
      dt <- as.list(datatype_list$dt[[dtrow]])
      group <- datatype_list$group[[dtrow]]
      # hlcm in group loop
      for (col in 1:length(dt)) {  
        dt1 <- as.character(dt[col])
          
        print(paste0(symbol," ",dt1))
                    
		#  Pre Process loop
		for(pprocrow in 1:nrow(pproc_list)) {        
		  pproc <- pproc_list[[pprocrow,"PPROC"]]
		   
		# Method loop
         for(methrow in 1:nrow(method_list)) {          
          PTYPE1 <- method_list[[methrow,"PTYPE1"]]
          PTYPE2 <- method_list[[methrow,"PTYPE2"]]          
          FTYPE1 <- method_list[[methrow,"FTYPE1"]]          
          PTYPE3 <- method_list[[methrow,"PTYPE3"]]
          PTYPE4 <- method_list[[methrow,"PTYPE4"]]          
          method <- method_list[[methrow,"METHOD"]]
          
		  ######## Calc number of bars here
		  
        # Tune bar loops here #############################
        for (tb in nbr_bars:0) {
          bupts <-  tb * ppb
            
          # current bar
          in1date <- request_data_l(msdataint=msdataint,type="date",bupts=bupts,len=datalen)
          in1 <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts,len=datalen)
		  
		in1pp <- in1
           in1pp <- preproc(PPROC=pproc, input=in1pp)
		   
          # target bar
          target <- rep(NA,datalen+ppb) # target unknown on current bar 
          if (bupts >= ppb) target <- request_data_l(msdataint=msdataint,type=dt1,bupts=bupts-ppb,len=datalen+ppb)		  
              currentTs <- Sys.time() # time this section
              
              
              f1 <- filter_func(in1pp,FTYPE1)
              fita1 <- auto.arima(f1, stepwise=FALSE, approximation=FALSE) #slower more accurate
              fc <- forecast::forecast(fita1, h=2)
              #print(fc$method)
              
              # View the point forecasts
              #fc$mean
              # View the upper prediction interval
              #fc$upper
              # View the lower prediction interval
              #fc$lower
              
              # Plot the forecast
              #plot(fc)
              #prediction is in fitted plus append mean
              pred2 <- fc$fitted
              for (i in seq(1:length(fc$mean))) {
                pred2 <- append(pred2,fc$mean[i])
              }
              
              predict <- pred2
              predict <- undo_preproc(PPROC=pproc, input=predict)
              predict <- round(predict, digits=dec)
            
              eltime <- Sys.time() - currentTs # end time section
            
             
              # chart in1 and predict    
              in1ppa <- in1
              for (i in seq(1:length(fc$mean))) {
                in1ppa <- append(in1ppa,NA)
              }
              
              if ( (totalcnt %% 10) == 0 ) {
                plot(in1ppa, type="l")
                points(predict, col="red", pch="*")
              }
              
              # calc errors
              err <- target - predict
              err <- round(err, digits=dec)
              sderr <- round(STDE(err), digits=dec * 2) 
              
              tunedates <- tail(in1date,ppb)
              in1s <- tail(in1,ppb)
              predicts <- tail(predict,ppb)
              targets <- tail(target,ppb)
              errs <- tail(err,ppb)
              sderrs <- tail(sderr,ppb)     
              
            for (i in 1: ppb) {
              detail <- detail %>%  
                add_row(
                  date = tunedates[i],
                  symbol = symbol,
                  dt = dt1,
                  group = group,
				          pproc = pproc,
                  method = method,
                  price = in1s[i],
                  predict = predicts[i],
                  target = targets[i],
                  error = errs[i],
                  sderr = sderr[i],
                  etime = as.double(eltime/ppb)
                )
              }
              totalcnt <- totalcnt + 1
              if (DEBUG == TRUE) { # debug
                print(paste0(totalcnt, " ", symbol, " ", tail(in1date,1), " ", 
                     dt1, " tb=", tb, " meth=", method, " sderr=", sderr))
              }
			} # end tune bar loop
          } # end preprocess loop
        } # end method loop
      } # datatype individual loop
    } # datatype group loop
  } # symbol loop
  print(paste0("Total count = ",totalcnt))
  print(proc.time() - ptm) # Total time
  return(detail)
} 


############### end predbars ################

