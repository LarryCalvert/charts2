############ General Purpose arma & NN predict ###############
# arma_in <- data to be predicted (Plen = length to be plotted)
# output predict in armap and armapbp residuals in resida1
# example: arma_in <- smoothed10
# returns:
#   armap, armapbp, resida1,  also arma_inext for plotting
####### Extend to predict length for plotting #############
# 03/12/2024 added zlma2
#2019-08-20 reworked arma predict
#2018-09-08 emd_predict2

#using("KernSmooth")
#using("bestNormalize") # for bestnormalize
#using("Rlibeemd") # for emd predict
#using("spectral") # for fft_filter

#using("smooth") # for only smoothCombine or es if needed
#using("mFilter")
#using("itsmr") # for smooth.fft
#using("pracma") # for movavg

# TODO - Larry fix this - use full package name
conflict_prefer("forecast::forecast", "forecast") 

usemod <- "aefnt" # hybrid drop the stl model requires Timeseries

# Todo: convert (in1, ptype, norm, pred_length, quiet?)
an_predict2 <- function (in1, ptype, wsize = (10*ppb) ) {
  
	chart <- FALSE
	DETAIL <- FALSE
	
  input_name <- deparse(substitute(in1))
  log4r::debug(logger, paste0("predict2 input=",input_name," ptype=",ptype))
 
  ### No predict just push input 1 bar forward - changed to return input
  if (ptype == "none" ) {
    #pred <- rep(NA,Plen)
    #pred <- append(pred,in1)
	#return(pred)
	return(in1) #LRC 05/27/2023
  } 
  
  ####### New normalize combined predict 
  if (ptype == "norm" || ptype == "normw" || ptype == "normcv") {

	if (chart == TRUE) {
		#Histogram Plot incomming signal
		index <- 1:(length(in1))  # create index variable
    
		pplot <- tibble(index,in1)
    
		g1 <- ggplot(pplot, aes(x=in1)) +
		geom_histogram(colour="black",  binwidth = 0.20) +
		ylab(label="in1") + 
		xlab("Values") 
    
		# put plots on desktop also
		DETAIL <- 0
		if (DETAIL == 1) {
			f_name <- "mstlm.png"
			png(f_name, width=1400, height=1000,  type = "windows") 
			suppressWarnings( print(g1) ) # surpress warnings about NAs
			dev.off() 
			browseURL(f_name) 
		}
		suppressWarnings( print(g1) ) # surpress warnings about NAs
    }

    BN_OBJ <- bestNormalize(in1, allow_lambert_s = TRUE,
                            standardize = TRUE, loo = TRUE)
   #quiet = TRUE)
    #print(BN_OBJ)
    #plot(BN_OBJ, inverse = FALSE, bounds = NULL,
     #    cols = NULL, methods = NULL, leg_loc = "top")
    
    in1norm <- predict(BN_OBJ)

    #Histogram Plot Normalized signal
	if (chart == TRUE) {
		index <- 1:(length(in1norm))  # create index variable
        pplot <- tibble(index,in1norm)
    
		g1 <- ggplot(pplot, aes(x=in1norm)) +
		geom_histogram(colour="black",  binwidth = 0.20) +
		ylab(label="in1norm") + 
		xlab("Values") 
    
		# put plots on desktop also
		if (DETAIL == 1) {
		f_name <- "mstlm.png"
		png(f_name, width=1400, height=1000,  type = "windows") 
		suppressWarnings( print(g1) ) # surpress warnings about NAs
		dev.off() 
		browseURL(f_name) 
		}
		suppressWarnings( print(g1) ) # surpress warnings about NAs
	}
    #message(paste0("Hybrid predict"))    
    if (ptype == "norm") 
      mod <- hybridModel(in1norm, models = usemod,  verbose = FALSE,
              parallel = PARALLEL, num.cores = use_cores)
    if (ptype == "normw") 
      mod <- hybridModel(in1norm, models = usemod, weights = "insample.errors",  verbose = FALSE,
              parallel = PARALLEL, num.cores = use_cores)
    
    if (ptype == "normcv") {
      #message("Cross Validate hybrid predict - may take a while")
      #wsize <- floor(length(in1norm) / 10) # 10 chunks
      #message(paste0("normcv wsize=", as.character(wsize)), 
      #       " Parallel=",PARALLEL," Use_cores=",use_cores)
	  wsize <- 10
      mod <- hybridModel(in1norm, models = usemod, weights = "cv.errors",
              verbose = FALSE,  
              #rolling = FALSE, # test
              #horizonAverage = TRUE,
              cvHorizon = ppb, windowSize = wsize,
              parallel = PARALLEL, num.cores = use_cores)	  
      #accuracy(mod)
    }
    
    mod_fit <- tibble(fitted(mod,individual = TRUE))
    #print(mod_fit)

    # write csv for duebugging    
    #in1v <- enframe(in1norm,name=NULL)
    #modl1_tbl <- dplyr::bind_cols(in1v,mod_fit)
    #MODL1FILE <- paste0(AFLDIR, "modl1", ".csv") 
    #write_csv(modl1_tbl, MODL1FILE)
    
    fc <- forecast::forecast(mod, h=Plen)
    #print(fc$method)
    
    # View the point forecasts
    #fc$mean
    # View the upper prediction interval
    #fc$upper
    # View the lower prediction interval
    #fc$lower
    
    # Plot the forecast
	if (chart == TRUE) {
		plot(fc)
	}
    
    #prediction is in fitted plus append mean
    predict <- fc$fitted
    for (i in seq(length(fc$mean))) {
      predict <- append(predict,fc$mean[i])
    }
    
    # replace any na's at beginning with input values
    for (i in seq(length(predict))) {
      if (!is.na(predict[i])) break;
      predict[i] <- in1norm[i]
    }
    predict <- predict(BN_OBJ, newdata = predict, warn = FALSE, inverse = TRUE)
    
    return(predict)
  }
  ################# end normalize test ########################
  
  
  ####### New combined predict 
  if (ptype == "hybrid" || ptype == "hybridw" || ptype == "hybridcv") {
    #message(paste0("Hybrid predict"))    
    #mod <- hybridModel(in1)
    if (ptype == "hybrid") 
      mod <- hybridModel(in1, models = usemod, verbose = FALSE,
              parallel = PARALLEL, num.cores = use_cores)	                          
    
    if (ptype == "hybridw")
      mod <- hybridModel(in1, models = usemod, weights = "insample.errors", verbose = FALSE,
              parallel = PARALLEL, num.cores = use_cores)	 
    
    # Cross-validated weight setting  can be very slow
  	if (ptype == "hybridcv") {
		  #message("Cross Validate hybrid predict - may take a while")
	    #wsize <- floor(length(in1) / 10) # 10 chunks
	   wsize <- 10 * ppb # 10 bar windows
	   # wsize = 3 * ppb
	    #message(paste0("hybridcv wsize=", as.character(wsize)), 
	     #       " Parallel=",PARALLEL," Use_cores=",use_cores)
	    mod <- hybridModel(in1, models = usemod,
	            weights = "cv.errors",
				      verbose = FALSE,
				      errorMethod = "RMSE",
	            cvHorizon =  ppb, windowSize = wsize,
				      #horizonAverage = TRUE, #looks like it doesn't improves
	            parallel = PARALLEL, num.cores = use_cores)
	    
		  #accuracy(mod)
	  }
    
    mod_fit <- tibble(fitted(mod,individual = TRUE))
    #print(mod_fit)
    
    # write csv for debugging
    #in1v <- enframe(in1,name=NULL)
    #modl1_tbl <- dplyr::bind_cols(in1v,mod_fit)
    #MODL1FILE <- paste0(AFLDIR, "modl1", ".csv") 
    #write_csv(modl1_tbl, MODL1FILE)
    
    fc <- forecast::forecast(mod, h=Plen)
    #print(fc$method)
    # View the point forecasts
    #fc$mean
    # View the upper prediction interval
    #fc$upper
    # View the lower prediction interval
    #fc$lower
   
     # Plot the forecast
    #plot(mod, type = "models")
    #plot(mod, type = "fit")    
    if (chart == TRUE) {
		plot(fc)
	}
    
    #prediction is in fitted plus append mean
    predict <- fc$fitted
    for (i in seq(length(fc$mean))) {
      predict <- append(predict,fc$mean[i])
    }
    
    # replace any na's at beginning with input values
    for (i in seq(length(predict))) {
      if (!is.na(predict[i])) break;
      predict[i] <- in1[i]
    }
    
    return(predict)
  }
  ################# end hybrid ########################
 
 ####### ptype  == tm caret/tidymodels not working ##################
  if (ptype == "caret") {
    log4r::debug(logger, paste0("Tidymodels predict"))
    message(paste0("TM predict"))
	  UNDEFINED()
	  
	  library(caret)
	  #library(forecast)
	  models_to_try <- c("lm", "rf", "gbm")  # Add other models as needed
	  N = plen
	  
	  df <- data.frame(in1 = in1[-length(in1)], in1_next = in1[-1])
	  
	  # Create training and testing sets
	  train_index <- 1:(0.80 * length(in1)) - 1
	  test_index <- 1:length(in1) - 1
	  train_data <- df[train_index, ]
	  test_data <- df[test_index, ]
	  
	  # Initialize caret's trainControl to define resampling method and tuning parameters
	  ctrl <- trainControl(method = "cv", number = 5)
	  
	  # Initialize variables for storing best model and its performance
	  best_model_name <- NULL
	  best_model <- NULL
	  best_performance <- Inf
	  
	  # Loop through each model to try
	  for (model_name in models_to_try) {
	    model_name = "arima"
	    # Define the model
	    model <- train(in1_next ~ ., data = train_data, method = model_name, trControl = ctrl)
      
	    ts_train <- ts(in1, frequency = 1)
	    model <-caret::train(ts_train, method = 'arima')
	    # Make predictions
	    #predictions <- forecast(model$finalModel, h = 2)$mean
	    #predictions <- forecast(model$finalModel, train_data, h = 2)$mean
	    p <- caret::forecast(model$finalModel, h = 2)
	    tmp1 <- tibble(test_data,p$mean)
	    view(tmp1)
	    

	    #predictions <- predict(model, newdata = train_data)
	    
	    # Evaluate performance (e.g., RMSE)
	    performance <- model$results$RMSE
	    
	    # Update best model if the current model performs better
	    if (performance < best_performance) {
	      best_model_name <- model_name
	      best_model <- model
	      best_performance <- performance
	    }
	  }
	  print(paste("best_model=",best_model_name,"best_performance=",best_performance))
    UNDEFINED()
    
    # Make predictions
    predictions <- predict(best_model, newdata = pred_data)
    view(predictions)
    
    ptib <- tibble(pred_data,predict=round(predictions,dec))
    #rename(glmnet = .pred)
    view(ptib)
    
    write_csv(ptib, "tmp1.csv")
    
    #ok now do plen predict
    UNDEFINED()
  }
  
#----------------------------------------------------------------------
  #caretForecast
  if (ptype == "cf1") {  
    # Load required libraries
    #library(caretForecast)
    
    #UNDEFINED()
    
    predict_N_ahead <- function(in1, N, resvpts, method) {
      train_data <- ts(in1[1: (length(in1) - resvpts) ])
      test_data <-  ts(in1[1: length(in1)])
      # fit <- invisible(ARml(train_data, caret_method = method, 
                            # max_lag = 12, verbose=TRUE)) # "lm"
      fit <- ARml(train_data, caret_method = method,
                  max_lag = 12, verbose=FALSE) %>% suppressMessages() # "lm"
      fc <- forecast(fit, h = resvpts + N) %>% suppressMessages()
      #autoplot(fc) + autolayer(test_data)
      #acc <- accuracy(fc, test_data)
      #rmse <- acc[4]
      #print(paste("RMSE=",rmse))
      return(fc$mean)
    }
    
    pred <- predict_N_ahead(in1, Plen, 2 * ppb, "rpart")
    pred <- round(pred,dec)
    in1s <- in1[1:(length(in1)-length(pred)+Plen)]
    preda <- append(in1s,pred)
    #view(preda)
    
    
    #in1ext <- c(in1,rep(NA,Plen))
    #ptib <- tibble(in1ext,preda)
    #print(tail(ptib,10))
    #write_csv(ptib, "tmp1.csv")
    return(preda)
  }
  
  if (ptype == "cf2") {  
    # Load required libraries
    #library(caretForecast)
    
    #UNDEFINED()
    
    
    predict_N_ahead <- function(in1, N, resvpts, method) {
      train_data <- ts(in1[1: (length(in1) - resvpts) ])
      test_data <-  ts(in1[1: length(in1)])
      fit <- ARml(train_data, caret_method = method, 
                            max_lag = 12, verbose=FALSE) %>% suppressMessages() # "lm"
      fc <- forecast(fit, h = resvpts + N) %>% suppressMessages()
      #autoplot(fc) + autolayer(test_data)
      #acc <- accuracy(fc, test_data)
      #rmse <- acc[4]
      #print(paste("RMSE=",rmse))
      return(fc$mean)
    }
    
    pred <- predict_N_ahead(in1, Plen, 2 * ppb, "glm")
    pred <- round(pred,dec)
    in1s <- in1[1:(length(in1)-length(pred)+Plen)]
    preda <- append(in1s,pred)
    return(preda)
  }
  
  if (ptype == "cf3") {  
    predict_N_ahead <- function(in1, N, resvpts, method) {
      train_data <- ts(in1[1: (length(in1) - resvpts) ])
      test_data <-  ts(in1[1: length(in1)])
      fit <- ARml(train_data, caret_method = method, 
                            max_lag = 12, verbose=FALSE) %>% suppressMessages() # "lm"
      fc <- forecast(fit, h = resvpts + N) %>% suppressMessages()
      return(fc$mean)
    }
    
    pred <- predict_N_ahead(in1, Plen, 2 * ppb, "lm")
    pred <- round(pred,dec)
    in1s <- in1[1:(length(in1)-length(pred)+Plen)]
    preda <- append(in1s,pred)
    return(preda)
  }
  if (ptype == "cf4") {  
    predict_N_ahead <- function(in1, N, resvpts, method) {
      train_data <- ts(in1[1: (length(in1) - resvpts) ])
      test_data <-  ts(in1[1: length(in1)])
      fit <- ARml(train_data, caret_method = method, 
                  max_lag = 12, verbose=FALSE) %>% suppressMessages() # "lm"
      fc <- forecast(fit, h = resvpts + N) %>% suppressMessages()
      return(fc$mean)
    }
    
    pred <- predict_N_ahead(in1, Plen, 2 * ppb, "knn")
    pred <- round(pred,dec)
    in1s <- in1[1:(length(in1)-length(pred)+Plen)]
    preda <- append(in1s,pred)
    return(preda)
  }
  if (ptype == "cf5") {  
    predict_N_ahead <- function(in1, N, resvpts, method) {
      train_data <- ts(in1[1: (length(in1) - resvpts) ])
      test_data <-  ts(in1[1: length(in1)])
      fit <- ARml(train_data, caret_method = method, 
                  max_lag = 12, verbose=FALSE) %>% suppressMessages() # "lm"
      fc <- forecast(fit, h = resvpts + N) %>% suppressMessages()
      return(fc$mean)
    }
    
    pred <- predict_N_ahead(in1, Plen, 2 * ppb, "svmLinear")
    pred <- round(pred,dec)
    in1s <- in1[1:(length(in1)-length(pred)+Plen)]
    preda <- append(in1s,pred)
    return(preda)
  }
  if (ptype == "cf6") {  
    predict_N_ahead <- function(in1, N, resvpts, method) {
      train_data <- ts(in1[1: (length(in1) - resvpts) ])
      test_data <-  ts(in1[1: length(in1)])
      fit <- ARml(train_data, caret_method = method, 
                  max_lag = 12, verbose=FALSE) %>% suppressMessages() # "lm"
      fc <- forecast(fit, h = resvpts + N) %>% suppressMessages()
      return(fc$mean)
    }
    
    pred <- predict_N_ahead(in1, Plen, 2 * ppb, "cforest")
    pred <- round(pred,dec)
    in1s <- in1[1:(length(in1)-length(pred)+Plen)]
    preda <- append(in1s,pred)
    return(preda)
  }
##########################################################################   
  if (ptype == "tm") {  
    # Function to predict N numbers ahead using caretForecast package
    predict_N_ahead <- function(in1, N) {
      # Convert data to time series
      ts_data <- ts(in1)
      
      # Create a training data frame
      train_df <- data.frame(time = time(ts_data), value = as.numeric(ts_data))
      
      # Fit a model
      model <- train(
        value ~ ., 
        data = train_df, 
        method = "nbagg", # You can choose different methods supported by caretForecast
        trControl = trainControl(method = "cv", number = 3) # Use cross-validation for training
      )
      
      # Forecast N numbers ahead
      forecast_result <- forecast(model$finalModel, h = N)
      
      # Return the forecasted values
      return(forecast_result)
    }
    
    pred <- predict_N_ahead(in1, Plen)
    pred <- round(pred,dec)
    view(pred)
    
    
    in1ext <- c(in1,rep(NA,length(pred) - length(in1)))
    ptib <- tibble(in1ext,pred)
    view(ptib)
    write_csv(ptib, "tmp1.csv")
    
    return(pred)
    

  } # End Caret/Tidymodels


	####### ptype  == nn ##################
	if (ptype == "nn") {
	  log4r::debug(logger, paste0("NN predict"))
	  #message(paste0("NN predict"))
	  
		best_fit <- nnetar(in1)
		best_forecast <- forecast::forecast(best_fit, h=Plen) # h controlls prediction length
		
		#if (!BACKTEST) {
		  #plot(best_forecast)
		#}
		
		# prediction now in best_forecast$mean
		#armapbp <- best_fit[["fitted"]]
		#for (i in 1:Plen) {
		#  armapbp <- append(armapbp, best_forecast$mean[i])
		#}
  
		armapbp <- fitted.values(best_forecast)
		predictvals <- best_forecast$mean
		predicta <- fitted.values(best_forecast)
		for (i in 1:length(predictvals)) {
			predicta <- append(predicta, predictvals[i])
		}
		predict <- predicta
		
		# replace any na's at beginning with input values
		for (i in seq(length(predict))) {
		  if (!is.na(predict[i])) break;
		  predict[i] <- in1[i]
		}
		return(predict)
	} # End NN

	if (ptype == "arma") {
	  log4r::debug(logger, paste0("Arma predict"))
	  #message(paste0("Arma predict"))
	  
		arma_inext <- in1
		for (i in 1:Plen) arma_inext <- append(arma_inext, NA)

		#fita1 <- auto.arima(in1) #faster
		fita1 <- auto.arima(in1, stepwise=FALSE, approximation=FALSE) #slower more accurate

		forea1 <- forecast::forecast(fita1, h=Plen)

		#extend lf1 with predicted has (lferr)
		armap <- in1
		armapbp <- forea1$fitted
    armapbp <- append(armapbp,forea1$mean)
		predict <- armapbp

    # replace any na's at beginning with input values
    for (i in seq(length(predict))) {
      if (!is.na(predict[i])) break;
      predict[i] <- in1[i]
    }
 
  	return(predict)
	} # End Arma
  
    if (ptype == "emd" ) {
	 theme_set(theme_minimal())
    TITLEB <<- "E"
    Plen <<- 2

    pred <- emd_predict2(in1)
	return(pred)
  } 
  
  log4r::debug(logger, paste0("ERROR: Invalid Predict type"))
  message(paste0("ERROR: Invalid Predict type"))
  stop()
  
} # End an_predict2
###############################################

# Smoothing Filters
# none (input is returned)
# mstl 
# atl = auto tuned loess - old tune
# atl2 - auto tuned loess - larry's new tune
# lm = ouch - not useful
# sc = smoothcombine - very slow
# zlma - usually the best
# ma - MovAvg
# ks = Kernsmooth - auto
# ss - super smoother - not Ehlers
# es - from library smooth
# mf - mFilter 
# aspline - autotune spline wsize=nbr most recent points to tune over
# alp - autotune lowpass fft filter

#filter_func <- function (input, ft = "none", matype = "s", maper = 4, detail = FALSE) {
filter_func <- function (input, ft = "none", 
            maper=6, matype="s", wsize=20*ppb, ttype="er", detail = FALSE) {  
  
  if (ft == "none") { # no filtering
    return(input)
  }
  
  if (ft == "mf") { # mfilter
    mf <- mFilter(as.ts(input), filter="HP")
    return(mf$trend)
  }
  
  if (ft == "ss") { # supersmoother
    x <- 1:(length(input))
    y <- input
    out <- supsmu(x=x, y=y, span = "cv", bass=0, trace=FALSE)
    return(out$y)
  }
  
  
  if (ft == "ks") { # kernsmooth
    gridsize <- length(input)
    x <- 1:(length(input))
    y <- input
    bw <- dpill(x=x,y=y,gridsize)
    lp <- locpoly(x=x, y=y,bandwidth=bw,gridsize=gridsize)
    smooth <- lp$y
    return(smooth)
  }
  
  if (ft == "mstl") { # mstl
    fit <- mstl(input)
    return(fit[,2]) # Trend
  }
  
  # From chatGPT 03/09.2024
  # Function to auto-tune LOESS filter
  auto_tune_loess <- function(in1, span_range) {
    # Initialize variables
    min_mse <- Inf
    best_span <- NULL
    best_model <- NULL
    
    # Iterate over span values
    for (span in span_range) {
      # Fit LOESS model
      loess_fit <- loess(in1 ~ seq_along(in1), span = span)
      
      # Predict using LOESS model
      loess_pred <- predict(loess_fit)
      
      # Calculate mean squared error
      mse <- mean((in1 - loess_pred)^2)
     
      # Update best span if MSE is lower
      if (mse < min_mse) {
        min_mse <- mse
        best_span <- span
        best_model <- loess_fit
        #print(paste(best_span,min_mse))
      }
    }
    return(best_model)
  }
  
  if (ft == "atl9") { # auto tune loess from chat GPT
  
    # Range of span values to try
    span_range <- seq(0.08, 0.20, by = 0.01)
    
    # Auto-tune LOESS filter
    loess_model <- auto_tune_loess(in1, span_range)
  
    # Predict using the final LOESS model
    loess_pred <- predict(loess_model)
    tmp <- tibble(in1,loess_pred)
    #write_csv(tmp,"tmp.csv")
    
    return(loess_pred)
  }
  
  if (ft == "atl") { # auto tune loess
    index <- 1:(length(input))  # create index variable
    ldata <- as.data.frame(cbind(index,input))
    colnames(ldata) <- c("index", "loessin")
    #08/04/2019 was next line - always tuned to 0.7999 so made it 0.8
    # See loess below 
    #o1 <- optim(par=c(0.5), calcSSE, lower=0.1, upper=0.8, method="Brent")
    #o1 <- optim(par=c(0.5), calcSSE, method="SANN")
    
    #message(paste0("Loess Best tune parm= "),o1$par)
    
    #loessMod1 <- loess(loessin ~ index, data=ldata, span=o1$par)
    loessMod1 <- loess(loessin ~ index, data=ldata, span=0.8)
    loess1 <- predict(loessMod1) 
    return(loess1)
  }
  
  if (ft == "atl2") { # auto tune loess - Larry's new tune
    loess1 <- s_loess(input=input, span=0.00, detail = detail)
    return(loess1)
  }
  
  if (ft == "lm") { # lm
    index <- 1:(length(input))  # create index variable
    in1_df <- as.data.frame(cbind(index,input) )
    fit1 <- lm(index ~ input)
    fitted1 <- fitted(fit1)
    return(fitted1)
  }
  
  if (ft == "sc") { # smoothcombine
    ourModel <- smoothCombine(input,h=ppb,silent="none")
    if (detail == TRUE) plot(ourModel)
    return(as.numeric(ourModel$fitted)) 
  }
  
  
  if (ft == "es") { # requires library smooth
    ourModel <- es(input,h=ppb,silent="none")
    plot(ourModel)
    return(as.numeric(ourModel$fitted)) 
  }
  
  if (ft == "zlma") { # zlma
    maper1 <- 3 * ppb
    matype1 <- "s" 
    
    ma1a <- movavg(input, maper1, matype1)
    ma1b <- movavg(ma1a, maper1, matype1)
    d1 <- 2.0 * (ma1a - ma1b)
    ma1 <- ma1b + d1
    return(ma1) 
  }
  
  if (ft == "zlma2") { # zlma2
    maper1 <- 2 * ppb
    matype1 <- "s" 
    
    ma1a <- movavg(input, maper1, matype1)
    ma1b <- movavg(ma1a, maper1, matype1)
    d1 <- 2.0 * (ma1a - ma1b)
    ma1 <- ma1b + d1
    return(ma1) 
  }
  
  if (ft == "ma") { # ma
    maper1 <- 3 * ppb
    matype1 <- "w" 
    ma1 <- movavg(input, maper1, matype1)
    return(ma1) 
  }
  
  if (ft == "azlma") { # auto-tune zero-lag ma
    print(paste0("AZLMA auto-tune zero-lag moving average"))
    maper <- matune(input=input,matype=matype,wsize=wsize,ttype=ttype,detail=detail)
    ma1a <- movavg(input, maper, matype)
    ma1b <- movavg(ma1a, maper, matype)
    d1 <- 2.0 * (ma1a - ma1b)
    ma1 <- ma1b + d1
    return(ma1) 
  }
  
  
  if (ft == "aspline") { # auto tune spline - Larry's new tune
    spline1 <- s_spline(input=input, wsize=wsize, ttype=ttype, detail = detail)
    return(spline1)
  }
  
  if (ft == "alp") { # auto tune lowpass fft - Larry's new tune
    best_freq <- lptune(input=input, wsize=wsize, ttype=ttype, detail = detail)
    best_f1 <- smooth.fft(input, best_freq)
    return(best_f1)
  }
  
  message ("ERROR: Invalid filter type code")
  return(NULL)
} # end filt_func

 

#####################################
lptune <- function(input, wsize, ttype, detail) {
  
  tune_length <- wsize
  
  errsv <- 99999999.0
  best_parm <- 0
  
  for (freq in seq( 0.05, 0.50, by=0.01)) {
    
    f1 <- smooth.fft(input, freq)
    resa <- input - f1
    res <- tail(resa,tune_length)
    
    residtib <- enframe(res)
    
    # do resid plus side
    residtibap <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value > 0, value, NA))
    
    residtibbp <- residtibap %>% drop_na()
    
    cntp <- nrow(residtibbp)
 
    rmsep <- maxep <- 0.0
    if (cntp > 0) {
      rmsep <- RMSE(residtibbp$value)
      maxep <- MAXE(abs(residtibbp$value))
    }
    
    # do resid minus side
    residtiban <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value < 0, value, NA))
    
    residtibbn <- residtiban %>% drop_na()
    
    cntn <- nrow(residtibbn)
  
    rmsen <- maxen <- 0.0
    if (cntn > 0) {
      rmsen <- RMSE(residtibbn$value)
      maxen <- MAXE(residtibbn$value)
    }
    
    if (detail == TRUE) print(paste0("maxep=",maxep," maxen=",maxen))
    if (detail == TRUE) print(paste0("cntp=",cntp," cntn=",cntn))
    
    err <- 9999999.0
    if (ttype == "ec") err <- abs(cntp - cntn) # tune by equal count
    if (ttype == "er") err <- abs(rmsep - rmsen) # tune by equal rmse
    if (ttype == "em") err <- abs(maxep - maxen) # tune by equal maxe
    
    if ( err < errsv) {
      if (detail == TRUE) print(paste0("err=",err," errsv=",errsv))
      errsv <- err
      best_parm <- freq
      if (detail == TRUE) {
        print(paste0(" FFT AutoTune type= ", ttype, "  Tune Window= ", tune_length))
        print(paste0("   parm  cnt rmse max"))
        print(paste0("   ",best_parm,"   ",cntp, "  ", rmsep,"  ",maxep))
        print(paste0("   ",best_parm,"   ",cntn, "  ", rmsen,"  ",maxen))
      }
    }
  }
  return(best_parm)
}

#####################################
matune <- function(input, matype, wsize, ttype, detail) {
  
  tune_length <- wsize
  
  errsv <- 999999
  best_parm <- 0
  
  for (t_per in seq( 2 * ppb, 10 * ppb, by=1)) {
    
    ma1a <- movavg(input, t_per, matype)
    ma1b <- movavg(ma1a, t_per, matype)
    d1 <- 2.0 * (ma1a - ma1b)
    ma1 <- ma1b + d1
    resa <- input - ma1
    res <- tail(resa,tune_length)
    
    residtib <- enframe(res)
    
    # do resid plus side
    residtibap <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value > 0, value, NA))
    
    residtibbp <- residtibap %>% drop_na()
    
    cntp <- nrow(residtibbp)
    
    rmsep <- RMSE(residtibbp$value)
    maxep <- MAXE(residtibbp$value)
    
    # do resid minus side
    residtiban <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value < 0, value, NA))
    
    residtibbn <- residtiban %>% drop_na()
    
    cntn <- nrow(residtibbn)
    
    rmsen <- RMSE(residtibbn$value)
    maxen <- MAXE(residtibbn$value)
    
    if (ttype == "ec") err <- abs(cntp - cntn) # tune by equal count
    if (ttype == "er") err <- abs(rmsep - rmsen) # tune by equal rmse
    if (ttype == "em") err <- abs(maxep - maxen) # tune by equal maxe
    
    if (err < errsv) {
      errsv <- err
      best_parm <- t_per
      print(paste0(" AutoSpline Tumetype= ",ttype," Tune Window= ", tune_length))
      print(paste0("+-  parm  cnt rmse max"))
      print(paste0("+  ",best_parm,"  ",cntp, "  ", rmsep,"  ",maxep))
      print(paste0("-  ",best_parm,"  ",cntn, "  ", rmsen,"  ",maxen))
    }
  }
  return(best_parm)
}

#####################################
stune <- function(input, wsize, ttype, detail) {
  
  tune_length <- wsize
  
  errsv <- 999999
  best_parm <- 0
  
   for (sm_parm in seq(0.70, 0.95, by=0.01)) {
   
    f1.s <- smooth.spline(input, spar = sm_parm)
    f1 <- f1.s$y
    resa <- input - f1
    res <- tail(resa,tune_length)
    
    residtib <- enframe(res)
    
    # do resid plus side
    residtibap <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value > 0, value, NA))
    
    residtibbp <- residtibap %>% drop_na()
    
    cntp <- nrow(residtibbp)
    
    rmsep <- RMSE(residtibbp$value)
    maxep <- MAXE(residtibbp$value)
    
    # do resid minus side
    residtiban <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value < 0, value, NA))
    
    residtibbn <- residtiban %>% drop_na()
    
    cntn <- nrow(residtibbn)
    
    rmsen <- RMSE(residtibbn$value)
    maxen <- MAXE(residtibbn$value)
    
    if (ttype == "ec") err <- abs(cntp - cntn) # tune by equal count
    if (ttype == "er") err <- abs(rmsep - rmsen) # tune by equal rmse
    if (ttype == "em") err <- abs(maxep - maxen) # tune by equal maxe
   
    if (err < errsv) {
      errsv <- err
      best_parm <- sm_parm
      print(paste0(" AutoSpline Tumetype= ",ttype," Tune Window= ", tune_length))
      print(paste0("+-  parm  cnt rmse max"))
      print(paste0("+  ",sm_parm,"  ",cntp, "  ", rmsep,"  ",maxep))
      print(paste0("-  ",sm_parm,"  ",cntn, "  ", rmsen,"  ",maxen))
    }
  }
  return(best_parm)
}


s_spline <- function(input,wsize,ttype,detail)
{
  tspar <- stune(input=input,wsize=wsize,ttype=ttype,detail=detail)
  f1.s <- smooth.spline(input, spar = tspar)
  f1 <- f1.s$y
  return(f1)
}


# for loess auto tune
# define function that returns the SSE
calcSSE <- function(x) {
  sse <- 9999999
  loessMod <- try(loess(loessin ~ index, data=ldata, span=x), silent=TRUE)
  res <- try(loessMod$residuals, silent=TRUE)
  if(class(res)!="try-error") {
    if((sum(res, na.rm=TRUE) > 0)) {
      sse <- sum(res^2)
    }
  }
  else {
    sse <- 9999999
  }
  return(sse)
}

# for loess auto tune
# define function that returns the SSE
# requires data frame ldata with columns index ..data..

#####################################
#####################################
ltune <- function(input) {
  
  tune_length <- 40 * ppb
  
  errsv <- 999999
  best_parm <- 0
  
  index <- 1:(length(input))  # create index variable
  ldata <- as.data.frame(cbind(index,input))
  
  for (sm_parm in seq(0.10, 0.50, by=0.01)) {
    
    loessMod1  <- loess(input ~ index, data=ldata, span=sm_parm)
    f1 <- predict(loessMod1) 
    resa <- input - f1
    res <- tail(resa,tune_length)
    
    residtib <- enframe(res)
    
    # do resid plus side
    residtibap <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value > 0, value, NA))
    
    residtibbp <- residtibap %>% drop_na()
    
    cntp <- nrow(residtibbp)
    
    rmsep <- RMSE(residtibbp$value)
    maxep <- MAXE(residtibbp$value)
    
    # do resid minus side
    residtiban <- residtib %>%
      dplyr::select(value) %>%
      dplyr::mutate(value = ifelse(value < 0, value, NA))
    
    residtibbn <- residtiban %>% drop_na()
    
    cntn <- nrow(residtibbn)
    
    rmsen <- RMSE(residtibbn$value)
    maxen <- MAXE(residtibbn$value)
    
    #err <- abs(cntp - cntn) # tune by equal count
    err <- abs(rmsep - rmsen) # tune by equal rmse
    #err <- abs(maxep - maxen) # tune by equal maxe
    
    if (err < errsv) {
      errsv <- err
      best_parm <- sm_parm
      #print(paste0("+-  parm  cnt rmse max"))
      #print(paste0("+  ",sm_parm,"  ",cntp, "  ", rmsep,"  ",maxep))
      #print(paste0("-  ",sm_parm,"  ",cntm, "  ", rmsem,"  ",maxem))
    }
  }
  return(best_parm)
}

# single input loess 
s_loess <- function (input, span, detail)
{
  input_name <- deparse(substitute(input))
  index <- 1:(length(input))  # create index variable
  ldata <- as.data.frame(cbind(index,input))
  if (span > 0.0) {
    loessMod1 <- loess(input ~ index, data=ldata, span=span)
  }
  if (span == 0.0) {
    o1 <- ltune(input)
    if (detail == TRUE) print(paste0(input_name," Loess Best tune parm= ", o1))
    loessMod1 <- loess(input ~ index, data=ldata, span=o1)
  }
  smooth <- predict(loessMod1) 
  return(smooth)
}

# Predict types
# "none" arma" "nn" 
# "norm" "normw" "normcv"
# "hybrid" "hybridw" "hybridcv"
#PTYPE1 <- "nn"
#PTYPE2 <- "none"
# 2 level predict with charts
predict2 <- function (in1, ptype1 = "arma", ptype2 = "none", wsize = wsize,
                      chart = FALSE, maintitle="???", title = "???", xstart=1)
{
  input_name <- deparse(substitute(in1))
  #message(paste0("Level 1 Predicting: ", input_name))
  
  titlea <- paste0(title," Level 1")
  
  predict1 <- an_predict2(in1, ptype1, wsize) 
  
  xend = length(predict1)
  
  tmp <- head(predict1,length(in1))
  r1 <- in1 - tmp
  rmse <- RMSE(r1)
  maxe <- MAXE(r1)
  paste0(titlea, " Error ", rmse, " ", maxe)
  
  # Plot first level
  if (chart == TRUE) {
    final_plots <- list()
    
    index <- 1:(length(predict1))  # create index variable
    in1ext <- c(in1,rep(NA,ppb))
    #pplot <- as_tibble(index,in1ext,predict1)
    pplot <- tibble(index,in1ext,predict1) # LRC 015/14/2023
    
    g1 <- ggplot(pplot, aes(x=index)) +
      geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
      geom_line(aes(y = in1ext), colour="black") +
      geom_line(aes(y = predict1), colour="blue") +
      geom_point(aes(y = predict1), colour="blue") +
      labs(title = maintitle, y = titlea, x = "" ) +
      coord_cartesian(xlim=c(xstart,xend)) # zoom
    
    #print(g1)
    
    final_plots[[1]] <- g1
    
    ############## resid @@@@@@@@@@@@@@@@@@@@@@@@
    titleb <- paste0(titlea," Resid")
    
    r1ext <- c(r1,rep(NA,ppb))
    #pplot <- as_tibble(index,r1ext)
    pplot <- tibble(index,r1ext)
    
    g2 <- ggplot(pplot, aes(x=index)) +
      geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
      geom_hline(aes(yintercept = 0), colour="black") +      
      geom_line(aes(y = r1ext), colour="black") +
      geom_point(aes(y = r1ext), colour="black", size = 1.00) +
      labs(title = "", y = titleb, x = "" ) +
      coord_cartesian(xlim=c(xstart,xend)) # zoom
    
    #print(g2)
    
    final_plots[[2]] <- g2
    
    multiplot(plotlist=final_plots, cols=1)
  }
  
  #########################
  paste0(titlea, " Error ", rmse, " ", maxe)
  predicted <- predict1
  
  ############ 2nd level ###################
  if (ptype2 != "none") {
    
    #message(paste0("Level 2 Predicting: ", input_name))
    
    predict2 <- an_predict2(r1, ptype2, wsize) 
    
    tmp <- head(predict2,length(in1))
    r2 <- r1 - tmp
    rmse2 <- RMSE(r2)
    maxe2 <- MAXE(r2)
    
    # Plot first level
    if (chart == TRUE) {
      final_plots <- list()
      
      titleb <- paste0(titlea," Resid 2")
      index <- 1:(length(predict2))  # create index variable
      r1ext <- c(r1,rep(NA,ppb))
      #pplot <- as_tibble(index,r1ext,predict2)
      pplot <- tibble(index,r1ext,predict2)
      
      g1 <- ggplot(pplot, aes(x=index)) +
        geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
        geom_line(aes(y = r1ext), colour="black") +
        geom_line(aes(y = predict2), colour="red") +
        geom_point(aes(y = predict2), colour="red") + 
        labs(title = maintitle, y = titlea, x = "" ) +
        coord_cartesian(xlim=c(xstart,xend)) # zoom
      
      #print(g1)
      
      final_plots[[1]] <- g1
      
      ############## resid @@@@@@@@@@@@@@@@@@@@@@@@
      r2ext <- c(r2,rep(NA,ppb))
      #pplot <- as_tibble(index,r2ext)
      pplot <- tibble(index,r2ext)      
      
      g2 <- ggplot(pplot, aes(x=index)) +
        geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
        geom_hline(aes(yintercept = 0), colour="black") +
        geom_line(aes(y = r2ext), colour="black") +
        geom_point(aes(y = r2ext), colour="black", size = 1.00) +        
        labs(title = "", y = titleb, x = "" ) +
        coord_cartesian(xlim=c(xstart,xend)) # zoom
      
      #print(g2)
      
      final_plots[[2]] <- g2
      
      multiplot(plotlist=final_plots, cols=1)
    }
    
    predicted <- predict1 + predict2
    # Larry fix this - both saying Level 1 Error
    #message(paste0(titlea, " Error ", rmse, " ", maxe))
    #message(paste0(titlea, " Error ", rmse2, " ", maxe2))
  }
  
  return(predicted)
} # end predict2

emd_predict <- function (in1, ptype1="hybrid", ptype2="none", wsize=20,
                         title = "Needs title", chart = TRUE, xstart = 1)
{
  #############################################
  # Decompose with CEEMDAN 
  #############################################
  cemd_in <- in1
  
  imfs1 <- ceemdan(cemd_in) # uses defaults
  
  #imfs1 <- ceemdan(cemd_in, num_siftings = 10, ensemble_size = 250, threads = 1)
  #imfs1 <- ceemdan(cemd_in, ensemble_size = 250,  noise_strength = 0.20)
  #imfs1 <- ceemdan(cemd_in, ensemble_size = EMD_ES, noise_strength = EMD_NS)
  #imfs1 <- eemd(cemd_in)
  #imfs1 <- emd(cemd_in) # Possibility
  #imfs1 <- bemd(cemd_in)
  
  ######### Plot Input #################
  index <- 1:(length(cemd_in))  # create index variable
  pplot <- as_tibble(cbind(index,cemd_in))
  
  g1 <- ggplot(pplot, aes(x=index)) +
    geom_line(aes(y = cemd_in), colour="black") +
    labs(title = TITLEB, y = "EMD INPUT", x = "Index" )
  
  suppressWarnings( print(g1) ) # surpress warnings about NAs
  
  if (chart == TRUE) plot(imfs1) # Plots IMF's
  
  nbrcols = ncol(imfs1)
  
  pred_cols <- matrix(nrow = (length(cemd_in)+Plen), ncol = nbrcols)
  err_cols <- matrix(nrow = length(cemd_in), ncol = nbrcols)
  rmse_err <- matrix(nrow = nbrcols, ncol = 2)
  
  #pred_cols <- NULL
  #err_cols <- NULL
  #rmse_err <- NULL
  
  # esch imf predict -> pred_cols
  # each inf predict error -> err_cols
  # each RMSE error -> rmse_err[]
  names <- colnames(imfs1)
  
  START_COL <-1
  
  for (i in seq(from= START_COL, to=nbrcols, by=1)) {
    name <- names[i]
    data <- imfs1[,i]
    #data <- as.numeric(imfs1[,i])
    #pred_cols[,i] <- an_predict2(data,PTYPE)
    pred_cols[,i] <- predict2(data, ptype1=ptype1, ptype2=ptype2, wsize=wsize,
                              chart = TRUE, maintitle = title, title = name, xstart = xstart) 
    tmp <- head( pred_cols[,i],length(data))
    err_cols[,i] <- data - tmp
    rmse_err[i,1] <- name
    #rmse_err[i,2] <- round(rmse(data,tmp),3)
    rmse_err[i,2] <- RMSE(data - tmp)
  }
  
  colnames(rmse_err) <- c("imf","rmse")
  #error_tbl <- as_tibble(rmse_err)
  error_tbl <- tibble(rmse_err)
  error_tbl <- arrange(error_tbl, desc(rmse))
  
  #print(error_tbl)
  
  # save our plots so we can put imf predicts and errors on 2 pages
  pred_plots <- list()
  error_plots <- list()
  
  ##### Plot each imf and predict and error(resid)
  for (i in seq(from= START_COL, to=nbrcols, by=1)) {
    name <- names[i]
    pred <-  pred_cols[,i]
    error <- err_cols[,i]
    
    index <- 1:(length(pred))  # create index variable
    pricel <- imfs1[,i]
    pricel <- append(pricel,rep(NA,Plen))
    
    pplot <- as_tibble(cbind(index, pricel, pred))
    labele <- paste0(name," Predict")
    
    g1 <- ggplot(pplot, aes(x=index)) +
      geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
      geom_line(aes(y = pricel), colour="black") +
      geom_line(aes(y = pred), colour="red") +
      ylab(label=labele) +
      xlab("") 
    if (i == 1) g1 <- g1 + labs(title = title)
    
    pred_plots[[i]] <- g1
    
    ############################################
    index <- 1:(length(error))  # create index variable
    pplot <- as_tibble(cbind(index,error))
    labele <- paste0(name," Error")
    
    g1 <- ggplot(pplot, aes(x=index)) +
      geom_line(aes(y = error), colour="black") +
      ylab(label=labele) +
      xlab("") 
    if (i == 1) g1 <- g1 + labs(title = title) 
    
    error_plots[[i]] <- g1
  }
  
  #pred_tbl <- as_tibble(pred_cols)
  pred_tbl <- tibble(pred_cols)
  
  pred_tbl <- pred_tbl %>% 
    mutate(predictf = rowSums(.)) %>% 
    mutate(input=pricel)
  
  tmp <- head(pred_tbl$predictf,length(cemd_in))
  resid <- cemd_in - tmp
  
  # add resid column
  pred_tbl <- pred_tbl %>% 
    mutate(resid = input - predictf) 
  
  # write predict table of predict to csv file
  #TMPFILE <- paste0(AFLDIR, "pred_tbl.csv")
  #write_csv(pred_tbl,TMPFILE)
  
  # Page prints
  multiplot(plotlist=error_plots, cols=2)
  multiplot(plotlist=pred_plots, cols=2)
  
  final_plots <- list()
  ###### Now input and final predict
  pred <- pred_tbl$predictf
  index <- 1:(length(pred))  # create index variable
  pricel <- cemd_in
  pricel <- append(pricel,rep(NA,Plen))
  pplot <- as_tibble(cbind(index, pricel, pred))
  
  g1 <- ggplot(pplot, aes(x=index)) +
    geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
    geom_line(aes(y = pricel), colour="black") +
    geom_line(aes(y = pred), colour="red") +
    geom_point(aes(y = pred), colour="red") +
    labs(title = title, y = "Final Predict", x = "" ) +
    coord_cartesian(xlim=c(xstart,xend)) # zoom
  
  #suppressWarnings( print(g1) ) # surpress warnings about NAs
  final_plots[[1]] <- g1
  
  ############################################
  index <- 1:(length(resid))  # create index variable
  
  pplot <- as_tibble(cbind(index,resid))
  
  g2 <- ggplot(pplot, aes(x=index)) +
    geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
    geom_line(aes(y = resid), colour="black") +
    geom_point(aes(y = resid), colour="black") +
    labs(title = "", y = "Final Resid", x = "" ) +
    coord_cartesian(xlim=c(xstart,xend)) # zoom
  
  #suppressWarnings( print(g2) ) # surpress warnings about NAs
  final_plots[[2]] <- g2
  
  multiplot(plotlist=final_plots, cols=1)
  
  print(error_tbl)
  
  rmset <- sum(as.numeric(error_tbl$rmse))
  
  #print(paste0("RMSE Total Error ", rmset))
  
  tail(in1,2*ppb)
  tail(pred,3*ppb)
  #tail(datein1, 2*ppb)
  print(paste0("RMSE Total Error ", rmset))
  
  return(as.numeric(pred))
 }
 
 
# with minmove
# if < 2 * minmove returns zeros use ptype2 (hybrid) only for larger imfs
# for each imf:
#  MAXE(imf) <= 1 * minmove return zeros
#  MAXE(imf) > minmove && MAXE(imf) <= 4 * mimove use ptype1)
#  MAXE(imf) > 4 * minmove use ptype2
# 06/03/2023  was ptype2="hybridcv" chart = TRUE
emd_predict2 <- function (in1, ptype1="arma", ptype2="arma", wsize=20,
                          title = "Needs title", chart = FALSE, xstart = 1, minmove = 0.25)
 {
   #############################################
   # Decompose with CEEMDAN 
   #############################################
   cemd_in <- in1
   maxe <- MAXE(cemd_in)
   zeros <- rep(0, length(cemd_in) + ppb)
                
   if (maxe < 2*minmove) return(zeros)
   
   imfs1 <- ceemdan(cemd_in) # uses defaults
   
   #imfs1 <- ceemdan(cemd_in, num_siftings = 10, ensemble_size = 250, threads = 1)
   #imfs1 <- ceemdan(cemd_in, ensemble_size = 250,  noise_strength = 0.20)
   #imfs1 <- ceemdan(cemd_in, ensemble_size = EMD_ES, noise_strength = EMD_NS)
   #imfs1 <- eemd(cemd_in)
   #imfs1 <- emd(cemd_in) # Possibility
   #imfs1 <- bemd(cemd_in)
   
   ######### Plot Input #################
   index <- 1:(length(cemd_in))  # create index variable
   pplot <- as_tibble(cbind(index,cemd_in))
   
   g1 <- ggplot(pplot, aes(x=index)) +
     geom_line(aes(y = cemd_in), colour="black") +
     labs(title = TITLEB, y = "EMD INPUT", x = "Index" )
   
   #suppressWarnings( print(g1) ) # surpress warnings about NAs
   
   if (chart == TRUE) plot(imfs1) # Plots IMF's
   
   nbrcols = ncol(imfs1)
   
   pred_cols <- matrix(nrow = (length(cemd_in)+Plen), ncol = nbrcols)
   err_cols <- matrix(nrow = length(cemd_in), ncol = nbrcols)
   rmse_err <- matrix(nrow = nbrcols, ncol = 2)
   
   #pred_cols <- NULL
   #err_cols <- NULL
   #rmse_err <- NULL
   
   # esch imf predict -> pred_cols
   # each inf predict error -> err_cols
   # each RMSE error -> rmse_err[]
   names <- colnames(imfs1)
   
   START_COL <-1
   
  for (i in seq(from= START_COL, to=nbrcols, by=1)) {
     name <- names[i]
     data <- imfs1[,i]
     #data <- as.numeric(imfs1[,i])
     #pred_cols[,i] <- an_predict2(data,PTYPE)
     
     #  MAXE(imf) <= 1 * minmove return zeros
     #  MAXE(imf) > minmove && MAXE(imf) <= 4 * mimove use ptype1)
     #  MAXE(imf) > 4 * minmove use ptype2
     rmse <- RMSE(data)
     maxe1 <- MAXE(data)
     
     maxe2 <- MAXE(diff(data,lag = 1, differences = 1))
     #maxe2 <- MAXE(data)
     #print(paste0("imf",i," RMS= ", rmse, " MAX1= ",maxe1," MAX2= ",maxe2))
     
    if (maxe1 < minmove) {
      #print(paste0("Predict just using zeros "))
      pred_cols[,i] <- zeros
    } else {
      if (maxe2 <= 4 * minmove) {
        #print(paste0("Using ", ptype1, " predict"))
        pred_cols[,i] <- predict2(data, ptype1=ptype1, ptype2="none", wsize=wsize,
                 chart = chart, maintitle = title, title = name, xstart = xstart) 
      } else {
        #print(paste0("Using ", ptype2, " predict"))
        pred_cols[,i] <- predict2(data, ptype1=ptype2, ptype2="none", wsize=wsize,
                                 chart = chart, maintitle = title, title = name, xstart = xstart) 
      }
    }
     
    tmp <- head( pred_cols[,i],length(data))
    err_cols[,i] <- data - tmp
    rmse_err[i,1] <- name
    #rmse_err[i,2] <- round(rmse(data,tmp),3)
    rmse_err[i,2] <- RMSE(data - tmp)
     
    rmse <- RMSE(data - tmp)
    maxe <- MAXE(data - tmp)
    #print(paste0("imf",i," RMSE= ", rmse, " MAXE= ",maxe))
   }
   
   colnames(rmse_err) <- c("imf","rmse")
   #error_tbl <- as_tibble(rmse_err)
   error_tbl <- tibble(rmse_err)
   error_tbl <- arrange(error_tbl, desc(rmse))
   
   #print(error_tbl)
   
   # save our plots so we can put imf predicts and errors on 2 pages
   pred_plots <- list()
   error_plots <- list()
   
   ##### Plot each imf and predict and error(resid)
   for (i in seq(from= START_COL, to=nbrcols, by=1)) {
     name <- names[i]
     pred <-  pred_cols[,i]
     error <- err_cols[,i]
     
     index <- 1:(length(pred))  # create index variable
     pricel <- imfs1[,i]
     pricel <- append(pricel,rep(NA,Plen))
     
     pplot <- as_tibble(cbind(index, pricel, pred))
     labele <- paste0(name," Predict")
     
     g1 <- ggplot(pplot, aes(x=index)) +
       geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
       geom_line(aes(y = pricel), colour="black") +
       geom_line(aes(y = pred), colour="red") +
       ylab(label=labele) +
       xlab("") 
     if (i == 1) g1 <- g1 + labs(title = title)
     
     pred_plots[[i]] <- g1
     
     ############################################
     index <- 1:(length(error))  # create index variable
     pplot <- as_tibble(cbind(index,error))
     labele <- paste0(name," Error")
     
     g1 <- ggplot(pplot, aes(x=index)) +
       geom_line(aes(y = error), colour="black") +
       ylab(label=labele) +
       xlab("") 
     if (i == 1) g1 <- g1 + labs(title = title) 
     
     error_plots[[i]] <- g1
   }
   
   #pred_tbl <- as_tibble(pred_cols)
   pred_tbl <- tibble(pred_cols)
   
   pred_tbl <- pred_tbl %>% 
     mutate(predictf = rowSums(.)) %>% 
     mutate(input=pricel)
   
   tmp <- head(pred_tbl$predictf,length(cemd_in))
   resid <- cemd_in - tmp
   
   # add resid column
   pred_tbl <- pred_tbl %>% 
     mutate(resid = input - predictf) 
   
   # write predict table of predict to csv file
   #TMPFILE <- paste0(AFLDIR, "pred_tbl.csv")
   #write_csv(pred_tbl,TMPFILE)
   
   # Page prints
   if (chart == TRUE) multiplot(plotlist=error_plots, cols=2)
   if (chart == TRUE) multiplot(plotlist=pred_plots, cols=2)
   
   final_plots <- list()
   ###### Now input and final predict
   pred <- pred_tbl$predictf
   index <- 1:(length(pred))  # create index variable
   pricel <- cemd_in
   pricel <- append(pricel,rep(NA,Plen))
   pplot <- as_tibble(cbind(index, pricel, pred))
   #pplot <- tibble(index, pricel, pred) # LRC
   xend <- length(pred) #LRC
   
   g1 <- ggplot(pplot, aes(x=index)) +
     geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
     geom_line(aes(y = pricel), colour="black") +
     geom_line(aes(y = pred), colour="red") +
     geom_point(aes(y = pred), colour="red") +
     labs(title = title, y = "Final Predict", x = "" ) +
     coord_cartesian(xlim=c(xstart,xend)) # zoom
   
   #suppressWarnings( print(g1) ) # surpress warnings about NAs
   final_plots[[1]] <- g1
   
   ############################################
   index <- 1:(length(resid))  # create index variable
   
   pplot <- as_tibble(cbind(index,resid))
   
   g2 <- ggplot(pplot, aes(x=index)) +
     geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
     geom_line(aes(y = resid), colour="black") +
     geom_point(aes(y = resid), colour="black") +
     labs(title = "", y = "Final Resid", x = "" ) +
     coord_cartesian(xlim=c(xstart,xend)) # zoom
   
   #suppressWarnings( print(g2) ) # surpress warnings about NAs
   final_plots[[2]] <- g2
   
    if (chart == TRUE) multiplot(plotlist=final_plots, cols=1)
   
   #print(error_tbl)
   
   #rmset <- sum(as.numeric(error_tbl$rmse))
   
   #print(paste0("RMSE Total Error ", rmset))
   
   #tail(in1,2*ppb)
   #tail(pred,3*ppb)
   #tail(datein1, 2*ppb)
   #print(paste0("RMSE Total Error ", rmset))
   
   return(as.numeric(pred))
 }
 
#  like emd_predict2 except
# checks for low rms and predicts zero if so
# predicts using ptype1 and ptype2 - usually "arma" & "none"
# if resid > 4 * minmove then will try hybridcv predict
# takes the better of the 2 predicts i.e. lowest resid
emd_predict3 <- function (in1, ptype1="arma", ptype2="none", wsize=20,
                          title = "Needs title", chart = TRUE, xstart = 1, minmove = 0.25)
{
  #############################################
  # Decompose with CEEMDAN 
  #############################################
  cemd_in <- in1
  maxe <- MAXE(cemd_in)
  zeros <- rep(0, length(cemd_in) + ppb)
  
  if (maxe < 2*minmove) return(zeros)
  
  imfs1 <- ceemdan(cemd_in) # uses defaults
  
  #imfs1 <- ceemdan(cemd_in, num_siftings = 10, ensemble_size = 250, threads = 1)
  #imfs1 <- ceemdan(cemd_in, ensemble_size = 250,  noise_strength = 0.20)
  #imfs1 <- ceemdan(cemd_in, ensemble_size = EMD_ES, noise_strength = EMD_NS)
  #imfs1 <- eemd(cemd_in)
  #imfs1 <- emd(cemd_in) # Possibility
  #imfs1 <- bemd(cemd_in)
  
  ######### Plot Input #################
  index <- 1:(length(cemd_in))  # create index variable
  pplot <- as_tibble(cbind(index,cemd_in))
  
  g1 <- ggplot(pplot, aes(x=index)) +
    geom_line(aes(y = cemd_in), colour="black") +
    labs(title = TITLEB, y = "EMD INPUT", x = "Index" )
  
  suppressWarnings( print(g1) ) # surpress warnings about NAs
  
  if (chart == TRUE) plot(imfs1) # Plots IMF's
  
  nbrcols = ncol(imfs1)
  
  pred_cols <- matrix(nrow = (length(cemd_in)+Plen), ncol = nbrcols)
  err_cols <- matrix(nrow = length(cemd_in), ncol = nbrcols)
  rmse_err <- matrix(nrow = nbrcols, ncol = 2)
  
  #pred_cols <- NULL
  #err_cols <- NULL
  #rmse_err <- NULL
  
  # esch imf predict -> pred_cols
  # each inf predict error -> err_cols
  # each RMSE error -> rmse_err[]
  names <- colnames(imfs1)
  
  START_COL <-1
  
  for (i in seq(from= START_COL, to=nbrcols, by=1)) {
    name <- names[i]
    data <- imfs1[,i]
    #data <- as.numeric(imfs1[,i])
    #pred_cols[,i] <- an_predict2(data,PTYPE)
    
    #  MAXE(imf) <= 1 * minmove return zeros
    #  MAXE(imf) > minmove && MAXE(imf) <= 4 * mimove use ptype1)
    #  MAXE(imf) > 4 * minmove use ptype2
    rmse <- RMSE(data)
    maxe <- MAXE(data)
    print(paste0("**********************************"))
    print(paste0("imf",i," RMS= ", rmse, " MAX= ",maxe))
    
    if (maxe <= minmove) {
      print(paste0("Predict just using zeros "))
      pred_cols[,i] <- zeros
    }
    else {
      print(paste0("Trying ", ptype1, " ", ptype2, " predict"))
      pred_cols[,i] <- predict2(data, ptype1=ptype1, ptype2=ptype2, wsize=wsize,
                                chart = TRUE, maintitle = title, title = name, xstart = xstart) 
      tmp <- head( pred_cols[,i],length(data))
      r1 <- data - tmp
      rmse1 <- RMSE(r1)
      maxe1 <- MAXE(r1)
      if (maxe1 > 4 * minmove) {
        print(paste0("Trying ", "ALT", " predict"))
        p2 <- predict2(data, ptype1="hybridcv", ptype2="nn", wsize=wsize,
                       chart = TRUE, maintitle = title, title = name, xstart = xstart) 
        tmp <- head( p2,length(data))
        r2 <- data - tmp
        rmse2 <- RMSE(r2)
        maxe2 <- MAXE(r2)
        #print(paste0("imf",i," RMSE1= ", rmse1, " MAXE1= ",maxe1))
        #print(paste0("imf",i," RMSE2= ", rmse2, " MAXE2= ",maxe2))
        if (rmse2 < rmse1 && maxe2 < maxe1) { # using hybridcv
          print(paste0("Using ", "arma nn", " predict"))
          print(paste0("imf",i," RMSE1= ", rmse1, " MAXE1= ",maxe1))
          print(paste0("imf",i," RMSE2= ", rmse2, " MAXE2= ",maxe2))
          pred_cols[,i] <- p2
        }
      }   
    }
    tmp <- head( pred_cols[,i],length(data))
    err_cols[,i] <- data - tmp
    rmse_err[i,1] <- name
    #rmse_err[i,2] <- round(rmse(data,tmp),3)
    rmse_err[i,2] <- RMSE(data - tmp)
    
    rmse <- RMSE(data - tmp)
    maxe <- MAXE(data - tmp)
    print(paste0("imf",i," RMSE= ", rmse, " MAXE= ",maxe))
  } 
  
  colnames(rmse_err) <- c("imf","rmse")
  error_tbl <- as_tibble(rmse_err)
  error_tbl <- arrange(error_tbl, desc(rmse))
  
  #print(error_tbl)
  
  # save our plots so we can put imf predicts and errors on 2 pages
  pred_plots <- list()
  error_plots <- list()
  
  ##### Plot each imf and predict and error(resid)
  for (i in seq(from= START_COL, to=nbrcols, by=1)) {
    name <- names[i]
    pred <-  pred_cols[,i]
    error <- err_cols[,i]
    
    index <- 1:(length(pred))  # create index variable
    pricel <- imfs1[,i]
    pricel <- append(pricel,rep(NA,Plen))
    
    pplot <- as_tibble(cbind(index, pricel, pred))
    labele <- paste0(name," Predict")
    
    g1 <- ggplot(pplot, aes(x=index)) +
      geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
      geom_line(aes(y = pricel), colour="black") +
      geom_line(aes(y = pred), colour="red") +
      ylab(label=labele) +
      xlab("") 
    if (i == 1) g1 <- g1 + labs(title = title)
    
    pred_plots[[i]] <- g1
    
    ############################################
    index <- 1:(length(error))  # create index variable
    pplot <- as_tibble(cbind(index,error))
    labele <- paste0(name," Error")
    
    g1 <- ggplot(pplot, aes(x=index)) +
      geom_line(aes(y = error), colour="black") +
      ylab(label=labele) +
      xlab("") 
    if (i == 1) g1 <- g1 + labs(title = title) 
    
    error_plots[[i]] <- g1
  }
  
  #pred_tbl <- as_tibble(pred_cols)
  pred_tbl <- tibble(pred_cols)
  
  pred_tbl <- pred_tbl %>% 
    mutate(predictf = rowSums(.)) %>% 
    mutate(input=pricel)
  
  tmp <- head(pred_tbl$predictf,length(cemd_in))
  resid <- cemd_in - tmp
  
  # add resid column
  pred_tbl <- pred_tbl %>% 
    mutate(resid = input - predictf) 
  
  # write predict table of predict to csv file
  #TMPFILE <- paste0(AFLDIR, "pred_tbl.csv")
  #write_csv(pred_tbl,TMPFILE)
  
  # Page prints
  multiplot(plotlist=error_plots, cols=2)
  multiplot(plotlist=pred_plots, cols=2)
  
  final_plots <- list()
  ###### Now input and final predict
  pred <- pred_tbl$predictf
  index <- 1:(length(pred))  # create index variable
  pricel <- cemd_in
  pricel <- append(pricel,rep(NA,Plen))
  pplot <- as_tibble(cbind(index, pricel, pred))
  
  g1 <- ggplot(pplot, aes(x=index)) +
    geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
    geom_line(aes(y = pricel), colour="black") +
    geom_line(aes(y = pred), colour="red") +
    geom_point(aes(y = pred), colour="red") +
    labs(title = title, y = "Final Predict", x = "" ) +
    coord_cartesian(xlim=c(xstart,xend)) # zoom
  
  #suppressWarnings( print(g1) ) # surpress warnings about NAs
  final_plots[[1]] <- g1
  
  ############################################
  index <- 1:(length(resid))  # create index variable
  
  pplot <- as_tibble(cbind(index,resid))
  
  g2 <- ggplot(pplot, aes(x=index)) +
    geom_vline(aes(xintercept = datalen + 0.5), linetype="dashed", colour="black") +
    geom_line(aes(y = resid), colour="black") +
    geom_point(aes(y = resid), colour="black") +
    labs(title = "", y = "Final Resid", x = "" ) +
    coord_cartesian(xlim=c(xstart,xend)) # zoom
  
  #suppressWarnings( print(g2) ) # surpress warnings about NAs
  final_plots[[2]] <- g2
  
  multiplot(plotlist=final_plots, cols=1)
  
  print(error_tbl)
  
  rmset <- sum(as.numeric(error_tbl$rmse))
  
  #print(paste0("RMSE Total Error ", rmset))
  
  tail(in1,2*ppb)
  tail(pred,3*ppb)
  #tail(datein1, 2*ppb)
  print(paste0("RMSE Total Error ", rmset))
  
  return(as.numeric(pred))
}

####################################################
# SD Predict function - predicts d1 and adds
# supply signal to be predicted extened (datalen + ppb)
# returns predicted signal (length = datalen + pbb)
# nbr_predicts 1 to 4 all nybridcv
sd_predict <- function(in1l, nbr_predicts=1, do_charts=TRUE)
{
  if(nbr_predicts == 1) {
    PTYPE1 = "hybridcv"
    PTYPE2 = "none"
    PTYPE3 = "none"
    PTYPE4 = "none"
  }
  if(nbr_predicts == 2) {
    PTYPE1 = "hybridcv"
    PTYPE2 = "hybridcv"
    PTYPE3 = "none"
    PTYPE4 = "none"
  }
  if(nbr_predicts == 3) {
    PTYPE1 = "hybridcv"
    PTYPE2 = "hybridcv"
    PTYPE3 = "hybridcv"
    PTYPE4 = "none"
  }
  if(nbr_predicts == 4) {
    PTYPE1 = "hybridcv"
    PTYPE2 = "hybridcv"
    PTYPE3 = "hybridcv"
    PTYPE4 = "hybridcv"
  }
  
  in1  <- tail(in1l,length(in1l) - ppb)
  
  d1 <- diff(in1l,lag=ppb)
  
  ############# Level 1 predict ############
  d1p <- predict2(d1, ptype1=PTYPE1, ptype2=PTYPE2, wsize=wsize,
                  chart = do_charts, maintitle = TITLEB, title = "D1 Predict", xstart = xstart)
  
  predl1 <- in1l + d1p
  
  # First level resid
  tmp <- head(predl1,datalen)
  resid <- in1 - tmp
  
  ############# Level 2 predict ######################
  # Predict first level resid
  residp <- predict2(resid, ptype1=PTYPE3, ptype2=PTYPE4, wsize=wsize,
                     chart = do_charts, maintitle = TITLEB, title = "Level 2 Predict", xstart = xstart)
  
  # Final predict
  predf <- predl1 + residp
  
  # check for leading NA's and replace with actual input
  for (i in 1:length(predf)) {
    if (!is.na(predf[i])) break;
    predf[i] = in1[i]
  }
  
  return(predf)
}

############################################################
# LP filter atarting at threshold  think 0.00 threshold=width
fftfilter <- function(x, freq=0.10)
{
  threshold <- freq * length(x)
  y.fft <- fft(x)
  y.fft.filter <- y.fft
  y.fft.filter[threshold:length(y.fft)] <- 0 + 0i
  y.ifft <- fft(y.fft.filter, inverse = TRUE)/length(y.fft.filter)
  return(Re(y.ifft))
}

######### LOWPASS Filter Needs work ##########
# supposed to be Master's LP filter in R
lowpass <- function(series, padding, freq, width)
{
  print(paste0("At lowpass padding = ", padding, " freq= ", freq," width= ", width))
  
  in_len <- length(series)
  nbr_pads <- 0.8 / width
  
  # push length up to power of 2
  new_len <- 2
  while (new_len < in_len + nbr_pads) {
    new_len <- new_len * 2
  }
  halfn <- new_len / 2
  
  if (padding != "mean") {
  # compute and subtract detrend line and pas with zeros 
  intercept <- series[1]
  slope <- (series[in_len] - series[1]) / in_len
  } else {
    slope <- 0.0 ;
    intercept <- mean(series)
  }  
  
  det <- NULL
  for (i in seq(1,in_len)) {
    det[i] <- series[i] - intercept - slope*i
  }
  
  # pad with zeros to full padded length
  det <- c(det,rep(0,new_len - in_len))
  
  ## tmp
  threshold=freq * in_len
  y.fft = fft(det)
  y.fft.filter = y.fft
  y.fft.filter[threshold:length(y.fft)] = 0 + 0i
  y.ifft = fft(y.fft.filter, inverse = TRUE)/length(y.fft.filter)
  #return(Re(y.ifft))
  
  fft_out <- NULL
  print(paste0(in_len,"  ", length(y.ifft)))
  
  for (i in seq(1,in_len)) {
    fft_out[i] <- Re(y.ifft[i]) + intercept + slope*i-1
  }
  return(fft_out)
 ### end tmp
  
  xr <- NULL
  xi <- NULL
  for (i in seq(1,in_len+1)) {
    if (i %% 2) {
      xi[i/2] <- series[i] - intercept - slope*i
    } else {
      xr[i/2] <- series[i] - intercept - slope*i
    }
  }
  
  for (j in seq(i+1,new_len+1)) {
    if (j %% 2) {
      xi[j/2] <- 0
    } else {
      xr[j/2] <- 0
    }
  }
  
  det.complex <- as.complex(xr,xi)
  
  y.fft <- fft(det.complex)
  y.fft.filter <- y.fft
  
  for (i in seq(1,halfn)) {
    f <- i / new_len
    if (f <= freq) {
      wt <- 1.0
    } else {
      dist <- (f - freq) / width
      wt <- exp (-dist * dist)
      wt <- 0 ####### TAKE OUT #########
    }
    #print(paste0(i, " ", as.character(f), " ", as.character(dist), " ", as.character(wt)))
    print(dist)
    
    y.fft.filter[i] <- y.fft[i] * wt
  }

  
  #dist <- (0.5 - freq) / width
  #wt <- exp (-dist * dist)
  #y.fft.filter[1] <- as.complex( Re(y.fft[1]), Im(y.fft[1]) * wt)
  
  #y.ifft <- fft(y.fft.filter, inverse = TRUE)/length(y.fft.filter)
  #y.ifft <- fft(y.fft.filter, inverse = TRUE)/halfn)
  #y.ifft <- fft(y.fft.filter, inverse = TRUE)
  y.ifft = fft(y.fft.filter, inverse = TRUE)/length(y.fft.filter)
  
  #fft_out <- head(Re(y.ifft),in_len)
  #for (i in seq(1,in_len)) {
  #  fft_out[i] <- fft_out[i] + intercept + slope*i
  #}
  
  fft_out <- NULL
  print(paste0(in_len,"  ", length(y.ifft)))
  
  for (i in seq(1,in_len)) {
    #if (i %% 2) {
    #  fft_out[i] <- Im(y.ifft[i/2]) + intercept + slope*i-1
    #} else {
    #  fft_out[i] <- Re(y.ifft[i/2]) + intercept + slope*i-1
    #}
    
    fft_out[i] <- Re(y.ifft[i]) + intercept + slope*i-1
    #y.ifft = fft(y.fft.filter, inverse = TRUE)/length(y.fft.filter)
  }
  return(fft_out)
}  


################## end a########################