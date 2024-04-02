##########################################
# tables.R for auto tune trade engine
#12/25/2022 Larry Calvert
##########################################
TUNEBARS <- 40 # number of bars in tune for prediction

INTL <- 1
ppb <- 1 + INTL
plen <- ppb # predict 1 bar ahead
datalen <- 4 * 20 * ppb # input data length 4 months

################  SYMBOLS  #################
SYMBOL_LIST <- tribble(
  ~symbol, ~desc, ~source, ~minmove, ~dollars, ~decimals,~ddate,~error1,~error2,
  "1WH3", "Wheat", "msdata", 0.25, 12.50, 2, NA,NA,NA,
  #"1CH3", "Corn",  "msdata", 0.25, 12.50, 2, NA,NA,NA,
  #"1OH3", "Oats",  "msdata", 0.25, 12.50, 2, NA,NA,NA
)

########## Pre-Process ###############
PPROC_LIST <- tribble(
~PPROC, ~comment,
"none",             "no preprocessing applied"
#"L",         "Logs"
#"LD",       "Logs Difference",
#"LDS",     "Logs Difference Standardize",
#"LDSB",   "Logs Difference Standardize BNorm",
#"D",         "Difference",
#"S",         "Standardize",
#"B",         "BNorm"
)

############## DATATYPE ####################
DATATYPE_LIST <- tribble(
  ~dt, ~group,
  c("h","l","c","m"),         "actual"
  #c("hro","lro","cro","mro"), "ro"
  #c("hr", "lr","cr","mr"),    "rpm"
)

########### METHODS ############################
METHOD_LIST <- tribble( 
~PTYPE1,~PTYPE2,~FTYPE1,~PTYPE3,~PTYPE4,~METHOD,
"none", "none", "mstl",  "arma","arma", "mstl",
"none", "none", "zlma", "arma", "arma", "zlma"
)

#######################################################################
# create detail for each walk forward bar
# could read saved DETAIL.csv file also and start from last detail date
DETAIL_LINES <- tibble(
  #wfdate     = character(),
  #date       = character(),
  date       = Date(),
  symbol     = character(),
  group      = character(),
  dt         = character(),  
  pproc      = character(),
  method     = character(),
  sderr      = double(),   
  price      = double(),
  predict    = double(),
  target     = double(),
  error      = double(),
  etime      = double()
)

############ END ################################
