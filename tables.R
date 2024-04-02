##########################################
# tables.R for auto tune trade engine
#12/25/2022 Larry Calvert
##########################################

L2_WALKFORWARD <- FALSE
TUNEBARS <- 40 # number of bars in tune for L1_prediction
TOTALBARS <- TUNEBARS
if (L2_WALKFORWARD == TRUE) TOTALBARS <- TUNEBARS * 2

INTL <- 1 # Do a Full tune if you change this !!!!!!
ppb <- 1 + INTL
plen <- ppb # predict 1 bar ahead
datalen <- 4 * 20 * ppb # input data length 4 months

################  SYMBOLS  #################
# source msdata=metastock tq=for stocks
# filename is tmp until index.html can read a symbol list
# note mad = minimum adverse exception for placing entry order (in minmoves - check) errs may not be used check
# src msdata, bc, tq
SYMBOL_LIST <- tribble(
 ~symbol, ~desc, ~filename,~source, ~minmove, ~dollars, ~decimals, ~mad,~ddate, ~error1, ~error2,
 "1WK24", "May Wheat", "wheat.html", "bc", 0.25, 12.50, 2, 0.75,NA,NA,NA,
 # "1CK24", "May Corn",  "corn.html", "bc", 0.25, 12.50, 2, 0.75,NA,NA,NA,
 # "1OK24", "May Oats",  "oats.html", "bc", 0.25, 12.50, 2, 0.75,NA,NA,NA,
 # "META", "Meta",  "meta.html", "tq", 0.01, 1, 2, 0.05,NA,NA,NA,
 # "DIS", "Disney",  "dis.html", "tq", 0.01, 1, 2, 0.05,NA,NA,NA,
 # "NVDA", "NVidia",  "nvda.html", "tq", 0.01, 1, 2, 0.05,NA,NA,NA,
 # "MMM", "3M Co",  "mmm.html", "tq", 0.01, 1, 2, 0.05,NA,NA,NA,
 # "COST", "Costco",  "cost.html", "tq", 0.01, 1, 2, 0.05,NA,NA,NA,
 # "SMCI", "Sup Mic Comp",  "smci.html", "tq", 0.01, 1, 2, 0.05,NA,NA,NA,
)

########## Pre-Process ###############
PPROC_LIST <- tribble(
~PPROC, ~comment,
"none",      "no preprocessing applied",
#"L",         "Logs",
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
c("h","l","c","m"), "actual",
#c("hro","lro","cro","mro"), "ro",
#c("hr", "lr","cr","mr"),    "rpm",
)

########### PREDICT METHODS ############################
# Could add char parmN following each PofF which will be parsed
METHOD_LIST <- tribble( 
~PTYPE1,~PTYPE2,~FTYPE1,~PTYPE3,~PTYPE4,~METHOD,

####### direct price predict - no filtering
# "arma", "none", "none", "none", "none", "arma1", #new#
# "arma", "arma", "none", "none", "none", "arma2", #new#
# "arma", "nn", "none", "none", "none", "armann", #new#
# "nn", "none", "none", "none", "none", "nn1", #new#
# "nn", "nn", "none", "none", "none", "nn2",  #new#

#03/03/2024 in development
#"caret", "none", "none", "none", "none", "caret", #new added#
# "cf6", "none", "none", "none", "none", "cf6",
# "cf4", "none", "none", "none", "none", "cf4",
# "cf5", "none", "none", "none", "none", "cf5",
# "cf1", "none", "none", "none", "none", "cf1",
# "cf2", "none", "none", "none", "none", "cf2",
# "cf3", "none", "none", "none", "none", "cf3",

# "none", "none", "mstl", "cf1", "none", "cf1m",
# "none", "none", "mstl", "cf2", "none", "cf2m",
# "none", "none", "mstl", "cf3", "none", "cf3m",
# "none", "none", "mstl", "cf4", "none", "cf4m",
# "none", "none", "mstl", "cf5", "none", "cf5m",
# "none", "none", "mstl", "cf6", "none", "cf6m",
# 
# "none", "none", "zlma", "cf1", "none", "cf1z",
# "none", "none", "zlma", "cf2", "none", "cf2z",
# "none", "none", "zlma", "cf3", "none", "cf3z",
# "none", "none", "zlma", "cf4", "none", "cf4z",
# "none", "none", "zlma", "cf5", "none", "cf5z",
# "none", "none", "zlma", "cf6", "none", "cf6z",

  
# "arma", "none", "none", "none", "none", "arma1",
# "nn", "none", "none", "none", "none", "nn1",

#"none", "none", "atl9",  "arma","none", "loess2", # from chatGPT ####

####### other filters 
# "none", "none", "ss",  "arma","none", "ss", # all new this section#
# "none", "none", "ks", "arma", "none", "ks",
# "none", "none", "ks", "arma", "arma", "ks2",
# "none", "none", "ks", "arma", "nn",   "ks3",
# "none", "none", "mf", "arma", "none", "mf",


"none", "none", "mstl", "arma", "none", "mstl1",     #org#
"none", "none", "mstl", "arma", "arma", "mstl2",     #org#
"none", "none", "mstl", "arma", "nn",   "mstl3",     #org#
#"none", "none", "mstl", "nn",   "none", "mstl4",
#"none", "none", "mstl", "nn",   "nn",   "mstl5",
"none", "none", "zlma", "arma", "none", "zlma1",     #org#
"none", "none", "zlma", "arma", "arma", "zlma2",     #org#
"none", "none", "zlma", "arma", "nn",   "zlma3",     #org#
#"none", "none", "zlma", "nn",   "none", "zlma4",
#"none", "none", "zlma", "nn",   "nn",   "zlma5",

############ Newe filter test ########################
# "none", "none", "zlma2", "arma", "none", "zlma12",
# "none", "none", "zlma2", "arma", "arma", "zlma22",
# "none", "none", "zlma2", "arma", "nn",   "zlma32",
# "none", "none", "zlma2", "nn",   "none", "zlma42",
# "none", "none", "zlma2", "nn",   "nn",   "zlma52",
# 
# "none", "none", "ma", "arma", "none", "ma1",
# "none", "none", "ma", "arma", "arma", "ma2",
# "none", "none", "ma", "arma", "nn",   "ma3",
# "none", "none", "ma", "nn",   "none", "ma4",
# "none", "none", "ma", "nn",   "nn",   "ma5",
# 
# "none", "none", "alp", "arma", "none", "alp1",
# "none", "none", "alp", "arma", "arma", "alp2",
# "none", "none", "alp", "arma", "nn",   "alp3",
# "none", "none", "alp", "nn",   "none", "alp4",
# "none", "none", "alp", "nn",   "nn",   "alp5",

# "none", "none", "mstl", "cf1", "none", "cf1m",
# "none", "none", "mstl", "cf2", "none", "cf2m",
# "none", "none", "mstl", "cf3", "none", "cf3m",
# "none", "none", "mstl", "cf4", "none", "cf4m",
# "none", "none", "mstl", "cf5", "none", "cf5m",
# "none", "none", "mstl", "cf6", "none", "cf6m",
# 
# "none", "none", "zlma", "cf1", "none", "cf1z",
# "none", "none", "zlma", "cf2", "none", "cf2z",
# "none", "none", "zlma", "cf3", "none", "cf3z",
# "none", "none", "zlma", "cf4", "none", "cf4z",
# "none", "none", "zlma", "cf5", "none", "cf5z",
# "none", "none", "zlma", "cf6", "none", "cf6z",

#"none", "none", "mstl", "hybridw", "none", "mstl6",
# "none", "none", "zlma", "hybridw", "none", "zlma6",

# hybrid predicts take a long time esp hybridcv
# "arma", "hybrid", "none", "none", "none", "ahybrid",
# "arma", "hybridw", "none", "none", "none", "ahybridw",
# "hybrid", "none", "none", "none", "none", "hybrid",

# "hybridw", "none", "none", "none", "none", "hybridw", #new#
#"hybridw", "hybridw", "none", "none", "none", "hybridw2", #new#

# "hybridcv", "none", "none", "none", "none", "hybridcv1", #new#
# "hybridcv", "hybridcv", "none", "none", "none", "hybridcv2" #new#

#"emd", "none", "none", "none", "none", "emd"
)

#######################################################################
# create detail for each walk forward bar
DETAIL_LINES <- tibble(
  date        = Date(),
  symbol    = character(),
  desc        = character(),
  group      = character(),
  dt            = character(),  
  pproc      = character(),
  method   = character(),
  price       = double(),
  predict    = double(),
  target     = double(),
  error       = double(), 
  etime      = double()
)


############ END ################################
