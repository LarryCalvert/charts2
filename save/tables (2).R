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
SYMBOL_LIST <- tribble(
 ~symbol, ~desc, ~filename,~source, ~minmove, ~dollars, ~decimals, ~ddate, ~error1, ~error2,
 "1WU3", "Sept. Wheat", "wheat.html", "msdata", 0.25, 12.50, 2, NA,NA,NA,
 "1CU3", "Sept. Corn",  "corn.html", "msdata", 0.25, 12.50, 2, NA,NA,NA,
 "1OU3", "Sept. Oats",  "oats.html", "msdata", 0.25, 12.50, 2, NA,NA,NA,
 "META", "Meta",  "meta.html", "tq", 0.01, 0.01, 2, NA,NA,NA,
 "DIS", "Disney",  "dis.html", "tq", 0.01, 0.01, 2, NA,NA,NA,  
)

########## Pre-Process ###############
PPROC_LIST <- tribble(
~PPROC, ~comment,
"none",             "no preprocessing applied",
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

#"arma", "none", "none", "none", "none", "arma1",
#"arma", "arma", "none", "none", "none", "arma2",
#"arma", "nn", "none", "none", "none", "armann",

#"nn", "none", "none", "none", "none", "nn1",
#"nn", "nn", "none", "none", "none", "nn2",

#"none", "none", "ss",  "arma","none", "ss",
#"none", "none", "ks", "arma", "none", "ks",
#"none", "none", "ks", "arma", "arma", "ks2",
#"none", "none", "ks", "arma", "nn", "ks3",
#"none", "none", "ks", "hybridw", "none", "ks4",
#"none", "none", "mf", "arma", "none", "mf",

"none", "none", "mstl",  "arma","none", "mstl1",
"none", "none", "mstl",  "arma","arma", "mstl2",
"none", "none", "mstl",  "arma","nn", "mstl3",
#"none", "none", "mstl", "hybridw", "none", "mstl4",

"none", "none", "zlma", "arma", "none", "zlma1",
"none", "none", "zlma", "arma", "arma", "zlma2",
"none", "none", "zlma", "arma", "nn", "zlma3",
#"none", "none", "zlma", "hybridw", "none", "zlma4",

# hybrid predicts take a long time esp hybridcv
#"arma", "hybrid", "none", "none", "none", "ahybrid",
#"arma", "hybridw", "none", "none", "none", "ahybridw",
#"hybrid", "none", "none", "none", "none", "hybrid",

#"hybridw", "none", "none", "none", "none", "hybridw",
#"hybridw", "hybridw", "none", "none", "none", "hybridw2",

#"hybridcv", "none", "none", "none", "none", "hybridcv1",
#"hybridcv", "hybridcv", "none", "none", "none", "hybridcv2"

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

######## Methods and trades #######################################
TRADE_METHOD_LIST <- tribble(
 ~method,~desc,~en_bors,~en_ordtype,~ex_bors,~ex_ordtype,~hadj,~ladj,
  "M1", "Method 1 Buy", "Buy", "Stop", "Sell", "Stop",0.20,0.20, # no protective stop move
  "M1", "Method 1 Sell", "Sell", "Stop", "Buy", "Stop",0.20,0.20,  # no protective stop move
  "M2", "Method 1 Buy", "Buy", "Stop", "Sell", "Stop",0.20,0.20, #  protective stop moves to predicted mid
  "M2", "Method 1 Sell", "Sell", "Stop", "Buy", "Stop",0.20,0.20,  # protective stop moves to predicted mid
  "M3", "Method 1 Buy", "Buy", "Stop", "Sell", "Stop",0.20,0.20, # cpred.mpred crosovver no protective stop move
  "M3", "Method 1 Sell", "Sell", "Stop", "Buy", "Stop",0.20,0.20,  # cpred.mpred crosovver no protective stop move 
  )
 
# create trades
TRADE_LINES <- tibble(
  symbol    = character(),
  rundate  = character(),
  method   = character(),
  hadj     = double(),
  ladj     = double(),  
  tradenbr = numeric(), 
  #entry
  endate   = character(), #Sould be date but de-interplate must be returning char
  enbors    = character(),
  enord     = character(),  
  enprice    = double(),
  #exit
  exdate   = character(),
  exbors   = character(),
  exord   = character(),  
  exprice   = double(),
  expl	     = double() ,  # Profit/loss in minmove --  to get dollars times by symbol's dollars
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
 pldtot      = double(),
 wintot      = numeric(),
 losstot   = numeric() 
)
 
############ END ################################
