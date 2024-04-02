# catchup prep - older attempt - delete when latest working
# 01/30/2023 Larry Calvert
# before change DETAIL 1296 entries



########### EXtract rows to detailw and delete in DETAIL test
DETAIL <- read_csv("detail.csv",col_names = TRUE)

# Delete row numbers column if it exists and add new row numbers
if (DETAIL$row[[1]]) {
  DETAIL <- select(DETAIL,-row) # delete row numbers
 }
DETAIL <- DETAIL %>% mutate(row=row_number()) # Add row numbers
view(DETAIL)

# EXtrcat from DETAIL to detailw
symbol1 = "1WH3"
group1 = "actual"
dt1 = "h"
pproc = "none"
method = "zlma"
detailw <- DETAIL %>% dplyr::filter(symbol==symbol1, group==group1,
                                dt==dt1,pproc==pproc,method==method) %>% dplyr::slice_tail( n=((TUNEBARS+1)*ppb))
view(detailw)

nrow(detailw)
start <- detailw$row[[1]]
stop <- detailw$row[[nrow(detailw)]]
#print(paste0("start=",start, " stop=",stop))

# TODO LRC here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# now delete these rows in DETAIL
#DETAIL <- DETAIL %>% slice(-c(start:stop))
view(DETAIL)

######### END extract TEST #################################