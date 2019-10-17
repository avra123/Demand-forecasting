start_time<- Sys.time()

# load required packages
library(data.table)
library(xlsx)
library(zoo)
library(ggplot2)
library(RcppRoll)
library(lubridate)
library(tidyverse)
library(matrixStats)

## Model for  DOLLARS VS QUANTITY
# this one is used later so it can do dollars or quantity model
MODEL_TYPE <- "DOLLARS"

TRAIN_CUTOFF_DATE <- as.Date("2018-09-01")
FAMILY_value_list_exc<- c("TT","AGIGA","COM",""," ",NA)

## load util functions
source("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\util.R")

# One Qlik app in GSGR home area builds all of these CSV files
# Need to update the app name here: <******>

## load Cypress Calender:
# make sure this .csv is automatically generated - because it changes slightly each day
CY_CAL <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\CALENDAR.csv", stringsAsFactors = FALSE)

## load Data:
PARTS <- fread('C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_PARTS_TEMP.csv', stringsAsFactors = FALSE)

# Check why we have the unique() function below
PARTS_FAM_MAP <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_PARTS_FAMILY_TEMP.csv", stringsAsFactors = FALSE)
PARTS_FAM_MAP <- unique(PARTS_FAM_MAP)

# Check why we have the unique() function below
PARTS_AGE <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_PARTS_AGE_TEMP.csv", stringsAsFactors = FALSE)
PARTS_AGE <- unique(PARTS_AGE)

BACKLOGS_RAW <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_BACKLOGS_TEMP.csv", stringsAsFactors = FALSE)

REVENUE_RAW <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_REVENUE_TEMP.csv", stringsAsFactors = FALSE)

## Load last 3 years CAT-A/B information:
## Custom file created by GSGR based on input from Gana
## Using this to prevent issues where customers move from cat-A to cat-B
## Where this occurs you can have backlog on one FC_CUST and shipments on a different FC_CUST

## Look at tweaking the below logic so the '| CURRENT_FC_CATAB=="CAT B"' doesn't introduce 
## CAT-B customers that moved between CAT-A,CAT-B
## may need a list of customers that changed

## The below lines totally remove line items which do not have a FC customer in the CAT-A
##  or if the line is marked as CAT B

CATA_17_19_LIST <- fread('C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\MASTER_FC_CUST_17-19.csv')

BACKLOGS_RAW <- BACKLOGS_RAW[CURRENT_FC_CUSTOMER %in% unique(CATA_17_19_LIST$CURRENT_FC_CUSTOMER)
                             | CURRENT_FC_CATAB=="CAT B"]
REVENUE_RAW <- REVENUE_RAW[CURRENT_FC_CUSTOMER %in% unique(CATA_17_19_LIST$CURRENT_FC_CUSTOMER)
                           | CURRENT_FC_CATAB=="CAT B"]

# MPN name alias table (right now it's for MB and BCM) - need this CSV file in the single Qlik app

OLD_NEW_MPN_MAP <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\MB_BCM_TO_CY_PARTS_MAPPING.csv", stringsAsFactors = FALSE)
## clean up MPNS:
OLD_NEW_MPN_MAP <- unique(OLD_NEW_MPN_MAP)

## do MPN cleaning here
names(OLD_NEW_MPN_MAP) <- c("OLD_MPN","MKT_PART_NUM")
OLD_NEW_MPN_MAP<- OLD_NEW_MPN_MAP[,.(OLD_MPN=first(OLD_MPN))
                                  ,by=.(MKT_PART_NUM)]
# change column names in the mapping file
names(OLD_NEW_MPN_MAP) <- c("NEW_MPN", "MKT_PART_NUM")

# filter mismatching MPNs from the raw data
CLEAN_UP <-  BACKLOGS_RAW[MKT_PART_NUM %in% OLD_NEW_MPN_MAP$MKT_PART_NUM]
BACKLOGS_RAW <- BACKLOGS_RAW[!(MKT_PART_NUM %in% OLD_NEW_MPN_MAP$MKT_PART_NUM)]

# Setkey tells the table which field to use when joining later
setkey(OLD_NEW_MPN_MAP,MKT_PART_NUM)
setkey(CLEAN_UP,MKT_PART_NUM)

#make the join, replace the MPNs and drop the duplicates
CLEAN_UP[OLD_NEW_MPN_MAP, on = 'MKT_PART_NUM', PART_NUM:=i.NEW_MPN]
CLEAN_UP[, MKT_PART_NUM:= PART_NUM]
CLEAN_UP[, PART_NUM:=NULL]

# re-insert the cleaned up rows back into the raw backlogs data
BACKLOGS_RAW <- rbind(BACKLOGS_RAW, CLEAN_UP)

# do the same thing to the revenue table
setkey(REVENUE_RAW,MKT_PART_NUM)
CLEAN_UP <-  REVENUE_RAW[MKT_PART_NUM %in% OLD_NEW_MPN_MAP$MKT_PART_NUM]
REVENUE_RAW <- REVENUE_RAW[!(MKT_PART_NUM %in% OLD_NEW_MPN_MAP$MKT_PART_NUM)]
setkey(CLEAN_UP,MKT_PART_NUM)

#make the join, replace the MPNs and drop the duplicates
CLEAN_UP[OLD_NEW_MPN_MAP, on = 'MKT_PART_NUM', PART_NUM:=i.NEW_MPN]
CLEAN_UP[, MKT_PART_NUM:= PART_NUM]
CLEAN_UP[, PART_NUM:=NULL]

REVENUE_RAW <- rbind(REVENUE_RAW, CLEAN_UP)
rm(CLEAN_UP)



# the rest of the script is coded using the QUANTITY field
# this step puts dollars in the quantity field when in "Dollars" mode
if(MODEL_TYPE=="DOLLARS"){
  print("Yolo!")
  REVENUE_RAW[,QUANTITY:=NET_REVENUE]
  BACKLOGS_RAW[,QUANTITY:=NET_REVENUE]
  
}

## Summarize Backlogs
## Just assigning without copy will leave them connected and changes can impact the raw table
BACKLOGS_RAW_CR <- copy(BACKLOGS_RAW)
BACKLOGS_RAW_ORD_ENT <- copy(BACKLOGS_RAW)

# BACKLOG_FC_CAT consists of unique customer-MPN combinations by Snapshot date
BACKLOG_FC_CAT <- copy(BACKLOGS_RAW)
BACKLOG_FC_CAT <- unique(BACKLOG_FC_CAT[,.(SNAPSHOT_DATE=as.Date(SNAPSHOT_DATE,format='%m/%d/%Y'),
                                           CURRENT_FC_CUSTOMER,CURRENT_FC_CATAB)])

## Clean up date formats and remove strange records like RS before snapshot quantity < 0 (returns)
BACKLOGS_RAW[,RS_DATE:=as.Date(RS_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW[,SNAPSHOT_DATE:=as.Date(SNAPSHOT_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW[,ORD_ENT_DATE:=as.Date(ORD_ENT_DATE, format='%m/%d/%Y')]

## Check why we picked on CYTECH - what about other unique distis like Farnell, etc
BACKLOGS_RAW <- BACKLOGS_RAW[
  DISTI_CORP_ID!='CYTECH'
  ,.(QUANTITY=sum(QUANTITY,na.rm=TRUE)),
  by=.(SNAPSHOT_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM,PARTS_ID,RS_DATE)]

### feature 1, Weeks to ship###
BACKLOGS_RAW[,WEEKS_TO_SHIP:=round(as.numeric(difftime(RS_DATE,SNAPSHOT_DATE,units="weeks")))]
BACKLOGS_RAW <- BACKLOGS_RAW[!(WEEKS_TO_SHIP<0 | QUANTITY <0)]

### BACKLOG FEATURE Engineering:

# The +0.5, +0.75 adds half a year, adds 3 quarters, etc
# all of these features will be repeated later for 2 quarter model
# The key difference between the two models is the definition of Y (ship to backlog ratio)

BACKLOGS <-BACKLOGS_RAW[as.yearqtr(RS_DATE)> as.yearqtr(SNAPSHOT_DATE) &
                          as.yearqtr(RS_DATE) <= as.yearqtr(SNAPSHOT_DATE)+0.5
                        ,.(BACKLOG=sum(QUANTITY,na.rm = TRUE))
                        ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]

BACKLOGS <- BACKLOGS[order(SNAPSHOT_DATE)]

## BACKLOG for First Quarter of Forecast Window:
BACKLOGS_FQ <-BACKLOGS_RAW[as.yearqtr(RS_DATE)> as.yearqtr(SNAPSHOT_DATE) &
                             as.yearqtr(RS_DATE) < as.yearqtr(SNAPSHOT_DATE)+0.5 #current qtr + 1 because this is less than and not less than equal to
                           ,.(BACKLOG_FQ=sum(QUANTITY,na.rm = TRUE))
                           ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]

## BACKLOG for Second Quarter of Forecast Window:
BACKLOGS_SQ <-BACKLOGS_RAW[as.yearqtr(RS_DATE)> as.yearqtr(SNAPSHOT_DATE)+0.25 &
                             as.yearqtr(RS_DATE) < as.yearqtr(SNAPSHOT_DATE)+0.75
                           ,.(BACKLOG_SQ=sum(QUANTITY,na.rm = TRUE))
                           ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]

# using the 4th quarter as a feature may help predict Q2/Q3 

## BACKLOG for Next Quarter After the Forecast Window:
BACKLOGS_NQ <-BACKLOGS_RAW[as.yearqtr(RS_DATE) - as.yearqtr(SNAPSHOT_DATE) >= 0.75 # why equal to and not greater than equal to? 
                           ,.(BACKLOG_NQ=sum(QUANTITY,na.rm = TRUE))
                           ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]


############################### New feature for testing ###########################

#BACKLOGS_CURR_QTR <- BACKLOGS_RAW[(as.yearqtr(RS_DATE) == as.yearqtr(SNAPSHOT_DATE)),
#                                  .(BACKLOG_CURR_QTR=sum(QUANTITY, na.rm=TRUE)),
#                                  by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)]

###################################################################################

## This one finds the backlog for the window of interest 13 weeks ago (has it grown a bunch?)
## This particular example does a 1-QTR window
## 13 weeks Lag Backlog for given snapshot:
BACKLOG_26_FOR_13WEEKS_AGO <- copy(BACKLOGS)
BACKLOG_26_FOR_13WEEKS_AGO <- BACKLOG_26_FOR_13WEEKS_AGO[,SNAPSHOT_DATE:=(SNAPSHOT_DATE+(7*13))]


#Merging everything into a single BACKLOGS table

setkey(BACKLOGS_SQ,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
BACKLOGS <- BACKLOGS_SQ[BACKLOGS]
BACKLOGS[,BACKLOG_SQ:=ifelse(is.na(BACKLOG_SQ),0,BACKLOG_SQ)]

setkey(BACKLOGS_FQ, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
BACKLOGS <- BACKLOGS_FQ[BACKLOGS]
BACKLOGS[,BACKLOG_FQ:=ifelse(is.na(BACKLOG_FQ), 0, BACKLOG_FQ)]

setkey(BACKLOGS_NQ, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
BACKLOGS <- BACKLOGS_NQ[BACKLOGS]
BACKLOGS[, BACKLOG_NQ:=ifelse(is.na(BACKLOG_NQ),0,BACKLOG_NQ)]

## in the above joins, I have skipped joining BACKLOGS_FQ and BACKLOGS_13
## BACKLOGS_13 is just a combination of FQ_13 and SQ_13 without the old orders
## and BACKLOGS and BACKLOGS_FQ are the same (no need to double count them!)

# All the same as before except us CR_DATE instead of RS_DATE

BACKLOGS_RAW_CR[,CR_DATE:=as.Date(CR_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW_CR[,SNAPSHOT_DATE:=as.Date(SNAPSHOT_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW_CR[,WEEKS_TO_SHIP_CR:=round(as.numeric(difftime(CR_DATE,SNAPSHOT_DATE,units="weeks")))]
BACKLOGS_RAW_CR <- BACKLOGS_RAW_CR[!(WEEKS_TO_SHIP_CR<0 | QUANTITY <0)]


BACKLOGS_CR <-BACKLOGS_RAW_CR[as.yearqtr(CR_DATE)> as.yearqtr(SNAPSHOT_DATE) &
                                as.yearqtr(CR_DATE) <= as.yearqtr(SNAPSHOT_DATE)+0.5
                              ,.(BACKLOG_CR=sum(QUANTITY,na.rm = TRUE)
                              )
                              ,by=.(SNAPSHOT_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM)]

BACKLOGS_CR_FQ <-BACKLOGS_RAW_CR[as.yearqtr(CR_DATE)> as.yearqtr(SNAPSHOT_DATE) &
                                   as.yearqtr(CR_DATE) < as.yearqtr(SNAPSHOT_DATE)+0.5
                                 ,.(BACKLOG_CR_FQ=sum(QUANTITY,na.rm = TRUE)
                                 )
                                 ,by=.(SNAPSHOT_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM)]

BACKLOGS_CR_SQ <-BACKLOGS_RAW_CR[as.yearqtr(CR_DATE)> as.yearqtr(SNAPSHOT_DATE)+0.25 &
                                   as.yearqtr(CR_DATE) < as.yearqtr(SNAPSHOT_DATE)+0.75
                                 ,.(BACKLOG_CR_SQ=sum(QUANTITY,na.rm = TRUE)
                                 )
                                 ,by=.(SNAPSHOT_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM)]

#Using ORD_ENT_DATE
BACKLOGS_RAW_ORD_ENT <- BACKLOGS_RAW_ORD_ENT[DISTI_CORP_ID!='CYTECH',
                                             .(QUANTITY=sum(QUANTITY,na.rm=TRUE)),
                                            by=.(SNAPSHOT_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM,PARTS_ID,ORD_ENT_DATE)]

BACKLOGS_RAW_ORD_ENT[,ORD_ENT_DATE:=as.Date(ORD_ENT_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW_ORD_ENT[,SNAPSHOT_DATE:=as.Date(SNAPSHOT_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW_ORD_ENT<- BACKLOGS_RAW_ORD_ENT[SNAPSHOT_DATE==max(SNAPSHOT_DATE),!c("SNAPSHOT_DATE"),with=FALSE]


# from SHIPMENTS
REVENUE_HIST<- copy(REVENUE_RAW)

REVENUE_HIST[,ORD_ENT_DATE:=(as.Date(ORD_ENT_DATE,format='%m/%d/%Y'))]

# Cleaning up the END_CUSTOMER column but this isn't needed anymore - it gets aggregated higher later anyway
REVENUE_HIST <- (REVENUE_HIST[DISTI_CORP_ID!='CYTECH'])
REVENUE_HIST[,QUANTITY:=ifelse(is.na(QUANTITY),0,QUANTITY)]
REVENUE_HIST[,END_CUSTOMER:=ifelse(END_CUSTOMER=='' | is.na(END_CUSTOMER),"UNKNOWN",END_CUSTOMER)]
REVENUE_HIST <- REVENUE_HIST[!(MKT_PART_NUM=='' | DISTI_CORP_ID=='')]

REVENUE_HIST <- REVENUE_HIST[,.(QUANTITY=sum(QUANTITY,na.rm=TRUE)),
                             by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,PARTS_ID,ORD_ENT_DATE)]

# need to combine these tables to get the full history on incoming orders
ORDERS_HIST <- rbind(REVENUE_HIST,BACKLOGS_RAW_ORD_ENT)

setkey(PARTS_FAM_MAP, MKT_PART_NUM)
setkey(ORDERS_HIST, MKT_PART_NUM)
ORDERS_HIST <- PARTS_FAM_MAP[ORDERS_HIST]

ORDERS_HIST[,SNAPSHOT_DATE:=floor_date(ORD_ENT_DATE,unit = "week")]

ORDERS_HIST_FAM <- ORDERS_HIST[order(FAMILY,SNAPSHOT_DATE),.(QUANTITY=sum(QUANTITY,na.rm=TRUE)),
                               by=.(FAMILY,SNAPSHOT_DATE)]

#########################################

# Use the rccproll package
# This section is similar to the new bookings KPI in the demand inflection dashboard
# How hot is the family right now compared to recent past (is it inflecting?)

ORDERS_HIST_FAM[,QUANTITY_ROLL6:=roll_mean(QUANTITY,6,align = "right", fill=NA),
                by=.(FAMILY)
                ]
ORDERS_HIST_FAM[,QUANTITY_ROLL26:=roll_mean(QUANTITY,26,align = "right", fill=NA),
                by=.(FAMILY)
                ]

ORDERS_HIST_FAM[,GAP_RATIO:=(QUANTITY_ROLL6/QUANTITY_ROLL26)]

ORDERS_HIST_FAM <- ORDERS_HIST_FAM[!(is.na(GAP_RATIO) & is.na(FAMILY))
                                   ,!c("QUANTITY","QUANTITY_ROLL6","QUANTITY_ROLL26")
                                   ,with=FALSE]

## creating another feature to identify the gap between invoice date and order entry date?
REVENUE_MODEL<- copy(REVENUE_RAW)

REVENUE_MODEL <- (REVENUE_MODEL[DISTI_CORP_ID!='CYTECH'])

REVENUE_MODEL[,QUANTITY:=ifelse(is.na(QUANTITY),0,QUANTITY)]
REVENUE_MODEL[,END_CUSTOMER:=ifelse(END_CUSTOMER=='' | is.na(END_CUSTOMER),"UNKNOWN",END_CUSTOMER)]
REVENUE_MODEL <- REVENUE_MODEL[!(MKT_PART_NUM=='' | DISTI_CORP_ID=='')]
REVENUE_MODEL[,INVOICE_DATE:=floor_date(as.Date(INVOICE_DATE,format='%m/%d/%Y'),unit = "week")]

REVENUE_MODEL <- REVENUE_MODEL[,.(QUANTITY=sum(QUANTITY,na.rm=TRUE))
                               ,by=.(INVOICE_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM,PARTS_ID)]

# Create a list of dates from the min to the max to cover the entire timeframe
# Join to the revenue data 
# This structure is commonly used in other models to transform
# records in tables without snapshots to link to tables with snapshots
# This structure is slow with data.table join, but works much faster with data.frame + merge

WEEKS <- setDT(as.data.frame((seq(min(BACKLOGS$SNAPSHOT_DATE),max(BACKLOGS$SNAPSHOT_DATE),by="week"))))
names(WEEKS)<- c("WEEK_DATES")

# by=NULL does full outer join all possible combinations

REVENUE_MODEL <- setDT(merge(as.data.frame(REVENUE_MODEL),as.data.frame(WEEKS),by=NULL))
REVENUE_MODEL[,FRWD_WEEKS:=round(as.numeric(difftime(INVOICE_DATE,WEEK_DATES,units="weeks")))]
REVENUE_MODEL <- REVENUE_MODEL[FRWD_WEEKS>0]

# New Addition
# 26 Week rolling RS backlog from snapshot date
BACKLOG_26WEEKS_13WEEKS_AGO_ROLLING <- BACKLOGS_RAW[((RS_DATE - SNAPSHOT_DATE <= 91*2) & (RS_DATE > SNAPSHOT_DATE)), 
                                                   .(BACKLOG_QTY = sum(QUANTITY)), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)]
BACKLOG_26WEEKS_13WEEKS_AGO_ROLLING[, SNAPSHOT_DATE:=SNAPSHOT_DATE+(13*7)]

REVENUE_13_13WEEKS_AGO_SNAP <- copy(REVENUE_MODEL)

REVENUE_13_13WEEKS_AGO_SNAP <-REVENUE_13_13WEEKS_AGO_SNAP[FRWD_WEEKS<=13
                                                          ,.(REVENUE_13=sum(QUANTITY, na.rm = TRUE)
                                                          )
                                                          ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,WEEK_DATES)]

names(REVENUE_13_13WEEKS_AGO_SNAP)[3] <- "SNAPSHOT_DATE"

REVENUE_13_13WEEKS_AGO_SNAP[,SNAPSHOT_DATE:=SNAPSHOT_DATE+((13*7))]

## Compute Fill rate for 13 Weeks ago snapshot as of current snapshot
setkey(REVENUE_13_13WEEKS_AGO_SNAP,CURRENT_FC_CUSTOMER,MKT_PART_NUM)
setkey(BACKLOG_26WEEKS_13WEEKS_AGO_ROLLING,CURRENT_FC_CUSTOMER,MKT_PART_NUM)

# Check to make sure the computation for progress for 1 Quarter model
# Is it always calculated 13 vs 26 (make sure in this one it's not 13 vs 13); this is comparing 13 vs 13 though; 
# what is the implication of such a comparision? 

PROGRESS_FOR_13WEEKS_AGO_SNAP <- merge(BACKLOG_26WEEKS_13WEEKS_AGO_ROLLING, REVENUE_13_13WEEKS_AGO_SNAP,
                                       by=c("CURRENT_FC_CUSTOMER","MKT_PART_NUM","SNAPSHOT_DATE"),
                                       all=TRUE)

PROGRESS_FOR_13WEEKS_AGO_SNAP[,REVENUE_13:=ifelse(is.na(REVENUE_13),0,REVENUE_13)]
PROGRESS_FOR_13WEEKS_AGO_SNAP[,BACKLOG_QTY:=ifelse(is.na(BACKLOG_QTY),0,BACKLOG_QTY)]
PROGRESS_FOR_13WEEKS_AGO_SNAP[,PROGRESS:=REVENUE_13/BACKLOG_QTY]

PROGRESS_FOR_13WEEKS_AGO_SNAP[,PROGRESS_FACTOR:=as.factor(ifelse(PROGRESS<.5,"BELOW_RR",
                                                          ifelse(PROGRESS<1,"ABOVE_RR",
                                                                 ifelse(PROGRESS==1,"COMPLETE",
                                                                        "TURNS"))))]


# WEEKS_DATES here is the snapshot field used to do the full outer join above

REVENUE_MODEL <-REVENUE_MODEL[as.yearqtr(INVOICE_DATE)> as.yearqtr(WEEK_DATES) &
                                as.yearqtr(INVOICE_DATE) <= as.yearqtr(WEEK_DATES)+0.5
                              ,.(REVENUE_26=sum(QUANTITY,na.rm=TRUE)
                              )
                              ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,WEEK_DATES)]

names(REVENUE_MODEL)[3] <- "SNAPSHOT_DATE"

## Training Data set:
#setkey(REVENUE_MODEL,DISTI_CORP_ID, CURRENT_FC_CUSTOMER,BILL_TO_GEOG,MKT_PART_NUM,SNAPSHOT_DATE)
#setkey(BACKLOGS,DISTI_CORP_ID, CURRENT_FC_CUSTOMER,BILL_TO_GEOG,MKT_PART_NUM,SNAPSHOT_DATE)

# now create the master data table for the model

MODEL_DATA <- merge(BACKLOGS,REVENUE_MODEL,by=c("CURRENT_FC_CUSTOMER","MKT_PART_NUM","SNAPSHOT_DATE"),
                    all=TRUE)


## Review both ends of the specturm. Some orders have REVENUE as NA and some have BACKLOG as NA.
## BAcklog is probably fine because the orders might be short to lead time/ in qtr orders. 
## Deal with Lines which have Backlog but no revenue for late snapshot dates



## Lookup table for PART_ID,SNAPSHOT_DATE,MPN
# This is probably used to get lead time (that's the only reason we have PARTS_ID)
MPN_PARTID_SNAP <- unique(BACKLOGS_RAW_CR[,.(SNAPSHOT_DATE,MKT_PART_NUM,PARTS_ID)])

## Now aggregate everything at MPN Level
MODEL_DATA_BACKUP <- copy(MODEL_DATA)


MODEL_DATA <- 
  MODEL_DATA[,.(
    REVENUE_26=sum(REVENUE_26,na.rm = TRUE),
    BACKLOG_26=sum(BACKLOG, na.rm = TRUE),
    BACKLOG_FQ=sum(BACKLOG_FQ,na.rm = TRUE),
    BACKLOG_SQ=sum(BACKLOG_SQ,na.rm = TRUE),
    BACKLOG_NQ=sum(BACKLOG_NQ,na.rm = TRUE)
  )
  ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
  ]

# Find first occurence of each combo of FC_CUST/MPN
# Need to make sure we understand why this one is being done
# Redundant lines (not being used anywhere)

#MODEL_DATA[,MIN_SNAP:=min(SNAPSHOT_DATE),
 #          by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM)]

#MODEL_DATA <- MODEL_DATA[SNAPSHOT_DATE < TRAIN_CUTOFF_DATE] # why is this being done here itself? 

MODEL_DATA <- MODEL_DATA[#SNAPSHOT_DATE>(MIN_SNAP+(26*7)) & 
  SNAPSHOT_DATE < TRAIN_CUTOFF_DATE]

MODEL_DATA[, SHIP_TO_BOOK_RATIO:=REVENUE_26/BACKLOG_26]

# Removing SNAPSHOT_DATE- 2018-11-02 (Backlog is 0 all across)
MODEL_DATA <- MODEL_DATA[!(SNAPSHOT_DATE == "2018-02-11")]
MODEL_DATA <- MODEL_DATA[SNAPSHOT_DATE!=as.Date("2018-02-07")]


## AVRA BAcklog Growth
MODEL_DATA[, BACKLOG_GROWTH:=(diff(BACKLOG_26)/BACKLOG_26), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM)]
MODEL_DATA[, BACKLOG_FQ_GROWTH:=(diff(BACKLOG_FQ)/BACKLOG_FQ), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM)]
MODEL_DATA[, BACKLOG_SQ_GROWTH:=(diff(BACKLOG_SQ)/BACKLOG_SQ), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM)]
MODEL_DATA[, BACKLOG_NQ_GROWTH:=(diff(BACKLOG_NQ)/BACKLOG_NQ), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM)]


MODEL_DATA[,BACKLOG_GROWTH:=as.factor(ifelse(is.finite(BACKLOG_GROWTH)
                                             ,ifelse(BACKLOG_GROWTH<0,"NEGATIVE_GROWTH",
                                                     ifelse(BACKLOG_GROWTH==0,"NO_CHANGE",
                                                            ifelse(BACKLOG_GROWTH<=0.5,"NORMAL_GROWTH",
                                                                   "AGGRESSIVE_GROWTH")))
                                             ,"FROM_ZERO"))]

MODEL_DATA[,BACKLOG_FQ_GROWTH:=as.factor(ifelse(is.finite(BACKLOG_FQ_GROWTH)
                                             ,ifelse(BACKLOG_FQ_GROWTH<0,"NEGATIVE_GROWTH",
                                                     ifelse(BACKLOG_FQ_GROWTH==0,"NO_CHANGE",
                                                            ifelse(BACKLOG_FQ_GROWTH<=0.5,"NORMAL_GROWTH",
                                                                   "AGGRESSIVE_GROWTH")))
                                             ,"FROM_ZERO"))]

MODEL_DATA[,BACKLOG_SQ_GROWTH:=as.factor(ifelse(is.finite(BACKLOG_SQ_GROWTH)
                                             ,ifelse(BACKLOG_SQ_GROWTH<0,"NEGATIVE_GROWTH",
                                                     ifelse(BACKLOG_SQ_GROWTH==0,"NO_CHANGE",
                                                            ifelse(BACKLOG_SQ_GROWTH<=0.5,"NORMAL_GROWTH",
                                                                   "AGGRESSIVE_GROWTH")))
                                             ,"FROM_ZERO"))]

MODEL_DATA[,BACKLOG_NQ_GROWTH:=as.factor(ifelse(is.finite(BACKLOG_NQ_GROWTH)
                                             ,ifelse(BACKLOG_NQ_GROWTH<0,"NEGATIVE_GROWTH",
                                                     ifelse(BACKLOG_NQ_GROWTH==0,"NO_CHANGE",
                                                            ifelse(BACKLOG_NQ_GROWTH<=0.5,"NORMAL_GROWTH",
                                                                   "AGGRESSIVE_GROWTH")))
                                             ,"FROM_ZERO"))]

## BACKLOG_SPREAD
# Check if this is always = 1.00 in the 1-QTR version
MODEL_DATA[,BACKLOG_SPREAD:=(BACKLOG_FQ)/BACKLOG_26]


### Create a master training data set by joining all data sources:
MODEL_DATA[,MKT_PART_NUM:=trim(MKT_PART_NUM)]

PARTS_LEAD_TIMES <- unique(PARTS[,.(PARTS_ID,PUBLISHED_LEADTIME_WKS)])

## LEAD TIME summary:
MPN_PARTID_SNAP <- 
  MPN_PARTID_SNAP[,.(PARTS_ID=max(PARTS_ID))
                  ,by=.(SNAPSHOT_DATE,MKT_PART_NUM)]

setkey(PARTS_LEAD_TIMES,PARTS_ID)
setkey(MPN_PARTID_SNAP,PARTS_ID)

PARTS_SUMMARY <- PARTS_LEAD_TIMES[MPN_PARTID_SNAP]
PARTS_SUMMARY <- PARTS_SUMMARY[,.(PUBLISHED_LEADTIME_WKS=mean(PUBLISHED_LEADTIME_WKS,na.rm = TRUE)),
                               by=.(SNAPSHOT_DATE,MKT_PART_NUM)]

## ADD LEAD time to MODEL DATA
setkey(MODEL_DATA,MKT_PART_NUM,SNAPSHOT_DATE)
setkey(PARTS_SUMMARY,MKT_PART_NUM,SNAPSHOT_DATE)
MODEL_DATA <- PARTS_SUMMARY[MODEL_DATA]

## LEAD TIME in QUARTERS
MODEL_DATA[,LEAD_TIME_QUARTERS:=round(PUBLISHED_LEADTIME_WKS/13)]
MODEL_DATA[,LEAD_TIME_MONTHS:=round(PUBLISHED_LEADTIME_WKS/4)]


## ADD FAMILY - BU - DIV Info
setkey(PARTS_FAM_MAP,MKT_PART_NUM)
setkey(MODEL_DATA,MKT_PART_NUM)
MODEL_DATA <- PARTS_FAM_MAP[MODEL_DATA]

## Add MPN AGE Info
# Need to understand how this is calculated
PARTS_AGE[,MPN_START_DATE:=as.Date(MPN_START_DATE,format='%m/%d/%Y')]
PARTS_AGE[,MKT_PART_NUM:=trim(MKT_PART_NUM)]

PARTS_AGE <- PARTS_AGE[,.(MPN_START_DATE=min(MPN_START_DATE,na.rm = TRUE))
                       ,by=.(MKT_PART_NUM)]

setkey(PARTS_AGE,MKT_PART_NUM)
setkey(MODEL_DATA,MKT_PART_NUM)
MODEL_DATA <- PARTS_AGE[MODEL_DATA]

MODEL_DATA[,MPN_OLD:=(ifelse(MPN_START_DATE<(SNAPSHOT_DATE-365),TRUE,FALSE))]


### LOAD VENDAVO DATA SETS:

MPN_PRICE_OPT <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_MPN_PRICE_OPT.csv", stringsAsFactors = FALSE)

MPN_PRICE_OPT_MODEL <- unique(MPN_PRICE_OPT[,.(MKT_PART_NUM,FISCAL_YEAR_QUARTER,JERRY_MPN_VARIATION_BIN,
                                               #JERRY_SIZE_Q_BIN,
                                               PRODUCT_AGE_BIN)])

JERRY_PRICE_OPT <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_JERRY_PRICE_OPT.csv", stringsAsFactors = FALSE)

# The Vendavo QTR fields for binning features may already have the 1-QTR delta built into its name
# Need to check this to ensure the data is lined up with the proper quarter

DATE_QTR <- unique(CY_CAL[,.(CALENDAR_DATE, FISCAL_YEAR_QUARTER)]) # why prior fiscal year qtr? 
DATE_QTR[,SNAPSHOT_DATE:=as.Date(CALENDAR_DATE,format='%m/%d/%Y')]
names(DATE_QTR)[2] <- "FISCAL_YEAR_QUARTER"

DATE_QTR<- DATE_QTR[,.(SNAPSHOT_DATE,FISCAL_YEAR_QUARTER)]
setkey(DATE_QTR,SNAPSHOT_DATE)
setkey(MODEL_DATA,SNAPSHOT_DATE)
MODEL_DATA <- DATE_QTR[MODEL_DATA]

### MPN QUARTERLY METRICS DATA:
MPN_PRICE_OPT_MODEL <- MPN_PRICE_OPT_MODEL[,.(JERRY_MPN_VARIATION_BIN=first(JERRY_MPN_VARIATION_BIN),
                                              #JERRY_SIZE_Q_BIN=first(JERRY_SIZE_Q_BIN),
                                              PRODUCT_AGE_BIN=first(PRODUCT_AGE_BIN)),
                                           by=.(MKT_PART_NUM,FISCAL_YEAR_QUARTER)]
setkey(MODEL_DATA,FISCAL_YEAR_QUARTER,MKT_PART_NUM)
setkey(MPN_PRICE_OPT_MODEL,FISCAL_YEAR_QUARTER,MKT_PART_NUM)

MODEL_DATA <- MPN_PRICE_OPT_MODEL[MODEL_DATA]

# correcting JERRY vairation and product age bins
MODEL_DATA[,JERRY_MPN_VARIATION_BIN:=as.factor(ifelse(is.na(JERRY_MPN_VARIATION_BIN),"UNKNOWN",as.character(JERRY_MPN_VARIATION_BIN)))]
MODEL_DATA[,PRODUCT_AGE_BIN:=as.factor(ifelse(is.na(PRODUCT_AGE_BIN),"UNKNOWN",as.character(PRODUCT_AGE_BIN)))]


# Dropping bad LT, Ship to Book and Revenue data
MODEL_DATA <-  MODEL_DATA[is.finite(PUBLISHED_LEADTIME_WKS)]
MODEL_DATA <- MODEL_DATA[!(is.na(REVENUE_26) | is.na(BACKLOG_26))]
MODEL_DATA <- MODEL_DATA[is.finite(SHIP_TO_BOOK_RATIO)]


quantiles <- quantile(MODEL_DATA$SHIP_TO_BOOK_RATIO,
                      prob =seq(0, 1, by = 0.01),
                      na.rm = TRUE
)

# dropping outliers
MODEL_DATA <- MODEL_DATA[SHIP_TO_BOOK_RATIO>quantiles[10] & SHIP_TO_BOOK_RATIO<quantiles[90]]

## Final Prep for features:

MODEL_DATA_BACKUP <- copy(MODEL_DATA)

MODEL_DATA <- MODEL_DATA[SNAPSHOT_DATE<(TRAIN_CUTOFF_DATE)]

## BACKLOG
breaks <- quantile(MODEL_DATA$BACKLOG_26,
                   prob =seq(0, 1, by = 0.1),
                   na.rm = TRUE
)

# skewed distribution can cause the breaks to be repeated - this will collapse these duplicates

x <- (lag((!duplicated(breaks)))[-1])*seq(1, 10, by = 1)
x <- x[ x != 0 ]

labels <- paste0("BACKLOG_SIZE_",x)

breaks <- breaks[(!duplicated(breaks))]

MODEL_DATA$BACKLOG_SIZE_BIN <- cut(MODEL_DATA$BACKLOG_26,breaks,
                                   include.lowest = T, right=FALSE,
                                   labels=labels)

## BACKLOG_SPREAD_BIN
MODEL_DATA[,BACKLOG_SPREAD_BIN:=as.factor(ifelse(BACKLOG_SPREAD==1,"BIN_1",
                                                 ifelse(BACKLOG_SPREAD<1 & BACKLOG_SPREAD>=.75,"BIN_2",
                                                        ifelse(BACKLOG_SPREAD<.75 & BACKLOG_SPREAD>=.35,"BIN_3",
                                                               ifelse(BACKLOG_SPREAD<.35 & BACKLOG_SPREAD>0,"BIN_4",
                                                                      "BIN_5"
                                                               )))))]
##  Cap number of families into 20 levels
MODEL_DATA<- MODEL_DATA[as.character(FAMILY)!='NAND']

## OUtlier, abnormal cases exclusion:
MPN_excl_list <- c("CG8889ATT","CG8863AFT","CYW20734UA1KFFB3GT")
MODEL_DATA <- MODEL_DATA[!(MKT_PART_NUM %in% MPN_excl_list)]

MODEL_DATA$FAMILY[MODEL_DATA$FAMILY %in% FAMILY_value_list_exc] <- "OTHER"

## Add financial quarter column
MODEL_DATA[,FQTR:=quarter(SNAPSHOT_DATE)]


## BACKLOG_CR DATA

## SUPPLY_CONSTRAINED_FCST_WINDOW
setkey(MODEL_DATA,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
setkey(BACKLOGS_CR,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
MODEL_DATA <- BACKLOGS_CR[MODEL_DATA]
MODEL_DATA[,BACKLOG_CR:=ifelse(is.na(BACKLOG_CR),0,BACKLOG_CR)]

MODEL_DATA[,TABLE_BACKLOG_RATIO:=(BACKLOG_CR-BACKLOG_26)/BACKLOG_26]
MODEL_DATA[,TABLE_BACKLOG:=(BACKLOG_CR-BACKLOG_26)]

MODEL_DATA[,SUPPLY_CONSTRAINED:=ifelse(TABLE_BACKLOG>0,TRUE,FALSE)]

## SUPPLY_CONSTRAINED_FIRST_Q
setkey(MODEL_DATA,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
setkey(BACKLOGS_CR_FQ,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
MODEL_DATA <- BACKLOGS_CR_FQ[MODEL_DATA]
MODEL_DATA[,BACKLOG_CR_FQ:=ifelse(is.na(BACKLOG_CR_FQ),0,BACKLOG_CR_FQ)]

MODEL_DATA[,TABLE_BACKLOG_RATIO_FQ:=(BACKLOG_CR_FQ-BACKLOG_FQ)/BACKLOG_FQ]
MODEL_DATA[,TABLE_BACKLOG_FQ:=(BACKLOG_CR_FQ-BACKLOG_FQ)]

MODEL_DATA[,SUPPLY_CONSTRAINED_FQ:=ifelse(TABLE_BACKLOG_FQ>0,TRUE,FALSE)]



## LOAD WAFER Information:
MPN_WAFER_MAP <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\ML_PARTS_WAFERS_MAP_TEMP.csv", stringsAsFactors = FALSE)

MPN_WAFER_MAP <- unique(MPN_WAFER_MAP[,.(MKT_PART_NUM,DIEPART_IDV)])
MPN_WAFER_MAP[,DIEPART_IDV:=ifelse(DIEPART_IDV=='','UNKNOWN',DIEPART_IDV)]

MPN_WAFER_MAP[,MPN_COUNT:=uniqueN(MKT_PART_NUM),by=.(DIEPART_IDV)]

setkey(MPN_WAFER_MAP,MKT_PART_NUM)
setkey(MODEL_DATA,MKT_PART_NUM)

MODEL_DATA <- MPN_WAFER_MAP[MODEL_DATA]
MODEL_DATA[,MPN_COUNT:=ifelse(DIEPART_IDV=='UNKNOWN',1,MPN_COUNT)]


## DIEPART MPN COUNT BIN 
breaks <- quantile(MODEL_DATA$MPN_COUNT,
                   prob =seq(0, 1, by = 0.1),
                   na.rm = TRUE
)

x <- (lag((!duplicated(breaks)))[-1])*seq(1, 10, by = 1)
x <- x[ x != 0 ]

labels <- paste0("DIEPART_MPN_COUNT_",x)
breaks <- breaks[(!duplicated(breaks))]

MODEL_DATA$DIEPART_MPN_COUNT_BIN <- cut(MODEL_DATA$MPN_COUNT, breaks,
                                        include.lowest = T, right=FALSE,
                                        labels=labels)


## ADD Progress Feature:
PROGRESS_FOR_13WEEKS_AGO_SNAP <- PROGRESS_FOR_13WEEKS_AGO_SNAP[,!c("BACKLOG_QTY","REVENUE_13", "PROGRESS"),with=FALSE]
setkey(PROGRESS_FOR_13WEEKS_AGO_SNAP,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
setkey(MODEL_DATA,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)

MODEL_DATA <- PROGRESS_FOR_13WEEKS_AGO_SNAP[MODEL_DATA]
MODEL_DATA[,PROGRESS_FACTOR:=as.factor(ifelse(is.na(PROGRESS_FACTOR),"NO_ACTIVITY",as.character(PROGRESS_FACTOR)))]

## MONTHS_TO_FCST_WINDOW
MODEL_DATA[,MONTHS_TO_FCST_WINDOW:=
             as.factor(as.integer(round((as.yearmon(as.yearqtr(SNAPSHOT_DATE)+0.25)-as.yearmon(SNAPSHOT_DATE))*12)))]
## WEEKS_TO_FCST_WINDOW
MODEL_DATA[,WEEKS_TO_FCST_WINDOW:=
             as.factor(round(as.integer(
               difftime(as.yearmon(as.yearqtr(SNAPSHOT_DATE)+0.25),SNAPSHOT_DATE,units="weeks")
             )))]

## Add CUST_CATAB Col
setkey(MODEL_DATA,SNAPSHOT_DATE,CURRENT_FC_CUSTOMER)
setkey(BACKLOG_FC_CAT,SNAPSHOT_DATE,CURRENT_FC_CUSTOMER)
MODEL_DATA <- BACKLOG_FC_CAT[MODEL_DATA]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Feature engineering done - now build and test the models


## Bin descrete Numeric varables follwoing factorizing:

## factorinze all categorical variables:
categorical_variables<-
  c(
    "CURRENT_FC_CUSTOMER",
    "FAMILY",
    "DIVISION",
    "BIZ_UNIT",
    "DIEPART_IDV",
    #"ASSET_CODE",
    "LEAD_TIME_MONTHS",
    "BACKLOG_SIZE_BIN",
    #"DISCOUNT_BIN",
    "JERRY_MPN_VARIATION_BIN",
    "PRODUCT_AGE_BIN",
    "MPN_OLD",
    "SUPPLY_CONSTRAINED",
    "SUPPLY_CONSTRAINED_FQ",
    #"SUPPLY_CONSTRAINED_SQ",
    "DIEPART_MPN_COUNT_BIN",
    "BACKLOG_GROWTH",
    "BACKLOG_FQ_GROWTH",
    "BACKLOG_SQ_GROWTH",
    "BACKLOG_NQ_GROWTH",
    "PROGRESS_FACTOR",
    #"MKT_INDEX_BIN",
    #"MONTH",
    #"MONTHS_TO_FCST_WINDOW",
    "WEEKS_TO_FCST_WINDOW",
    "BACKLOG_SPREAD_BIN",
    #"FORECAST",
    #"JERRY_SIZE_Q_BIN",
    "CURRENT_FC_CATAB"
  )

MODEL_DATA[, (categorical_variables) := lapply(.SD, as.factor), .SDcols = categorical_variables]

MODEL_DATA_BTBLE <- MODEL_DATA[FAMILY=="BTBLE"]

PREDS<- setdiff(c(categorical_variables),
                c("CURRENT_FC_CUSTOMER",
                  "PRODUCT_AGE_BIN","FORECAST",
                  "MONTHS_TO_FCST_WINDOW","WEEKS_TO_FCST_WINDOW", "DIEPART_IDV", "DIVISION", "BIZ_UNIT"))

Y_VAR<- c("SHIP_TO_BOOK_RATIO")


path <- "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR"
setwd(path)
library(h2o)
# make sure the port # works for whoever is going to log into the R-SVR
# defult is 54321 - also try h2o.init()
h2o.init(ip='localhost', nthreads=-1, min_mem_size='10G', max_mem_size='20G')  #check this later : issue with port

for (i in 0:13){

model_data_h2o <- as.h2o(MODEL_DATA_BTBLE[WEEKS_TO_FCST_WINDOW==i])
split_h2o_cv <- h2o.splitFrame(model_data_h2o, c(0.2), seed = 123 )
train_cv_h2o <- h2o.assign(split_h2o_cv[[2]], "train" ) # 80%
valid_cv_h2o <- h2o.assign(split_h2o_cv[[1]], "test" ) # 20%

source("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\MODEL_TUNING.R")

# This can be removed - it was in here before we started the hyper-tuning steps above 
#gbm_model <- h2o.gbm(y = Y_VAR,
#                    x = PREDS,
#                    training_frame = train_cv_h2o,
#                    validation_frame = valid_cv_h2o,
#                    ntrees = 10000,
#                    learn_rate=0.01,
#                    sample_rate = 0.8,
#                    min_rows = 5,
#                    col_sample_rate = 0.8,
#                    score_tree_interval = 10,
#                    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "RMSE",
#                    nfolds = 4,
#                    seed = 0xDECAF)

gbm_model <- gbm_fine_tuned ## run Model_tuning file
gbm_var_imp <- h2o.varimp(gbm_model)

gbm_var_imp
h2o.varimp_plot(gbm_model,20)

# we could access model performance metrics and log them for analysis

# In this version we force smart names into the output model files
h2o.saveModel(object=gbm_model, path=getwd(), force=TRUE)
##DOLLARS, QUANT MODELS
name <- file.path(getwd(),paste0("SHIPPMENTS_",MODEL_TYPE,"_2QFCST_MODEL_",i,"WEEKS_TOGO",Sys.Date(),"_BTBLE"))
file.rename(file.path(getwd(), gbm_model@model_id), name)

name

# h2o.partialPlot(object = gbm_model, data = valid_cv_h2o, cols = c("WEEKS_TO_FCST_WINDOW"))

}



detach("package:h2o",unload = TRUE)

rm(gbm_fine_tuned)
rm(gbm_model)
rm(gbm_var_imp)
rm(model_data_h2o)
rm(grid)
rm(hyper_params)
rm(search_criteria)
rm(sortedGrid)
rm(split_h2o_cv)
rm(train_cv_h2o)
rm(valid_cv_h2o)
rm(minDepth)
rm(maxDepth)
rm(topDepths)
rm(i)



# Random Forest Implementation

rf_model <- h2o.randomForest(y = Y_VAR,
                             x = PREDS,
                             ntrees = 50,
                             training_frame = train_cv_h2o,
                             validation_frame = valid_cv_h2o,
                             score_tree_interval = 10,
                             stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MAE",
                             nfolds = 4,
                             seed = 0xDECAF
)
h2o.varimp_plot(rf_model, 20)
h2o.performance(rf_model,valid_cv_h2o)

detach("package:h2o",unload = TRUE)
