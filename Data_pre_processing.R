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
library(gdata)

## Model for  DOLLARS VS QUANTITY
# this one is used later so it can do dollars or quantity model
MODEL_TYPE <- "DOLLARS"

FAMILY_value_list_exc<- c("TT","AGIGA","COM",""," ",NA)

# One Qlik app in GSGR home area builds all of these CSV files
# Need to update the app name here: <******>

## load Cypress Calender:
# make sure this .csv is automatically generated - because it changes slightly each day
CY_CAL <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\CALENDAR.csv", stringsAsFactors = FALSE)

## load Data:
PARTS <- fread('C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_PARTS_TEMP.csv', stringsAsFactors = FALSE)

# Check why we have the unique() function below
PARTS_FAM_MAP <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_PARTS_FAMILY_TEMP.csv", stringsAsFactors = FALSE)
PARTS_FAM_MAP <- unique(PARTS_FAM_MAP)

# Check why we have the unique() function below
PARTS_AGE <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_PARTS_AGE_TEMP.csv", stringsAsFactors = FALSE)
PARTS_AGE <- unique(PARTS_AGE)

LEAD_TIME <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_LEAD_TIME.csv", stringsAsFactors = FALSE)

BACKLOGS_RAW <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_BACKLOGS_TEMP.csv", stringsAsFactors = FALSE)

REVENUE_RAW <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_REVENUE_TEMP.csv", stringsAsFactors = FALSE)

## Load last 3 years CAT-A/B information:
## Custom file created by GSGR based on input from Gana
## Using this to prevent issues where customers move from cat-A to cat-B
## Where this occurs you can have backlog on one FC_CUST and shipments on a different FC_CUST

## Look at tweaking the below logic so the '| CURRENT_FC_CATAB=="CAT B"' doesn't introduce 
## CAT-B customers that moved between CAT-A,CAT-B
## may need a list of customers that changed

## The below lines totally remove line items which do not have a FC customer in the CAT-A
##  or if the line is marked as CAT B

CATA_17_19_LIST <- fread('C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\MASTER_FC_CUST_17-19.csv')

LEAD_TIME <- fread('C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_LEAD_TIME.csv')


BACKLOGS_RAW <- BACKLOGS_RAW[CURRENT_FC_CUSTOMER %in% unique(CATA_17_19_LIST$CURRENT_FC_CUSTOMER)
                             | CURRENT_FC_CATAB=="CAT B"]
REVENUE_RAW <- REVENUE_RAW[CURRENT_FC_CUSTOMER %in% unique(CATA_17_19_LIST$CURRENT_FC_CUSTOMER)
                           | CURRENT_FC_CATAB=="CAT B"]

# MPN name alias table (right now it's for MB and BCM) - need this CSV file in the single Qlik app

OLD_NEW_MPN_MAP <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\MB_BCM_TO_CY_PARTS_MAPPING.csv", stringsAsFactors = FALSE)
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

BACKLOGS_RS <-BACKLOGS_RAW[as.yearqtr(RS_DATE)> as.yearqtr(SNAPSHOT_DATE) &
                          as.yearqtr(RS_DATE) <= as.yearqtr(SNAPSHOT_DATE)+0.5
                        ,.(BACKLOG_RS=sum(QUANTITY,na.rm = TRUE))
                        ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]

BACKLOGS_RS <- BACKLOGS_RS[order(SNAPSHOT_DATE)]

## BACKLOG for First Quarter of Forecast Window:
BACKLOGS_RS_FQ <-BACKLOGS_RAW[as.yearqtr(RS_DATE)> as.yearqtr(SNAPSHOT_DATE) &
                             as.yearqtr(RS_DATE) < as.yearqtr(SNAPSHOT_DATE)+0.5 #current qtr + 1 because this is less than and not less than equal to
                           ,.(BACKLOG_RS_FQ=sum(QUANTITY,na.rm = TRUE))
                           ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]

## BACKLOG for Second Quarter of Forecast Window:
BACKLOGS_RS_SQ <-BACKLOGS_RAW[as.yearqtr(RS_DATE)> as.yearqtr(SNAPSHOT_DATE)+0.25 &
                             as.yearqtr(RS_DATE) < as.yearqtr(SNAPSHOT_DATE)+0.75
                           ,.(BACKLOG_RS_SQ=sum(QUANTITY,na.rm = TRUE))
                           ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]

# using the 4th quarter as a feature may help predict Q2/Q3 

## BACKLOG for Next Quarter After the Forecast Window:
BACKLOGS_RS_NQ <-BACKLOGS_RAW[as.yearqtr(RS_DATE) - as.yearqtr(SNAPSHOT_DATE) >= 0.75 # why equal to and not greater than equal to? 
                           ,.(BACKLOG_RS_NQ=sum(QUANTITY,na.rm = TRUE))
                           ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]


############################### New feature for testing ###########################

BACKLOGS_RS_CURR_QTR <- BACKLOGS_RAW[(as.yearqtr(RS_DATE) == as.yearqtr(SNAPSHOT_DATE)),
                                  .(BACKLOG_RS_CURR_QTR=sum(QUANTITY, na.rm=TRUE)),
                                  by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)]

###################################################################################

#Merging everything into a single BACKLOGS table

# setkey(BACKLOGS_SQ,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
# BACKLOGS <- BACKLOGS_SQ[BACKLOGS]
# BACKLOGS[,BACKLOG_SQ:=ifelse(is.na(BACKLOG_SQ),0,BACKLOG_SQ)]
# 
# setkey(BACKLOGS_FQ, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
# BACKLOGS <- BACKLOGS_FQ[BACKLOGS]
# BACKLOGS[,BACKLOG_FQ:=ifelse(is.na(BACKLOG_FQ), 0, BACKLOG_FQ)]
# 
# setkey(BACKLOGS_NQ, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
# BACKLOGS <- BACKLOGS_NQ[BACKLOGS]
# BACKLOGS[, BACKLOG_NQ:=ifelse(is.na(BACKLOG_NQ),0,BACKLOG_NQ)]
# 
# setkey(BACKLOGS_CURR_QTR, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
# BACKLOGS <- BACKLOGS_CURR_QTR[BACKLOGS]
# BACKLOGS[, BACKLOG_CURR_QTR:=ifelse(is.na(BACKLOG_CURR_QTR), 0, BACKLOG_CURR_QTR)]

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


############################### New feature for testing ###########################

## BACKLOG for Next Quarter After the Forecast Window:
BACKLOGS_CR_NQ <-BACKLOGS_RAW_CR[as.yearqtr(CR_DATE) - as.yearqtr(SNAPSHOT_DATE) >= 0.75 # why equal to and not greater than equal to? 
                           ,.(BACKLOG_CR_NQ=sum(QUANTITY,na.rm = TRUE))
                           ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)]

## CR for current qtr 
BACKLOGS_CR_CURR_QTR <- BACKLOGS_RAW_CR[(as.yearqtr(CR_DATE) == as.yearqtr(SNAPSHOT_DATE)),
                                  .(BACKLOG_CR_CURR_QTR=sum(QUANTITY, na.rm=TRUE)),
                                  by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)]


######################################################################################

## This one finds the backlog for the window of interest 13 weeks ago (has it grown a bunch?)
## This particular example does a 1-QTR window
## 13 weeks Lag Backlog for given snapshot:
BACKLOG_26_FOR_13WEEKS_AGO <- copy(BACKLOGS_CR)
BACKLOG_26_FOR_13WEEKS_AGO <- BACKLOG_26_FOR_13WEEKS_AGO[,SNAPSHOT_DATE:=(SNAPSHOT_DATE+(7*13))]

setkey(BACKLOGS_CR, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
setkey(BACKLOGS_CR_SQ,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
BACKLOGS <- BACKLOGS_CR_SQ[BACKLOGS_CR]
BACKLOGS[,BACKLOG_CR_SQ:=ifelse(is.na(BACKLOG_CR_SQ),0,BACKLOG_CR_SQ)]

setkey(BACKLOGS_CR_FQ, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
BACKLOGS <- BACKLOGS_CR_FQ[BACKLOGS]
BACKLOGS[,BACKLOG_CR_FQ:=ifelse(is.na(BACKLOG_CR_FQ), 0, BACKLOG_CR_FQ)]

setkey(BACKLOGS_CR_NQ, CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)
BACKLOGS <- BACKLOGS_CR_NQ[BACKLOGS]
BACKLOGS[, BACKLOG_CR_NQ:=ifelse(is.na(BACKLOG_CR_NQ),0,BACKLOG_CR_NQ)]

BACKLOGS <- merge(BACKLOGS, BACKLOGS_CR_CURR_QTR, by.x=c("SNAPSHOT_DATE", "MKT_PART_NUM", "CURRENT_FC_CUSTOMER"),
                        by.y=c("SNAPSHOT_DATE", "MKT_PART_NUM", "CURRENT_FC_CUSTOMER"), all.x=TRUE, all.y=TRUE)

BACKLOGS[,BACKLOG_CR_SQ:=ifelse(is.na(BACKLOG_CR_SQ),0,BACKLOG_CR_SQ)]
BACKLOGS[,BACKLOG_CR_FQ:=ifelse(is.na(BACKLOG_CR_FQ), 0, BACKLOG_CR_FQ)]
BACKLOGS[, BACKLOG_CR_NQ:=ifelse(is.na(BACKLOG_CR_NQ),0,BACKLOG_CR_NQ)]
BACKLOGS[, BACKLOG_CR_CURR_QTR:=ifelse(is.na(BACKLOG_CR_CURR_QTR),0,BACKLOG_CR_CURR_QTR)]

# Using ORD_ENT_DATE
BACKLOGS_RAW_ORD_ENT <- BACKLOGS_RAW_ORD_ENT[DISTI_CORP_ID!='CYTECH',
                                             .(QUANTITY=sum(QUANTITY,na.rm=TRUE)),
                                             by=.(SNAPSHOT_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM,PARTS_ID,ORD_ENT_DATE)]

BACKLOGS_RAW_ORD_ENT[,ORD_ENT_DATE:=as.Date(ORD_ENT_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW_ORD_ENT[,SNAPSHOT_DATE:=as.Date(SNAPSHOT_DATE,format='%m/%d/%Y')]
BACKLOGS_RAW_ORD_ENT<- BACKLOGS_RAW_ORD_ENT[SNAPSHOT_DATE==max(SNAPSHOT_DATE),!c("SNAPSHOT_DATE"),with=FALSE]


## creating another feature to identify the gap between invoice date and order entry date?
REVENUE_MODEL<- copy(REVENUE_RAW)

REVENUE_MODEL <- (REVENUE_MODEL[DISTI_CORP_ID!='CYTECH'])

REVENUE_MODEL[,QUANTITY:=ifelse(is.na(QUANTITY),0,QUANTITY)]
REVENUE_MODEL[,END_CUSTOMER:=ifelse(END_CUSTOMER=='' | is.na(END_CUSTOMER),"UNKNOWN",END_CUSTOMER)]
REVENUE_MODEL <- REVENUE_MODEL[!(MKT_PART_NUM=='' | DISTI_CORP_ID=='')]
REVENUE_MODEL[,CR_DATE:=floor_date(as.Date(CR_DATE,format='%m/%d/%Y'),unit = "week")]

REVENUE_MODEL <- REVENUE_MODEL[,.(QUANTITY=sum(QUANTITY,na.rm=TRUE))
                               ,by=.(CR_DATE,CURRENT_FC_CUSTOMER,MKT_PART_NUM,PARTS_ID)]

# Create a list of dates from the min to the max to cover the entire timeframe
# Join to the revenue data 
# This structure is commonly used in other models to transform
# records in tables without snapshots to link to tables with snapshots
# This structure is slow with data.table join, but works much faster with data.frame + merge

WEEKS <- setDT(as.data.frame((seq(min(BACKLOGS$SNAPSHOT_DATE),max(BACKLOGS$SNAPSHOT_DATE),by="week"))))
names(WEEKS)<- c("WEEK_DATES")

# by=NULL does full outer join all possible combinations

REVENUE_MODEL <- setDT(merge(as.data.frame(REVENUE_MODEL),as.data.frame(WEEKS),by=NULL))
REVENUE_MODEL[,FRWD_WEEKS:=round(as.numeric(difftime(CR_DATE,WEEK_DATES,units="weeks")))]
TEMP_REVENUE_DATA <- copy(REVENUE_MODEL)
REVENUE_MODEL <- REVENUE_MODEL[FRWD_WEEKS>0]

# New Addition
# 26 Week rolling RS backlog from snapshot date
BACKLOG_26WEEKS_13WEEKS_AGO_ROLLING <- BACKLOGS_RAW_CR[((CR_DATE - SNAPSHOT_DATE <= 91*2) & (CR_DATE > SNAPSHOT_DATE)), 
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

# WEEKS_DATES here is the snapshot field used to do the full outer join above
REVENUE_MODEL_BACKUP <- copy(REVENUE_MODEL)

REVENUE_MODEL <-REVENUE_MODEL[as.yearqtr(CR_DATE)> as.yearqtr(WEEK_DATES) &
                                as.yearqtr(CR_DATE) <= as.yearqtr(WEEK_DATES)+0.5
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
    BACKLOG_CURR_QTR=sum(BACKLOG_CR_CURR_QTR, na.rm=TRUE),
    BACKLOG_26=sum(BACKLOG_CR, na.rm = TRUE),
    BACKLOG_FQ=sum(BACKLOG_CR_FQ,na.rm = TRUE),
    BACKLOG_SQ=sum(BACKLOG_CR_SQ,na.rm = TRUE),
    BACKLOG_NQ=sum(BACKLOG_CR_NQ,na.rm = TRUE)
  )
  ,by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
  ]

# Find first occurence of each combo of FC_CUST/MPN
# Need to make sure we understand why this one is being done
# Redundant lines (not being used anywhere)

#MODEL_DATA[,MIN_SNAP:=min(SNAPSHOT_DATE),
#          by=.(CURRENT_FC_CUSTOMER,MKT_PART_NUM)]

#MODEL_DATA <- MODEL_DATA[SNAPSHOT_DATE < TRAIN_CUTOFF_DATE] # why is this being done here itself? 
MODEL_DATA[, SHIP_TO_BOOK_RATIO:=REVENUE_26/BACKLOG_26]

# Removing SNAPSHOT_DATE- 2018-11-02 (Backlog is 0 all across)
MODEL_DATA <- MODEL_DATA[!(SNAPSHOT_DATE == "2018-02-11")]
MODEL_DATA <- MODEL_DATA[!(SNAPSHOT_DATE == "2018-02-07")]

## BACKLOG_SPREAD
# Check if this is always = 1.00 in the 1-QTR version
MODEL_DATA[,BACKLOG_SPREAD:=(BACKLOG_FQ)/BACKLOG_26]


### Create a master training data set by joining all data sources:
MODEL_DATA[,MKT_PART_NUM:=trim(MKT_PART_NUM)]

## LEAD TIME summary:
needed_cal_cols <- c('FIRST_DAY_OF_WEEK', 'FISCAL_YEAR_WEEK')
date_for_LT <- CY_CAL[,needed_cal_cols, with=FALSE]
date_for_LT[, SNAPSHOT_DATE:=as.Date(FIRST_DAY_OF_WEEK, format="%m/%d/%Y")]
date_for_LT[, FIRST_DAY_OF_WEEK:=NULL]
names(date_for_LT)[1] <- c("WORK_WEEK")
date_for_LT <- date_for_LT[!is.na(WORK_WEEK)]
date_for_LT[, SNAPSHOT_DATE:=SNAPSHOT_DATE]


LEAD_TIME <- LEAD_TIME[!duplicated(LEAD_TIME), ]

# cleaning up LEAD_TIME for MB->CY and BCM _> CY conversions
CLEAN_UP <- LEAD_TIME[MKT_PART_NUM %in% OLD_NEW_MPN_MAP$MKT_PART_NUM]
LEAD_TIME <- LEAD_TIME[! (MKT_PART_NUM %in% OLD_NEW_MPN_MAP$MKT_PART_NUM)]
setkey(CLEAN_UP, MKT_PART_NUM)
setkey(OLD_NEW_MPN_MAP, MKT_PART_NUM)

CLEAN_UP[OLD_NEW_MPN_MAP, on="MKT_PART_NUM", PART_NUM:=i.NEW_MPN]
CLEAN_UP[, MKT_PART_NUM:=PART_NUM]
CLEAN_UP[, PART_NUM:=NULL]

LEAD_TIME <- rbind(LEAD_TIME, CLEAN_UP)
LEAD_TIME <- LEAD_TIME[, .(PUBLISHED_LT = mean(PUBLISHED_LT_WKS)),
                       by=.(WORK_WEEK, MKT_PART_NUM)]

setkey(date_for_LT, WORK_WEEK)
setkey(LEAD_TIME, WORK_WEEK)

LEAD_TIME <- LEAD_TIME[date_for_LT, on="WORK_WEEK", SNAPSHOT_DATE:=SNAPSHOT_DATE]
LEAD_TIME[, SNAPSHOT_DATE:= SNAPSHOT_DATE - 1]

# ## ADD LEAD time to MODEL DATA
setkey(MODEL_DATA,MKT_PART_NUM,SNAPSHOT_DATE)
setkey(LEAD_TIME,MKT_PART_NUM,SNAPSHOT_DATE)

MODEL_DATA <- LEAD_TIME[MODEL_DATA]

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

MPN_PRICE_OPT <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_MPN_PRICE_OPT.csv", stringsAsFactors = FALSE)

MPN_PRICE_OPT_MODEL <- unique(MPN_PRICE_OPT[,.(MKT_PART_NUM,FISCAL_YEAR_QUARTER,JERRY_MPN_VARIATION_BIN,
                                               #JERRY_SIZE_Q_BIN,
                                               PRODUCT_AGE_BIN)])

JERRY_PRICE_OPT <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_JERRY_PRICE_OPT.csv", stringsAsFactors = FALSE)

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
setkey(BACKLOGS_RS,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
MODEL_DATA <- BACKLOGS_RS[MODEL_DATA]
MODEL_DATA[,BACKLOG_RS:=ifelse(is.na(BACKLOG_RS),0,BACKLOG_RS)]

MODEL_DATA[,TABLE_BACKLOG_RATIO:=(BACKLOG_26-BACKLOG_RS)/BACKLOG_26]
MODEL_DATA[,TABLE_BACKLOG:=(BACKLOG_26-BACKLOG_RS)]

MODEL_DATA[,SUPPLY_CONSTRAINED:=ifelse(TABLE_BACKLOG>0,TRUE,FALSE)]

## SUPPLY_CONSTRAINED_FIRST_Q
setkey(MODEL_DATA,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
setkey(BACKLOGS_RS_FQ,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)
MODEL_DATA <- BACKLOGS_RS_FQ[MODEL_DATA]
MODEL_DATA[,BACKLOG_RS_FQ:=ifelse(is.na(BACKLOG_RS_FQ),0,BACKLOG_RS_FQ)]

MODEL_DATA[,TABLE_BACKLOG_RATIO_FQ:=(-BACKLOG_RS_FQ+BACKLOG_FQ)/BACKLOG_FQ]
MODEL_DATA[,TABLE_BACKLOG_FQ:=(-BACKLOG_RS_FQ+BACKLOG_FQ)]

MODEL_DATA[,SUPPLY_CONSTRAINED_FQ:=ifelse(TABLE_BACKLOG_FQ>0,TRUE,FALSE)]

## ADD Progress Feature:
PROGRESS_FOR_13WEEKS_AGO_SNAP <- PROGRESS_FOR_13WEEKS_AGO_SNAP[,!c("BACKLOG_QTY","REVENUE_13", "PROGRESS"),with=FALSE]
setkey(PROGRESS_FOR_13WEEKS_AGO_SNAP,CURRENT_FC_CUSTOMER,MKT_PART_NUM,SNAPSHOT_DATE)

## LOAD WAFER Information:
MPN_WAFER_MAP <- fread("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\Forecasting GSGR\\Data files\\Data Files\\ML_PARTS_WAFERS_MAP_TEMP.csv", stringsAsFactors = FALSE)

MPN_WAFER_MAP <- unique(MPN_WAFER_MAP[,.(MKT_PART_NUM,DIEPART_IDV)])
MPN_WAFER_MAP[,DIEPART_IDV:=ifelse(DIEPART_IDV=='','UNKNOWN',DIEPART_IDV)]

## MONTHS_TO_FCST_WINDOW
MODEL_DATA[,MONTHS_TO_FCST_WINDOW:=
             as.factor(as.integer(round((as.yearmon(as.yearqtr(SNAPSHOT_DATE)+0.25)-as.yearmon(SNAPSHOT_DATE))*12)))]

## WEEKS_TO_FCST_WINDOW
MODEL_DATA[,WEEKS_TO_FCST_WINDOW:=
             as.factor(round(as.integer(
               difftime(as.yearmon(as.yearqtr(SNAPSHOT_DATE)+0.25),SNAPSHOT_DATE,units="weeks")
             )))]

####################

REVENUE_DATA <- copy(REVENUE_RAW)
REVENUE_DATA[, INVOICE_DATE:=as.Date(INVOICE_DATE, format="%m/%d/%Y")]
REVENUE_DATA[,END_CUSTOMER:=ifelse(END_CUSTOMER=='' | is.na(END_CUSTOMER),"UNKNOWN",END_CUSTOMER)]
REVENUE_DATA[,QUANTITY:=ifelse(is.na(QUANTITY),0,QUANTITY)]
REVENUE_DATA[, INVOICE_QTR:=as.yearqtr(INVOICE_DATE)]
REVENUE_DATA <- REVENUE_DATA[INVOICE_QTR > "2017 Q2"]
REVENUE_DATA[, INVOICE_QTR:=INVOICE_QTR+0.25]

REVENUE_DATA_FINAL <- REVENUE_DATA[, .(REVENUE_HIST = sum(QUANTITY, na.rm=TRUE)), by=.(INVOICE_QTR, CURRENT_FC_CUSTOMER, MKT_PART_NUM)]

MODEL_DATA[, SNAPSHOT_QTR:=as.yearqtr(SNAPSHOT_DATE)]

FINAL_MODEL_DATA <- merge(MODEL_DATA, REVENUE_DATA_FINAL, by.x = c("SNAPSHOT_QTR", "CURRENT_FC_CUSTOMER", "MKT_PART_NUM"), 
                          by.y=c("INVOICE_QTR", "CURRENT_FC_CUSTOMER", "MKT_PART_NUM"), all.x=TRUE)

FINAL_MODEL_DATA[, REVENUE_HIST:=ifelse(is.na(REVENUE_HIST), 0, REVENUE_HIST)]
FINAL_MODEL_DATA[, REVENUE_HIST:=ifelse(REVENUE_HIST < 0, 0, REVENUE_HIST)]

auto_families <- c("AUTO MCU", "NOR", "AUTOPSOC", "WLAUTO")
FINAL_MODEL_DATA[, segment:=ifelse(FAMILY %in% auto_families, "AUTO", "NON-AUTO")]

FINAL_MODEL_DATA <- FINAL_MODEL_DATA[!(is.na(SHIP_TO_BOOK_RATIO)|is.infinite(SHIP_TO_BOOK_RATIO))]
FINAL_MODEL_DATA <- FINAL_MODEL_DATA[(SHIP_TO_BOOK_RATIO > 0) & (SHIP_TO_BOOK_RATIO <= 10)]

colnames(FINAL_MODEL_DATA) <- tolower(colnames(FINAL_MODEL_DATA))


# ###############################################
# ########### Ship to Book Lag $$$$$$$$$$$$$$$$$$
# 
# # Ship to Book Lab calculation
# Q417_Revenue <- REVENUE_MODEL_BACKUP[(as.yearqtr(CR_DATE) == as.yearqtr(WEEK_DATES))
#                                      , .(Q4_REVENUE = sum(QUANTITY, na.rm=TRUE)), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, WEEK_DATES)]
# Q417_Revenue <- Q417_Revenue[WEEK_DATES <= "2017-12-31"]
# 
# # Q417 CR 
# Q417_Backlog <- BACKLOGS_RAW_CR[(as.yearqtr(CR_DATE) == as.yearqtr(SNAPSHOT_DATE)),
#                                 .(CR_BACKLOG_FQ = sum(QUANTITY, na.rm=TRUE)),
#                                 by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)]
# Q417_Backlog <- Q417_Backlog[SNAPSHOT_DATE <= "2017-12-31"]
# 
# 
# final_Q417 <- merge(Q417_Backlog, Q417_Revenue, by.y=c("MKT_PART_NUM", "CURRENT_FC_CUSTOMER", "WEEK_DATES"), 
#                     by.x=c("MKT_PART_NUM", "CURRENT_FC_CUSTOMER", "SNAPSHOT_DATE"), all.x=TRUE)
# 
# lags_Q417 <- final_Q417[SNAPSHOT_DATE == '2017-10-08']
# lags_Q417[, Q4_REVENUE:= ifelse(is.na(Q4_REVENUE), 0, Q4_REVENUE)]
# lags_Q417[, S2B_RATIO := Q4_REVENUE/CR_BACKLOG_FQ]
# lags_Q417[, SNAPSHOT_QTR:= as.yearqtr(SNAPSHOT_DATE)]
# 
# names(lags_Q417) <- c("MKT_PART_NUM", "CURRENT_FC_CUSTOMER", "SNAPSHOT_DATE", "BACKLOG_CR_FQ", "REVENUE", "S2B_RATIO", "SNAPSHOT_QTR")
# lags_Q417[, SNAPSHOT_QTR:= SNAPSHOT_QTR - 0.25]
# 
# 
# #### For all other quarters
# backlog_lag <- BACKLOGS_RAW_CR[(as.yearqtr(CR_DATE) > as.yearqtr(SNAPSHOT_DATE)) &
#                                  (as.yearqtr(CR_DATE) <= as.yearqtr(SNAPSHOT_DATE) + 0.25),
#                                .(BACKLOG_CR_FQ = sum(QUANTITY, na.rm=TRUE)),
#                                by=.(SNAPSHOT_DATE, CURRENT_FC_CUSTOMER, MKT_PART_NUM)]
# 
# backlog_lag[,WEEKS_TO_FCST_WINDOW:=
#               as.factor(round(as.integer(
#                 difftime(as.yearmon(as.yearqtr(SNAPSHOT_DATE)+0.25),SNAPSHOT_DATE,units="weeks")
#               )))]
# 
# backlog_lag <- backlog_lag[WEEKS_TO_FCST_WINDOW == 1]
# 
# 
# revenue_lag <- REVENUE_MODEL_BACKUP[((as.yearqtr(CR_DATE) > as.yearqtr(WEEK_DATES)) & (as.yearqtr(CR_DATE) <= as.yearqtr(WEEK_DATES) + 0.25))
#                                     ,.(REVENUE = sum(QUANTITY, na.rm=TRUE)), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, WEEK_DATES)]
# 
# names(revenue_lag)[3] <- "SNAPSHOT_DATE"
# 
# final_lag <- merge(backlog_lag, revenue_lag, by.x=c("SNAPSHOT_DATE", "MKT_PART_NUM", "CURRENT_FC_CUSTOMER"),
#                    by.y=c("SNAPSHOT_DATE", "MKT_PART_NUM", "CURRENT_FC_CUSTOMER"), all.x=TRUE)
# 
# final_lag[, REVENUE:= ifelse(is.na(REVENUE), 0, REVENUE)]
# final_lag[, S2B_RATIO:= REVENUE/BACKLOG_CR_FQ]
# final_lag[, WEEKS_TO_FCST_WINDOW:=NULL]
# final_lag[, SNAPSHOT_QTR:= as.yearqtr(SNAPSHOT_DATE) + 0.25]
# 
# final_lag <- rbind(final_lag, lags_Q417)
# final_lag <- final_lag[S2B_RATIO > 0 & S2B_RATIO <=10]
# 
# customer_lags <- final_lag[, .(customer_lag = mean(S2B_RATIO)), by=.(SNAPSHOT_QTR, CURRENT_FC_CUSTOMER)]
# mpn_lags <- final_lag[, .(mpn_lag=mean(S2B_RATIO)), by=.(SNAPSHOT_QTR, MKT_PART_NUM)]
# 
# # avg lags
# avg_lag <- final_lag[, mean(S2B_RATIO)]
# avg_customer_lag <- customer_lags[, mean(customer_lag)]
# avg_mpn_lag <- mpn_lags[, mean(mpn_lag)]
# 
# 
# final_lag[, SNAPSHOT_QTR:= SNAPSHOT_QTR + 0.25]
# names(final_lag) <- tolower(names(final_lag))
# names(customer_lags) <- tolower(names(customer_lags))
# names(mpn_lags) <- tolower(names(mpn_lags))
# 
# #dropping un-necessary cols
# final_lag[, snapshot_date:=NULL]
# final_lag[, backlog_cr_fq:=NULL]
# final_lag[, revenue:=NULL]
# final_lag[, s2b_lag := s2b_ratio]
# final_lag[, s2b_ratio:=NULL]
# 
# # merging everything
# FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, final_lag, by=c('snapshot_qtr', 'current_fc_customer', 'mkt_part_num'), all.x=TRUE)
# FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, customer_lags, by=c('snapshot_qtr', 'current_fc_customer'), all.x=TRUE)
# FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, mpn_lags, by=c('snapshot_qtr', 'mkt_part_num'), all.x=TRUE)
# 
# # # impute missing lags with averages
# FINAL_MODEL_DATA[, s2b_lag:= ifelse(is.na(s2b_lag), avg_lag, s2b_lag)]
# FINAL_MODEL_DATA[, mpn_lag:= ifelse(is.na(mpn_lag), avg_mpn_lag, mpn_lag)]
# FINAL_MODEL_DATA[, customer_lag:= ifelse(is.na(customer_lag), avg_customer_lag, customer_lag)]
# 
# # # removing 2017 out of training data
# FINAL_MODEL_DATA <- FINAL_MODEL_DATA[snapshot_qtr > "2017 Q4"]
# MODEL_DATA <- copy(FINAL_MODEL_DATA)
# 
# 




######################################### More lag code $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# # lags
Lags <- FINAL_MODEL_DATA[, .(s2b_lag = mean(ship_to_book_ratio)), by=.(snapshot_qtr, mkt_part_num, current_fc_customer)]
customer_lags <- FINAL_MODEL_DATA[, .(customer_lag = mean(ship_to_book_ratio)), by=.(snapshot_qtr, current_fc_customer)]
mpn_lags <- FINAL_MODEL_DATA[, .(mpn_lag = mean(ship_to_book_ratio)), by=.(snapshot_qtr, mkt_part_num)]

# average values to fill missing vals
avg_Lags <- Lags[, mean(s2b_lag)]
avg_customer_lags <- customer_lags[, mean(customer_lag)]
avg_mpn_lag <- mpn_lags[, mean(mpn_lag)]


# increment snapshot qtr
Lags[, snapshot_qtr:= snapshot_qtr + 0.5]
customer_lags[, snapshot_qtr:= snapshot_qtr + 0.5]
mpn_lags[, snapshot_qtr:= snapshot_qtr + 0.5]


# setkeys and merge
setkey(Lags, snapshot_qtr, mkt_part_num, current_fc_customer)
setkey(FINAL_MODEL_DATA, snapshot_qtr, mkt_part_num, current_fc_customer)
FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, Lags, by = c("snapshot_qtr", "mkt_part_num", "current_fc_customer"), all.x = TRUE)


setkey(customer_lags, snapshot_qtr, current_fc_customer)
setkey(FINAL_MODEL_DATA, snapshot_qtr, current_fc_customer)
FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, customer_lags, by = c('snapshot_qtr', 'current_fc_customer'), all.x = TRUE)


setkey(mpn_lags, snapshot_qtr, mkt_part_num)
setkey(FINAL_MODEL_DATA, snapshot_qtr, mkt_part_num)
FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, mpn_lags, by = c('snapshot_qtr', 'mkt_part_num'), all.x = TRUE)


# impute missing lags with averages
FINAL_MODEL_DATA[, s2b_lag:= ifelse(is.na(s2b_lag), avg_Lags, s2b_lag)]
FINAL_MODEL_DATA[, mpn_lag:= ifelse(is.na(mpn_lag), avg_mpn_lag, mpn_lag)]
FINAL_MODEL_DATA[, customer_lag:= ifelse(is.na(customer_lag), avg_customer_lags, customer_lag)]

# removing 2017 out of training data
FINAL_MODEL_DATA <- FINAL_MODEL_DATA[snapshot_qtr > "2018 Q1"]
MODEL_DATA <- copy(FINAL_MODEL_DATA)


rm(BACKLOGS_RAW)
rm(BACKLOGS)
rm(REVENUE_RAW)
rm(BACKLOGS_RAW_CR)
rm(BACKLOGS_RAW_ORD_ENT)
rm(BACKLOG_26_FOR_13WEEKS_AGO)
rm(BACKLOG_26WEEKS_13WEEKS_AGO_ROLLING)
rm(BACKLOGS_CR)
rm(BACKLOGS_CR_CURR_QTR)
rm(BACKLOGS_CR_FQ)
rm(BACKLOGS_CR_NQ)
rm(BACKLOGS_RS)
rm(BACKLOGS_RS_CURR_QTR)
rm(BACKLOGS_RS_FQ)
rm(BACKLOGS_RS_SQ)
rm(BACKLOGS_RS_NQ)

rm(BACKLOG_FC_CAT)
rm(BACKLOGS_CR_SQ)
rm(CLEAN_UP)
rm(JERRY_PRICE_OPT)
rm(LEAD_TIME)
rm(mpn_lags)
rm(MPN_PARTID_SNAP)
rm(MPN_PRICE_OPT)
rm(MPN_PRICE_OPT_MODEL)
rm(MPN_WAFER_MAP)

rm(CATA_17_19_LIST)
rm(customer_lags)
rm(date_for_LT)
rm(DATE_QTR)
rm(Lags)
rm(OLD_NEW_MPN_MAP)
rm(PARTS)
rm(PARTS_AGE)
rm(PARTS_FAM_MAP)

rm(PROGRESS_FOR_13WEEKS_AGO_SNAP)
rm(REVENUE_13_13WEEKS_AGO_SNAP)
rm(REVENUE_DATA)
rm(REVENUE_DATA_FINAL)
rm(REVENUE_MODEL)
rm(REVENUE_MODEL_BACKUP)
rm(TEMP_REVENUE_DATA)
rm(WEEKS)
