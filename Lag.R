# Ship to Book Lab calculation
Q417_Revenue <- REVENUE_MODEL_BACKUP[(as.yearqtr(CR_DATE) == as.yearqtr(WEEK_DATES))
                               , .(Q4_REVENUE = sum(QUANTITY, na.rm=TRUE)), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, WEEK_DATES)]
Q417_Revenue <- Q417_Revenue[WEEK_DATES <= "2017-12-31"]

# Q417 CR 
Q417_Backlog <- BACKLOGS_RAW_CR[(as.yearqtr(CR_DATE) == as.yearqtr(SNAPSHOT_DATE)),
                                .(CR_BACKLOG_FQ = sum(QUANTITY, na.rm=TRUE)),
                                by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, SNAPSHOT_DATE)]
Q417_Backlog <- Q417_Backlog[SNAPSHOT_DATE <= "2017-12-31"]


final_Q417 <- merge(Q417_Backlog, Q417_Revenue, by.y=c("MKT_PART_NUM", "CURRENT_FC_CUSTOMER", "WEEK_DATES"), 
                    by.x=c("MKT_PART_NUM", "CURRENT_FC_CUSTOMER", "SNAPSHOT_DATE"), all.x=TRUE)

lags_Q417 <- final_Q417[SNAPSHOT_DATE == '2017-10-08']
lags_Q417[, Q4_REVENUE:= ifelse(is.na(Q4_REVENUE), 0, Q4_REVENUE)]
lags_Q417[, S2B_RATIO := Q4_REVENUE/CR_BACKLOG_FQ]
lags_Q417[, SNAPSHOT_QTR:= as.yearqtr(SNAPSHOT_DATE)]

names(lags_Q417) <- c("MKT_PART_NUM", "CURRENT_FC_CUSTOMER", "SNAPSHOT_DATE", "BACKLOG_CR_FQ", "REVENUE", "S2B_RATIO", "SNAPSHOT_QTR")
lags_Q417[, SNAPSHOT_QTR:= SNAPSHOT_QTR - 0.25]


#### For all other quarters
backlog_lag <- BACKLOGS_RAW_CR[(as.yearqtr(CR_DATE) > as.yearqtr(SNAPSHOT_DATE)) &
                                 (as.yearqtr(CR_DATE) <= as.yearqtr(SNAPSHOT_DATE) + 0.25),
                                .(BACKLOG_CR_FQ = sum(QUANTITY, na.rm=TRUE)),
                               by=.(SNAPSHOT_DATE, CURRENT_FC_CUSTOMER, MKT_PART_NUM)]

backlog_lag[,WEEKS_TO_FCST_WINDOW:=
             as.factor(round(as.integer(
               difftime(as.yearmon(as.yearqtr(SNAPSHOT_DATE)+0.25),SNAPSHOT_DATE,units="weeks")
             )))]

backlog_lag <- backlog_lag[WEEKS_TO_FCST_WINDOW == 1]


revenue_lag <- REVENUE_MODEL_BACKUP[((as.yearqtr(CR_DATE) > as.yearqtr(WEEK_DATES)) & (as.yearqtr(CR_DATE) <= as.yearqtr(WEEK_DATES) + 0.25))
                              ,.(REVENUE = sum(QUANTITY, na.rm=TRUE)), by=.(CURRENT_FC_CUSTOMER, MKT_PART_NUM, WEEK_DATES)]

names(revenue_lag)[3] <- "SNAPSHOT_DATE"

final_lag <- merge(backlog_lag, revenue_lag, by.x=c("SNAPSHOT_DATE", "MKT_PART_NUM", "CURRENT_FC_CUSTOMER"),
                   by.y=c("SNAPSHOT_DATE", "MKT_PART_NUM", "CURRENT_FC_CUSTOMER"), all.x=TRUE)

final_lag[, REVENUE:= ifelse(is.na(REVENUE), 0, REVENUE)]
final_lag[, S2B_RATIO:= REVENUE/BACKLOG_CR_FQ]
final_lag[, WEEKS_TO_FCST_WINDOW:=NULL]
final_lag[, SNAPSHOT_QTR:= as.yearqtr(SNAPSHOT_DATE) + 0.25]

final_lag <- rbind(final_lag, lags_Q417)
final_lag <- final_lag[S2B_RATIO > 0 & S2B_RATIO <=15]

customer_lags <- final_lag[, .(customer_lag = mean(S2B_RATIO)), by=.(SNAPSHOT_QTR, CURRENT_FC_CUSTOMER)]
mpn_lags <- final_lag[, .(mpn_lag=mean(S2B_RATIO)), by=.(SNAPSHOT_QTR, MKT_PART_NUM)]

# avg lags
avg_lag <- final_lag[, mean(S2B_RATIO)]
avg_customer_lag <- customer_lags[, mean(customer_lag)]
avg_mpn_lag <- mpn_lags[, mean(mpn_lag)]


final_lag[, SNAPSHOT_QTR:= SNAPSHOT_QTR + 0.25]
names(final_lag) <- tolower(names(final_lag))
names(customer_lags) <- tolower(names(customer_lags))
names(mpn_lags) <- tolower(names(mpn_lags))

#dropping un-necessary cols
final_lag[, snapshot_date:=NULL]
final_lag[, backlog_cr_fq:=NULL]
final_lag[, revenue:=NULL]
final_lag[, s2b_lag := s2b_ratio]
final_lag[, s2b_ratio:=NULL]

# merging everything
FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, final_lag, by=c('snapshot_qtr', 'current_fc_customer', 'mkt_part_num'), all.x=TRUE)
FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, customer_lags, by=c('snapshot_qtr', 'current_fc_customer'), all.x=TRUE)
FINAL_MODEL_DATA <- merge(FINAL_MODEL_DATA, mpn_lags, by=c('snapshot_qtr', 'mkt_part_num'), all.x=TRUE)

# # impute missing lags with averages
FINAL_MODEL_DATA[, s2b_lag:= ifelse(is.na(s2b_lag), avg_lag, s2b_lag)]
FINAL_MODEL_DATA[, mpn_lag:= ifelse(is.na(mpn_lag), avg_mpn_lag, mpn_lag)]
FINAL_MODEL_DATA[, customer_lag:= ifelse(is.na(customer_lag), avg_customer_lag, customer_lag)]

# # removing 2017 out of training data
FINAL_MODEL_DATA <- FINAL_MODEL_DATA[snapshot_qtr > "2017 Q4"]
MODEL_DATA <- copy(FINAL_MODEL_DATA)


####################################################################


