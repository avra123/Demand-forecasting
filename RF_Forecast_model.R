library(rsample)
library(ranger)
library(caret)
library(splitstackshape)

source("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\R scripts\\Data_pre_processing.R")

############## More pre-processing
cutoff_date <- as.Date("2018-12-31")

# apply log tranformation to scale down the backlog $
MODEL_DATA[, backlog_rs := ifelse(backlog_rs==0, 0, log(backlog_rs))]
MODEL_DATA[, backlog_rs_fq := ifelse(backlog_rs_fq==0, 0, log(backlog_rs_fq))]
MODEL_DATA[, backlog_nq := ifelse(backlog_nq==0, 0, log(backlog_nq))]
MODEL_DATA[, backlog_26 := ifelse(backlog_26==0, 0, log(backlog_26))]
MODEL_DATA[, backlog_sq := ifelse(backlog_sq==0, 0, log(backlog_sq))]
MODEL_DATA[, backlog_fq := ifelse(backlog_fq==0, 0, log(backlog_fq))]
MODEL_DATA[, revenue_hist:= ifelse(revenue_hist==0, 0, log(revenue_hist))]
MODEL_DATA[, backlog_curr_qtr:= ifelse(backlog_curr_qtr==0, 0, log(backlog_curr_qtr))]

############# imputing missing records by family with mode of Lead time
mode_LT_family <- setkey(MODEL_DATA[!(is.na(published_lt)),
                                    .(freq= .N), by=.(family, published_lt)], family, freq)[J(unique(family)), mult="last"]

missing_LT_rows <- MODEL_DATA[is.na(published_lt)]
clean_data <- MODEL_DATA[!is.na(published_lt)]


setkey(mode_LT_family, family)
setkey(missing_LT_rows, family)

missing_LT_rows <- merge(missing_LT_rows, mode_LT_family, by="family")
missing_LT_rows[, published_lt.x:=NULL]
missing_LT_rows[, freq:=NULL]
missing_LT_rows[, published_lt := published_lt.y]
missing_LT_rows[, published_lt.y:=NULL]
MODEL_DATA <- rbind(clean_data, missing_LT_rows)

###############
train_data <- MODEL_DATA[snapshot_date <= cutoff_date]
test_data <- MODEL_DATA[snapshot_date > cutoff_date]
test_data <- test_data[!(family == "SUBSYS")]

train_data <- train_data[!(is.na(ship_to_book_ratio)| is.infinite(ship_to_book_ratio) | (ship_to_book_ratio <= 0))]
quantiles <- quantile(train_data$ship_to_book_ratio,
                      prob =seq(0, 1, by = 0.01),
                      na.rm = TRUE)

# dropping outliers
train_data <- train_data[ship_to_book_ratio>quantiles[10]]

############# New Feature (Ship to Book nature of customers)
s2b_avg_customers <- train_data[, .(avg_ship_to_book = mean(ship_to_book_ratio)), by=.(family, current_fc_customer)]

setkey(s2b_avg_customers, family, current_fc_customer)
setkey(train_data, family, current_fc_customer)
setkey(test_data, family, current_fc_customer)

s2b_avg_customers[, s2b_nature:= ifelse(is.na(avg_ship_to_book), "NEW",
                                        ifelse(avg_ship_to_book <= 0.8, "LOW", ifelse(avg_ship_to_book <=1.4, "AVG",
                                               ifelse(avg_ship_to_book <=2.0, "HIGH", "SUPER-HIGH"))))]

s2b_avg_customers[, avg_ship_to_book:=NULL]
train_data <- train_data[s2b_avg_customers]
test_data <- test_data[s2b_avg_customers]


###############################################################

#test_data[, backlog_spread:=ifelse(is.na(backlog_spread), 0, backlog_spread)]
#test_data[, table_backlog_ratio:=ifelse(is.na(table_backlog_ratio), 0, table_backlog_ratio)]
surprise_data <- test_data[(backlog_26==0 | is.na(backlog_26))] 
predict_data <- test_data[!(backlog_26==0 | is.na(backlog_26))]


###############################################################
# filtering needed_columns

needed_cols <- c("backlog_rs","backlog_rs_fq" ,"jerry_mpn_variation_bin", "product_age_bin", "backlog_26", "backlog_fq",
                 "segment", "published_lt", "backlog_nq", "ship_to_book_ratio", "backlog_sq" , "revenue_hist", "s2b_lag",
                 "backlog_spread", "fqtr", "table_backlog_ratio", "weeks_to_fcst_window", "customer_lag", "mpn_lag")

training_dataset <- subset(train_data, select = needed_cols)

needed_cols_test <- c("backlog_rs","backlog_rs_fq" ,"jerry_mpn_variation_bin", "product_age_bin", "backlog_26", 
                      "segment", "published_lt", "backlog_nq", "backlog_sq" , "backlog_fq", "revenue_hist", "s2b_lag",
                      "backlog_spread", "fqtr", "table_backlog_ratio", "weeks_to_fcst_window", "customer_lag", "mpn_lag")
#datasets_splitted <- stratified(training_dataset_final[(weeks_to_fcst_window == i)], group="family", 
#                                  size=0.7, bothSets=TRUE)
  
  
#train_data <- datasets_splitted[[1]]
#train_data[, weeks_to_fcst_window:=NULL]
  
#test_data <- datasets_splitted[[2]]
#test_data[, weeks_to_fcst_window:=NULL]
######################################################  

for(i in 0:11){
  
  training_data <- training_dataset[weeks_to_fcst_window == i]
  training_data[, weeks_to_fcst_window:=NULL]
  testing_data <- predict_data[weeks_to_fcst_window == i]
  
  rf_model <- ranger(
    formula = ship_to_book_ratio ~ . ,
    data = training_data,
    num.trees=500,
    mtry = 6,
    min.node.size = 20,
    max.depth = 6,
    importance='impurity'
  )
  
  print(rf_model$r.squared)
  print(rf_model$prediction.error)
  writeLines("\n")
  
  rf_model_predict <- predict(rf_model, testing_data[, needed_cols_test, with=FALSE])
  predictions <- rf_model_predict$predictions
  
  predictions <- setDT(as.data.frame(predictions))
  temp_file <- copy(testing_data)
  temp_file[, index:=(.I)]
  predictions[, index:=(.I)]
  
  setkey(predictions, index)
  setkey(temp_file, index)
  
  temp_file <- predictions[temp_file]
  temp_file[, expected_shipments:= predictions*exp(backlog_26)]
  
  if(exists("model_predictions")){
    model_predictions <- rbind(model_predictions, temp_file)
  } else{
    model_predictions <- copy(temp_file)
  }
}

# clubbing the 12 and 13 weeks together

training_data <- training_dataset[(weeks_to_fcst_window == 11)|(weeks_to_fcst_window == 13)]
training_data[, weeks_to_fcst_window:=NULL]
testing_data <- predict_data[(weeks_to_fcst_window == 12) | (weeks_to_fcst_window == 13)]

rf_model_last <- ranger(
  formula = ship_to_book_ratio ~ . ,
  data = training_data,
  num.trees=500,
  mtry = 6,
  importance='permutation'
  
)

print(rf_model$r.squared)
print(rf_model$prediction.error)
writeLines("\n")

rf_model_predict <- predict(rf_model_last, testing_data[, needed_cols_test, with=FALSE])
predictions <- rf_model_predict$predictions

predictions <- setDT(as.data.frame(predictions))
temp_file <- copy(testing_data)
temp_file[, index:=(.I)]
predictions[, index:=(.I)]

setkey(predictions, index)
setkey(temp_file, index)

temp_file <- predictions[temp_file]
temp_file[, expected_shipments:= predictions*exp(backlog_26)]
model_predictions <- rbind(model_predictions, temp_file)


#################
must_touch_list <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\Must Touch List.csv")
model_predictions <- merge(model_predictions, must_touch_list,
                           by.x=c("current_fc_customer", "mkt_part_num"), by.y=c("Fc Customer", "MPN"), all.x=TRUE)
model_predictions[, must_touch:= ifelse(is.na(`Must Touch`), "N", `Must Touch`)]

predict_data <- merge(predict_data, must_touch_list,
                            by.x=c("current_fc_customer", "mkt_part_num"), by.y=c("Fc Customer", "MPN"), all.x=TRUE)
predict_data[, must_touch:= ifelse(is.na(`Must Touch`), "N", `Must Touch`)]

predictions_from_backlog <- model_predictions[,.(predicted_backlog = sum(expected_shipments),
                                                 actual_backlog = sum(exp(backlog_26))),
                                                 by=.(family, snapshot_date, current_fc_customer, mkt_part_num, must_touch)]


known_revenue <- predict_data[, .(family, revenue_26, snapshot_date, current_fc_customer, mkt_part_num, must_touch)]

write.csv(predictions_from_backlog, "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\FC_FCST2Q_REVENUE_DOLLARS_CR_BACKLOG_RF_PRED_V4_0919.csv")
write.csv(known_revenue, "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\FC_FCST2Q_REVENUE_DOLLARS_CR_BACKLOG_RF_ACTUAL_V4_0919.csv")

######################

