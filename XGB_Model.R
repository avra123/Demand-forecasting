source("C:\\Users\\avra\\OneDrive - Cypress Semiconductor\\R scripts\\Data_pre_processing.R")

cutoff_date <- as.Date("2018-12-31")
# 
# # apply log tranformation to scale down the backlog $
MODEL_DATA[, backlog_rs := ifelse(backlog_rs==0, 0, log(backlog_rs))]
MODEL_DATA[, backlog_rs_fq := ifelse(backlog_rs_fq==0, 0, log(backlog_rs_fq)+1)]
MODEL_DATA[, backlog_nq := ifelse(backlog_nq==0, 0, log(backlog_nq))]
MODEL_DATA[, backlog_26 := ifelse(backlog_26==0, 0, log(backlog_26))]
MODEL_DATA[, backlog_sq := ifelse(backlog_sq==0, 0, log(backlog_sq))]
MODEL_DATA[, backlog_fq := ifelse(backlog_fq==0, 0, log(backlog_fq))]
MODEL_DATA[, revenue_hist:= ifelse(revenue_hist==0, 0, log(revenue_hist))]
MODEL_DATA[, backlog_curr_qtr:= ifelse(backlog_curr_qtr==0, 0, log(backlog_curr_qtr))]


# # apply log tranformation to scale down the backlog $
# MODEL_DATA[, backlog_rs := ifelse(backlog_rs==0, 0, sqrt(backlog_rs))]
# MODEL_DATA[, backlog_rs_fq := ifelse(backlog_rs_fq==0, 0, sqrt(backlog_rs_fq))]
# MODEL_DATA[, backlog_nq := ifelse(backlog_nq==0, 0, sqrt(backlog_nq))]
# MODEL_DATA[, backlog_26 := ifelse(backlog_26==0, 0, sqrt(backlog_26))]
# MODEL_DATA[, backlog_sq := ifelse(backlog_sq==0, 0, sqrt(backlog_sq))]
# MODEL_DATA[, backlog_fq := ifelse(backlog_fq==0, 0, sqrt(backlog_fq))]
# MODEL_DATA[, revenue_hist:= ifelse(revenue_hist==0, 0, sqrt(revenue_hist))]
# MODEL_DATA[, backlog_curr_qtr:= ifelse(backlog_curr_qtr==0, 0, sqrt(backlog_curr_qtr))]

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

# train_data <- train_data[!(is.na(transformed_ship_to_book)| is.infinite(transformed_ship_to_book))]
# quantiles <- quantile(train_data$transformed_ship_to_book,
#                       prob =seq(0, 1, by = 0.01),
#                       na.rm = TRUE)


train_data <- train_data[ship_to_book_ratio>quantiles[10]]
#train_data <- train_data[transformed_ship_to_book > quantiles[1]]
###############################################################

#test_data[, backlog_spread:=ifelse(is.na(backlog_spread), 0, backlog_spread)]
#test_data[, table_backlog_ratio:=ifelse(is.na(table_backlog_ratio), 0, table_backlog_ratio)]
surprise_data <- test_data[(backlog_26==0 | is.na(backlog_26))] 
predict_data <- test_data[!(backlog_26==0 | is.na(backlog_26))]


###############################################################
# filtering needed_columns

needed_cols <- c("backlog_rs","backlog_rs_fq" ,"jerry_mpn_variation_bin", "product_age_bin", "backlog_26", "backlog_fq",
                 "segment", "published_lt", "backlog_nq", "ship_to_book_ratio", "backlog_sq" , "s2b_lag", "revenue_hist",
                 "backlog_spread", "fqtr", "table_backlog_ratio", "weeks_to_fcst_window", "mpn_lag", "customer_lag")

training_dataset <- subset(train_data, select = needed_cols)

needed_cols_test <- c("backlog_rs","backlog_rs_fq" ,"jerry_mpn_variation_bin", "product_age_bin", "backlog_26", 
                      "segment", "published_lt", "backlog_nq", "backlog_sq" , "backlog_fq", "s2b_lag", "revenue_hist",
                      "backlog_spread", "fqtr", "table_backlog_ratio", "weeks_to_fcst_window", "mpn_lag", "customer_lag")

ship_to_book <- c("ship_to_book_ratio")


library(h2o)
# make sure the port # works for whoever is going to log into the R-SVR
# defult is 54321 - also try h2o.init()
############## More pre-processing
h2o.init(ip='localhost', nthreads=-1, min_mem_size='10G',
         max_mem_size='20G', enable_assertions = FALSE) #check this later : issue with port


model_data_h2o <- as.h2o(training_dataset)
split_h2o_cv <- h2o.splitFrame(model_data_h2o, c(0.2), seed = 123 )
train_cv_h2o <- h2o.assign(split_h2o_cv[[2]], "train" ) # 80%
valid_cv_h2o <- h2o.assign(split_h2o_cv[[1]], "test" ) # 20%

gbm_params1 <- list(learn_rate = c(0.1, 0.05),
                    max_depth = c(17, 18), 
                    sample_rate = c(0.8, 1.0), 
                    col_sample_rate = c(0.9, 1.0)
)

search_criteria <- list(strategy = "RandomDiscrete", stopping_metric = "RMSE",
                        stopping_tolerance = 0.001, stopping_rounds = 5)

gbm_grid1 <- h2o.grid("gbm", x = needed_cols_test, y=ship_to_book,
                      grid_id = "gbm_grid1", 
                      training_frame = train_cv_h2o, 
                      validation_frame = valid_cv_h2o,
                      ntrees = 2000, 
                      seed = 1,
                      hyper_params = gbm_params1,
                      search_criteria = search_criteria)

all_models <- h2o.getGrid(grid_id = "gbm_grid1",
                          sort_by = "RMSE")
#best_gbm1_model <- h2o.getModel(all_models@model_ids[[1]])

test_data_h2o <- as.h2o(test_data[, needed_cols_test, with=FALSE])
h2o.varimp_plot(h2o.getModel(all_models@model_ids[[1]]), 20)
h2o.partialPlot(object = h2o.getModel(all_models@model_ids[[1]]), data = valid_cv_h2o, cols = c("weeks_to_fcst_window"))


predictions_best_model <- h2o.predict(object=gbm_model, newdata=test_data_h2o)
predictions <- setDT(as.data.frame(predictions_best_model))
model_predictions <- predict_data[snapshot_date>cutoff_date]
model_predictions[,index:=(.I)]
predictions[,index:=(.I)]

setkey(model_predictions,index)
setkey(predictions,index)

model_predictions <- predictions[model_predictions]
model_predictions[,EXP_SHIPMENTS:=predict*exp(backlog_26)]

detach("package:h2o",unload = TRUE)
#################
must_touch_list <- fread("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\Must Touch List.csv")
model_predictions <- merge(model_predictions, must_touch_list,
                           by.x=c("current_fc_customer", "mkt_part_num"), by.y=c("Fc Customer", "MPN"), all.x=TRUE)
model_predictions[, must_touch:= ifelse(is.na(`Must Touch`), "N", `Must Touch`)]

predict_data <- merge(predict_data, must_touch_list,
                      by.x=c("current_fc_customer", "mkt_part_num"), by.y=c("Fc Customer", "MPN"), all.x=TRUE)
predict_data[, must_touch:= ifelse(is.na(`Must Touch`), "N", `Must Touch`)]

predictions_from_backlog <- model_predictions[,.(predicted_backlog = sum(EXP_SHIPMENTS),
                                                 actual_backlog = sum(exp(backlog_26))),
                                              by=.(family, snapshot_date, current_fc_customer, mkt_part_num, must_touch)]


known_revenue <- predict_data[, .(family, revenue_26, snapshot_date, current_fc_customer, mkt_part_num, must_touch)]

write.csv(predictions_from_backlog, "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\FC_FCST2Q_REVENUE_DOLLARS_CR_BACKLOG_XGB_TUNED_PRED_V5_1008.csv")
write.csv(known_revenue, "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\FC_FCST2Q_REVENUE_DOLLARS_CR_BACKLOG_XGB_TUNED_ACTUAL_V5_1008.csv")





for (i in 0:13){
  
  model_data_h2o <- as.h2o(training_dataset[weeks_to_fcst_window==i])
  split_h2o_cv <- h2o.splitFrame(model_data_h2o, c(0.2), seed = 123 )
  train_cv_h2o <- h2o.assign(split_h2o_cv[[2]], "train" ) # 80%
  valid_cv_h2o <- h2o.assign(split_h2o_cv[[1]], "test" ) # 20%
  
  source("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\MODEL_TUNING.R")
  
  # # This can be removed - it was in here before we started the hyper-tuning steps above 
  #  gbm_model <- h2o.gbm(y = Y_VAR,
  #                       x = PREDS,
  #                       training_frame = train_cv_h2o,
  #                       validation_frame = valid_cv_h2o,
  #                       ntrees = 10000,
  #                       learn_rate=0.01,
  #                       sample_rate = 0.8,
  #                       min_rows = 5,
  #                       col_sample_rate = 0.8,
  #                       score_tree_interval = 10,
  #                       stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MAE",
  #                       nfolds = 4,
  #                       seed = 0xDECAF)
  
  gbm_model <- gbm_fine_tuned ## run Model_tuning file
  
  #gbm_var_imp <- h2o.varimp(gbm_model)
  
  #gbm_var_imp
  h2o.varimp_plot(gbm_model,20)
  
  h2o.performance(gbm_model,valid_cv_h2o)
  
  # we could access model performance metrics and log them for analysis
  
  # In this version we force smart names into the output model files
  h2o.saveModel(object=gbm_model, path=getwd(), force=TRUE)
  ##DOLLARS, QUANT MODELS
  name <- file.path(getwd(),paste0("SHIPPMENTS_",MODEL_TYPE,"_2QFCST_XB_MODEL_",i,"WEEKS_TOGO",Sys.Date(),"_VXR"))
  file.rename(file.path(getwd(), gbm_model@model_id), name)
  
  name
  
  # h2o.partialPlot(object = gbm_model, data = valid_cv_h2o, cols = c("WEEKS_TO_FCST_WINDOW"))
  
}
