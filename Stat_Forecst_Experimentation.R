#############################################
# Experimentation



# Tune one parameter at a time to gauge the grid that we need to build for 
# tuning all the hyper params

# tuning node_size
for(i in 0:13){
  training_data <- training_dataset[weeks_to_fcst_window == i]
  testing_data <- predict_data[weeks_to_fcst_window == i]
  print(i)
  print(nrow(training_data))
  print(nrow(testing_data))
  writeLines("\n")
}



training_data <- training_dataset[(weeks_to_fcst_window == 11)|(weeks_to_fcst_window == 13)]
training_data[, weeks_to_fcst_window:=NULL]
testing_data <- predict_data[(weeks_to_fcst_window == 12) | (weeks_to_fcst_window == 13)]

for(i in 1:nrow(hyper_grid)){
  
  model <- ranger(
    formula = ship_to_book_ratio ~ .,
    data = training_data,
    num.trees= 500,
    mtry = 5,
    min.node.size = hyper_grid$node_size[i],
    max.depth = hyper_grid$max_depth[i],
    sample.fraction = 0.7,
    seed = 123
  )
  
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
  preds <- predict(model, testing_data)
  predictions_rev <- preds$predictions*exp(testing_data$backlog_26)
  hyper_grid$Test_RMSE[i] <- RMSE(predictions_rev, testing_data[((weeks_to_fcst_window == 12) | (weeks_to_fcst_window == 13)), .(revenue_26)])
  
}

###########################################

# XGBoost experimentation
xgb_needed_cols <- c("backlog_cr","backlog_cr_fq" ,"jerry_mpn_variation_bin", "product_age_bin", "backlog_26", 
                     "family", "published_leadtime_wks", "backlog_nq", "backlog_sq" , "backlog_fq", "revenue_hist",
                     "backlog_spread", "fqtr", "table_backlog_ratio")


library(xgboost)

training_data <- training_dataset[weeks_to_fcst_window == 11]
training_data[, weeks_to_fcst_window:=NULL]
testing_data <- predict_data[weeks_to_fcst_window == 11]

new_tr <- model.matrix(~.+0, data = training_data[, xgb_needed_cols, with=F])
new_tst <- model.matrix(~.+0, data = testing_data[, xgb_needed_cols, with=F])

dtrain <- xgb.DMatrix(new_tr, label = training_data$ship_to_book_ratio)
dtest <- xgb.DMatrix(new_tst, label = testing_data$ship_to_book_ratio)

params <- list(booster='gbtree', objective="reg:squarederror", gamma=0.1, eta=.2,
               max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds=1000, watchlist = list(train = dtrain),
                       print_every_n = 10, early_stopping_rounds = 5, maximize = F, eval_metric="rmse")


##############################################

#########################
# XGBoost experimentation
xgb_needed_cols <- c("backlog_cr","backlog_cr_fq" ,"jerry_mpn_variation_bin", "product_age_bin", "backlog_26", 
                     "family", "published_leadtime_wks", "backlog_nq", "backlog_sq" , "backlog_fq", "revenue_hist",
                     "backlog_spread", "fqtr", "table_backlog_ratio")

path <- "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\Models"
setwd(path)
library(h2o)
# make sure the port # works for whoever is going to log into the R-SVR
# defult is 54321 - also try h2o.init()
h2o.init(ip='localhost', nthreads=-1, min_mem_size='10G',
         max_mem_size='20G', enable_assertions = FALSE)  #check this later : issue with port


for (i in 0:11){
  
  model_data_h2o <- as.h2o(training_dataset[weeks_to_fcst_window==i])
  split_h2o_cv <- h2o.splitFrame(model_data_h2o, c(0.2), seed = 123 )
  train_cv_h2o <- h2o.assign(split_h2o_cv[[2]], "train" ) # 80%
  valid_cv_h2o <- h2o.assign(split_h2o_cv[[1]], "test" ) # 20%
  
  source("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\MODEL_TUNING.R")
  
  gbm_model <- gbm_fine_tuned ## run Model_tuning file
  gbm_var_imp <- h2o.varimp(gbm_model)
  
  gbm_var_imp
  h2o.varimp_plot(gbm_model,20)
  
  # we could access model performance metrics and log them for analysis
  
  # In this version we force smart names into the output model files
  h2o.saveModel(object=gbm_model, path=getwd(), force=TRUE)
  ##DOLLARS, QUANT MODELS
  name <- file.path(getwd(),paste0("SHIPPMENTS_",MODEL_TYPE,"_2QFCST_MODEL_",i,"WEEKS_TOGO",Sys.Date(),"_MODEL"))
  file.rename(file.path(getwd(), gbm_model@model_id), name)
  
  name
  
  # h2o.partialPlot(object = gbm_model, data = valid_cv_h2o, cols = c("WEEKS_TO_FCST_WINDOW"))
  
}


# for weeks 12 and 13

model_data_h2o <- as.h2o(training_dataset[(weeks_to_fcst_window==12) | (weeks_to_fcst_window==13)])
split_h2o_cv <- h2o.splitFrame(model_data_h2o, c(0.2), seed = 123 )
train_cv_h2o <- h2o.assign(split_h2o_cv[[2]], "train" ) # 80%
valid_cv_h2o <- h2o.assign(split_h2o_cv[[1]], "test" ) # 20%

source("C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\MODEL_TUNING.R")

gbm_model <- gbm_fine_tuned ## run Model_tuning file
gbm_var_imp <- h2o.varimp(gbm_model)

gbm_var_imp
h2o.varimp_plot(gbm_model,20)

# we could access model performance metrics and log them for analysis

# In this version we force smart names into the output model files
h2o.saveModel(object=gbm_model, path=getwd(), force=TRUE)
##DOLLARS, QUANT MODELS
name <- file.path(getwd(),paste0("SHIPPMENTS_",MODEL_TYPE,"_2QFCST_MODEL_",'12',"WEEKS_TOGO",Sys.Date(),"_MODEL"))
file.rename(file.path(getwd(), gbm_model@model_id), name)

name

detach("package:h2o",unload = TRUE)
# h2o.partialPlot(object = gbm_model, data = valid_cv_h2o, cols = c("WEEKS_TO_FCST_WINDOW"))
###################################################

#predictions with tuned xgboost

testing_dataset <- subset(predict_data, select = needed_cols_test)

path <- "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\Models"
setwd(path)
library(h2o)
h2o.init(ip='localhost', nthreads=-1, min_mem_size='10G', max_mem_size='20G',
         enable_assertions = FALSE) #check this later : issue with port

for(i in 0:11){
  
  model_predict_data_h2o <- as.h2o(testing_dataset[weeks_to_fcst_window==i])
  
  model <- h2o.loadModel(paste0("SHIPPMENTS_",MODEL_TYPE,"_2QFCST_MODEL_",i,"WEEKS_TOGO2019-09-23","_MODEL"))
  
  predictions <- h2o.predict(object =model, newdata = model_predict_data_h2o)
  
  predictions <- setDT(as.data.frame(predictions))
  
  model_predictions_temp <- predict_data[snapshot_date > cutoff_date & weeks_to_fcst_window==i]
  
  model_predictions_temp[,index:=(.I)]
  
  predictions[,index:=(.I)]
  
  setkey(model_predictions_temp,index)
  
  setkey(predictions,index)
  
  model_predictions_temp <- predictions[model_predictions_temp]
  model_predictions_temp[,exp_shipments:=predict*exp(backlog_26)]
  
  if(exists("model_predictions")){
    model_predictions <- rbind(model_predictions, model_predictions_temp)
  } else {
    model_predictions <- copy(model_predictions_temp)
  }
  
}


model_predict_data_h2o <- as.h2o(testing_dataset[(weeks_to_fcst_window==12)|(weeks_to_fcst_window==13)])
model <- h2o.loadModel(paste0("SHIPPMENTS_",MODEL_TYPE,"_2QFCST_MODEL_",'12',"WEEKS_TOGO2019-09-23","_MODEL"))
predictions <- h2o.predict(object =model, newdata = model_predict_data_h2o)
predictions <- setDT(as.data.frame(predictions))
model_predictions_temp <- predict_data[(snapshot_date > cutoff_date) & ((weeks_to_fcst_window==12)|(weeks_to_fcst_window==13))]
model_predictions_temp[,index:=(.I)]
predictions[,index:=(.I)]
setkey(model_predictions_temp,index)
setkey(predictions,index)
model_predictions_temp <- predictions[model_predictions_temp]
model_predictions_temp[,exp_shipments:=predict*exp(backlog_26)]
model_predictions <- rbind(model_predictions, model_predictions_temp)



detach("package:h2o",unload = TRUE)

#################
predictions_from_backlog <- model_predictions[,.(predicted_backlog = sum(exp_shipments),
                                                 actual_backlog = sum(exp(backlog_26))),
                                              by=.(family, snapshot_date, current_fc_customer, mkt_part_num)]

known_revenue <- predict_data[, .(family, revenue_26, snapshot_date, current_fc_customer, mkt_part_num)]

write.csv(predictions_from_backlog, "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\FC_FCST2Q_REVENUE_DOLLARS_BACKLOG_XGB_PRED_V1_0919.csv")
write.csv(known_revenue, "C:\\Users\\avra\\Downloads\\data files for R\\Forecasting GSGR\\Data files\\New folder\\FC_FCST2Q_REVENUE_DOLLARS_BACKLOG_XGB_ACTUAL_V1_0919.csv")
