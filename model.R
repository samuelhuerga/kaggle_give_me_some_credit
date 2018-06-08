# library(tidyverse)
library(caret)
library(modelr)
library(randomForest)
library(pROC)
library(doParallel)

# 01 - Load data ---------------------------------
#_________________________________________________
source("load_data.R")


# 02 - Random forest model -----------------------
#_________________________________________________
modelLookup("rf")

ctrl <- trainControl(method = "cv",
                    number = 5,
                    # sampling = "down",
                    summaryFunction = twoClassSummary,
                    classProbs = T,
                    verboseIter = T)

# Set options to tune grid:
grid <- expand.grid(mtry = c(4,6,8))

gc()
# n_cores <- detectCores()
# cl <- makePSOCKcluster(n_cores - 1)
cl <- makeCluster(8)
registerDoParallel(cl)

train_df %>% nearZeroVar(names = T)
fit_rf <- train(serious_dlqin2yrs ~ .,
                data = train_df %>% select(-x1) %>% mutate(serious_dlqin2yrs = factor(serious_dlqin2yrs, levels = c(1,0),labels =c("Delinquency","No_Delinquency"))),
                method = "rf",
                importance = T,
                # na.action = na.omit,
                tuneGrid = grid,
                trControl = ctrl,
                metric = "ROC",
                verbose = T)

# stopCluster(cl)
predictions <- predict(fit_rf,test_df,type="prob")


test_df %>% 
  bind_cols(predictions) %>% 
  select(x1,Churn) %>% 
  rename(Id = x1,
         Probability = Delinquency) %>% 
  write_csv("rf_2.csv")




# 03 - xgBoost model -----------------------
#_________________________________________________
modelLookup("xgbTree")

ctrl_xgb <-  trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

grid_xgb <-  expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  subsample = 1,
  colsample_bytree = 1,
  min_child_weight = 1
  
)

xgb_train_1 <-  train(serious_dlqin2yrs ~ .,
                    data = train_df %>% select(-x1) %>% mutate(serious_dlqin2yrs = factor(serious_dlqin2yrs, levels = c(1,0),labels =c("Delinquency","No_Delinquency"))),
                    trControl = ctrl_xgb,
                    tuneGrid = grid_xgb,
                    method = "xgbTree",
                    metric = "AUC",
                    verbose = T
)

predictions <- predict(xgb_train_1,test_df,type="prob")


test_df %>% 
  bind_cols(predictions) %>% 
  select(x1,Delinquency) %>% 
  rename(Id = x1,
         Probability = Delinquency) %>% 
  mutate(Id = as.integer(Id)) %>% 
  write_csv("xgb_1.csv")


