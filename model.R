library(caret)
library(modelr)
library(randomForest)


ctrl <- trainControl(method = "cv",
                    number = 5,
                    sampling = "down",
                    summaryFunction = twoClassSummary,
                    classProbs = T)

grid <- expand.grid(mtry = c(6,10,14))

fit_rf <- train(serious_dlqin2yrs ~ .,
                data = train_df %>% select(-x1) %>% mutate(serious_dlqin2yrs = factor(serious_dlqin2yrs, levels = c(1,0),labels =c("Delinquency","No Delinquency"))),
                method = "rf",
                importance = T,
                na.action = na.omit,
                tuneGrid = grid,
                trControl = ctrl,
                verbose = T)

predictions <- predict(fit_rf,test_df,type="prob")


test_df %>% 
  bind_cols(predictions) %>% 
  select(x1,Churn) %>% 
  rename(Id = x1,
         Probability = Delinquency) %>% 
  write_csv("rf_1.csv")


