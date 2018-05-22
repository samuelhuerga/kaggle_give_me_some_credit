library(caret)


ctrl <- trainControl(method = "cv",
                    number = 5,
                    sampling = "down")

fit_rf <- train(seriousdlqin2yrs ~ .,
                data = train_df %>% select(-x1) %>% mutate(seriousdlqin2yrs = factor(seriousdlqin2yrs, levels = c(1,0),labels =c("Churn","No Churn"))),
                method = "rf",
                importance = T,
                na.action = na.omit,
                trControl = ctrl)

fit_rf

