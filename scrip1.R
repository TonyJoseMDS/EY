library(readxl)
library(dplyr)
library(caret)
library(ranger)
library(randomForest)

train_data <- data.frame(read_xlsx("D:/EY/data_train.xlsx"))
test_data <- data.frame(read_xlsx("D:/EY/data_test_new.xlsx"))

test_data_2 <- test_data %>%
  group_by(hash) %>%
  summarise(n=sum(City.center))

test_data_1 <- left_join(test_data,test_data_2, by="hash")

train_data_2 <- train_data %>%
  group_by(hash) %>%
  summarise(n=sum(City.center))

train_data_1 <- left_join(train_data,train_data_2, by="hash")

# train1 <- sample_frac(train_data_1,0.7)
# sid <- as.numeric(rownames(train1))
# test1 <- train_data_1[-sid,]

trainx <- train_data_1[,c(4,5,9,10,13,14,15)]
testx <-  test_data_1[,c(4,5,9,10,13,14,15)]

# trainx_no_nzv_pca <- preProcess(select(trainx, - City.center), 
#                                  method = c("center", "scale", "YeoJohnson", "nzv", "pca"))
# trainx_no_nzv_pca
# 
# set.seed(20)
# train_index <- createDataPartition(trainx$City.center,
#                                    p = 0.75,
#                                    list = FALSE)
# 
# training <- trainx[train_index,]
# testing <- trainx[-train_index,]





fit_control <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

tg_grid <- expand.grid(mtry=2:5, splitrule = c("gini", "extratrees"),min.node.size = c(1,3,5))

# run a random forest model
set.seed(2)
rf_fit <- train(as.factor(City.center) ~ ., 
                data = trainx, 
                method = "ranger",
                trControl = fit_control,
                num.trees = 100,
                tuneGrid = tg_grid)
rf_fit

# predict the outcome on a test set
rf_fit_pred_x <- predict(rf_fit, testx)
# compare predicted outcome and true outcome
confusionMatrix(rf_fit_pred, as.factor(testing$City.center))
rf_fit_pred_x
df <- data.frame(rf_fit_pred_x)
write.csv(df,"D:/EY/output.csv")
  
