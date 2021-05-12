df = read.csv('clean19.csv',stringsAsFactors = TRUE)
df$X = NULL
df$number_of_reviews = NULL
df$reviews_per_month = NULL
df$availability_365 = NULL
df$log_reviews_per_month = NULL
df$latitude = NULL
df$longitude = NULL
Manhattan <- subset(df,neighbourhood_group == "Manhattan")
Manhattan$neighbourhood_group = NULL
Manhattan = Manhattan[Manhattan$price<500,]

Brooklyn <- subset(df,neighbourhood_group == "Brooklyn")
Brooklyn$neighbourhood_group = NULL
Brooklyn = Brooklyn[Brooklyn$price<500,]

levels(df$neighbourhood_group) <- list(Manhattan = c('Manhattan'), Brooklyn =  c("Brooklyn"), Q_BX_SI = c("Queens","Bronx","Staten Island"))
Q_BX_SI <- subset(df,neighbourhood_group == "Q_BX_SI")
Q_BX_SI$neighbourhood_group = NULL
Q_BX_SI = Q_BX_SI[Q_BX_SI$price<500,]


library(caret)
library(class)
library(ROCR)
library(plyr)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)
library(reshape2) 
library(ISLR) 
set.seed(200)


# KNN Regression M
mtest = Manhattan
price_outcome <- mtest %>% select(price)
mtest <- mtest %>% select(-price)
mtest[, sapply(mtest, is.numeric)] <- scale(mtest[, sapply(mtest, is.numeric)])
neighbourhood <- as.data.frame(dummy.code(mtest$neighbourhood))
room_type <- as.data.frame(dummy.code(mtest$room_type))
mtest <- cbind(mtest, neighbourhood, room_type)
mtest <- mtest %>% select(-one_of(c('neighbourhood', 'room_type')))

train <- sample(nrow(mtest), nrow(mtest)*.8)
mtest_train <- mtest[train, ]
mtest_test <- mtest[-train, ]
price_train <- price_outcome[train, ]
price_test <- price_outcome[-train, ]

accuracy = function(res) {
  mean(abs(price_test - res$pred))
}

k_to_try = 10:50
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  pred = knn.reg(mtest_train, mtest_test, price_train, k = k_to_try[i])
  acc_k[i] = accuracy(pred)
}
plot(k_to_try, acc_k, type = "b", col = "dodgerblue", 
     xlab = "k, number of neighbors", ylab = "regression accuracy",
     main = "Accuracy vs Neighbors")

res <- knn.reg(mtest_train, mtest_test, price_train, k=20)
plot(price_test, res$pred, xlab="y", ylab=expression(hat(y)))
#MSE
mmse = mean((price_test - res$pred) ^ 2)
mmse #4930.258
#RMSE
mrmse = sqrt(mean((price_test - res$pred) ^ 2))
mrmse #70.21579
#MAE
mmae = mean(abs(price_test - res$pred))
mmae #48.44883



# KNN Regression O
otest = Q_BX_SI
price_outcome <- otest %>% select(price)
otest <- otest %>% select(-price)
otest[, sapply(otest, is.numeric)] <- scale(otest[, sapply(otest, is.numeric)])
neighbourhood <- as.data.frame(dummy.code(otest$neighbourhood))
room_type <- as.data.frame(dummy.code(otest$room_type))
otest <- cbind(otest, neighbourhood, room_type)
otest <- otest %>% select(-one_of(c('neighbourhood', 'room_type')))

train <- sample(nrow(otest), nrow(otest)*.8)
otest_train <- otest[train, ]
otest_test <- otest[-train, ]
price_train <- price_outcome[train, ]
price_test <- price_outcome[-train, ]

k_to_try = 40:70
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  pred = knn.reg(otest_train, otest_test, price_train, k = k_to_try[i])
  acc_k[i] = accuracy(pred)
}
plot(k_to_try, acc_k, type = "b", col = "dodgerblue", 
     xlab = "k, number of neighbors", ylab = "regression accuracy",
     main = "Accuracy vs Neighbors")

res <- knn.reg(otest_train, otest_test, price_train, k=55)
plot(price_test, res$pred, xlab="y", ylab=expression(hat(y)))
#MSE
omse = mean((price_test - res$pred) ^ 2)
omse #2401.696
#RMSE
ormse = sqrt(mean((price_test - res$pred) ^ 2))
ormse #49.0071
#MAE
omae = mean(abs(price_test - res$pred))
omae #31.27857


# KNN Regression B
btest = Brooklyn
price_outcome <- btest %>% select(price)
btest <- btest %>% select(-price)
btest[, sapply(btest, is.numeric)] <- scale(btest[, sapply(btest, is.numeric)])
neighbourhood <- as.data.frame(dummy.code(btest$neighbourhood))
room_type <- as.data.frame(dummy.code(btest$room_type))
btest <- cbind(btest, neighbourhood, room_type)
btest <- btest %>% select(-one_of(c('neighbourhood', 'room_type')))

train <- sample(nrow(btest), nrow(btest)*.8)
btest_train <- btest[train, ]
btest_test <- btest[-train, ]
price_train <- price_outcome[train, ]
price_test <- price_outcome[-train, ]

k_to_try = 10:50
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  pred = knn.reg(btest_train, btest_test, price_train, k = k_to_try[i])
  acc_k[i] = accuracy(pred)
}
plot(k_to_try, acc_k, type = "b", col = "dodgerblue", 
     xlab = "k, number of neighbors", ylab = "regression accuracy",
     main = "Accuracy vs Neighbors")

res <- knn.reg(btest_train, btest_test, price_train, k=30)
plot(price_test, res$pred, xlab="y", ylab=expression(hat(y)))
#MSE
bmse = mean((price_test - res$pred) ^ 2)
bmse #3081.11
#RMSE
brmse = sqrt(mean((price_test - res$pred) ^ 2))
brmse #55.50775
#MAE
bmae = mean(abs(price_test - res$pred))
bmae #36.46393
