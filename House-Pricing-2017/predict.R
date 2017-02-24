library(caTools)
library(rpart)
library(glmnet)
library(randomForest)
source("accuracy.R")


split_index = createDataPartition(training$SalePrice, p=0.75, list=F)
sample_train = training[split_index,]
sample_test = training[-split_index,]


# Linear Regression Model
lm_model <- lm(log(SalePrice) ~ RichNeighborhood + OverallQual + log(GrLivArea) + GarageCars + CentralAir + YearBuilt + SaleCondition + log(LotArea) + MSZoningRM + log(LotFrontage+1) + BsmtQual + Residential + GoodBasementExposure + ExterCond + Fireplaces + OverallCond + log(TotalBsmtSF+1), data=sample_train)

predicted_price = exp(predict(lm_model, sample_test))
RMS = accuracy(predicted_price, sample_test$SalePrice)


# Final prediction
prediction <- exp(predict(lm_model, testing))
output <- cbind(testing$Id, prediction)
write.csv(output, file="submission.csv", row.names = F)

