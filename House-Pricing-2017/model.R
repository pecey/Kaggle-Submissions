setwd("~/Desktop/Kaggle/HousePrices")
source("accuracy.R")
source("helper.R")
library(data.table)
library(rpart)
library(Amelia)
library(caTools)
library(mice)
library(lattice)
library(corrplot)
library(plyr)
training = fread("train.csv", na.strings = c("","NA"))
testing = fread("test.csv", na.strings = c("","NA"))
#missmap(training, y.labels = NULL, y.at = NULL)
#str(training)
#summary(training)

# Changing some column names
training <- rename_column(training, "1stFlrSF", "AreaFloor1")
training <- rename_column(training, "2ndFlrSF", "AreaFloor2")
training <- rename_column(training, "3SsnPorch", "Porch3Season")

testing <- rename_column(testing, "1stFlrSF", "AreaFloor1")
testing <- rename_column(testing, "2ndFlrSF", "AreaFloor2")
testing <- rename_column(testing, "3SsnPorch", "Porch3Season")

# Fixing false NAs
training$BsmtExposure[is.na(training$BsmtExposure)] <- "NoBasement"
testing$BsmtExposure[is.na(testing$BsmtExposure)] <- "NoBasement"

training$BsmtFinType2[is.na(training$BsmtFinType2)] <- "NoBasement"
testing$BsmtFinType2[is.na(testing$BsmtFinType2)] <- "NoBasement"

training$BsmtFinType1[is.na(training$BsmtFinType1)] <- "NoBasement"
testing$BsmtFinType1[is.na(testing$BsmtFinType1)] <- "NoBasement"

training$BsmtCond[is.na(training$BsmtCond)] <- "NoBasement"
testing$BsmtCond[is.na(testing$BsmtCond)] <- "NoBasement"

training$BsmtQual[is.na(training$BsmtQual)] <- "NoBasement"
testing$BsmtQual[is.na(testing$BsmtQual)] <- "NoBasement"

training$Fence[is.na(training$Fence)] <- "NoFence"
testing$Fence[is.na(testing$Fence)] <- "NoFence"

training$PoolQC[is.na(training$PoolQC)] <- "NoPool"
testing$PoolQC[is.na(testing$PoolQC)] <- "NoPool"

training$GarageCond[is.na(training$GarageCond)] <- "NoGarage"
testing$GarageCond[is.na(testing$GarageCond)] <- "NoGarage"

training$GarageQual[is.na(training$GarageQual)] <- "NoGarage"
testing$GarageQual[is.na(testing$GarageQual)] <- "NoGarage"

training$GarageFinish[is.na(training$GarageFinish)] <- "NoGarage"
testing$GarageFinish[is.na(testing$GarageFinish)] <- "NoGarage"

training$GarageType[is.na(training$GarageType)] <- "NoGarage"
testing$GarageType[is.na(testing$GarageType)] <- "NoGarage"

training$FireplaceQu[is.na(training$FireplaceQu)] <- "NoFireplace"
testing$FireplaceQu[is.na(testing$FireplaceQu)] <- "NoFireplace"

training$Alley[is.na(training$Alley)] <- "NoAlley"
testing$Alley[is.na(testing$Alley)] <- "NoAlley"

training$MiscFeature[is.na(training$MiscFeature)] <- "None"
testing$MiscFeature[is.na(testing$MiscFeature)] <- "None"

# Imputing using mice
imp.train <- mice(training, method="cart", printFlag = F)
training <- complete(imp.train)

imp.test <- mice(testing, method="cart", printFlag = F)
testing <- complete(imp.test)

# Other imputations
training$MasVnrType[is.na(training$MasVnrType)] <- "None"
testing$MasVnrType[is.na(testing$MasVnrType)] <- "None"

testing$MSZoning[is.na(testing$MSZoning)] <- "RL"
testing$Functional[is.na(testing$Functional)] <- "Typ"
testing$Utilities[is.na(testing$Utilities)] <- "AllPub"
testing$SaleType[is.na(testing$SaleType)] <- "WD"
testing$KitchenQual[is.na(testing$KitchenQual)] <- "TA"
testing$Exterior1st[is.na(testing$Exterior1st)] <- "VinylSd"
testing$Exterior2nd[is.na(testing$Exterior2nd)] <- "VinylSd"


#plot_box_and_histograms(training)


#Removing features where one variable is dominating
training$Street <- NULL
testing$Street <- NULL

training$Utilities <- NULL
testing$Utilities <- NULL

training$Condition2 <- NULL
testing$Condition2 <- NULL

training$RoofMatl <- NULL
testing$RoofMatl <- NULL

training$Heating <- NULL
testing$Heating <- NULL

training$PoolQC <- NULL
testing$PoolQC <- NULL

#plot_box_and_histograms(training)

# Further Imputation
training$MSZoning[!training$MSZoning %in% c("RL","RM")] <- "Others"
testing$MSZoning[!testing$MSZoning %in% c("RL","RM")] <- "Others"

training$HasAlley[training$Alley %in% c("NoAlley")] <- 0
training$HasAlley[training$Alley %in% c("Grvl", "Pave")] <- 1
testing$HasAlley[testing$Alley %in% c("NoAlley")] <- 0
testing$HasAlley[testing$Alley %in% c("Grvl", "Pave")] <- 1
training$HasAlley <- as.factor(training$HasAlley)
testing$HasAlley <- as.factor(testing$HasAlley)

training$Exterior1st[training$Exterior1st %in% c("AsphShn","BrkComm","CBlock","ImStucc","Stone")] <- "UncommonExterior"
testing$Exterior1st[testing$Exterior1st %in% c("AsphShn","BrkComm","CBlock","ImStucc","Stone")] <- "UncommonExterior"

training$Exterior2nd[training$Exterior2nd %in% c("Brk Cmn")] <- "BrkComm"
testing$Exterior2nd[testing$Exterior2nd %in% c("Brk Cmn")] <- "BrkComm"

training$Exterior2nd[training$Exterior2nd %in% c("AsphShn","BrkComm","CBlock","ImStucc","Other","Stone")] <- "UncommonExterior"
testing$Exterior2nd[testing$Exterior2nd %in% c("AsphShn","BrkComm","CBlock","ImStucc","Other","Stone")] <- "UncommonExterior"

training$ExterCond[training$ExterCond %in% c("Po")] <- "Fa"
training$ExterCond[training$ExterCond %in% c("Ex")] <- "Gd"
testing$ExterCond[testing$ExterCond %in% c("Po")] <- "Fa"
testing$ExterCond[testing$ExterCond %in% c("Ex")] <- "Gd"

training$BsmtCond[training$BsmtCond %in% c("Po")] <- "Fa"
testing$BsmtCond[testing$BsmtCond %in% c("Po")] <- "Fa"

training$HeatingQC[training$HeatingQC %in% c("Po")] <- "Fa"
testing$HeatingQC[testing$HeatingQC %in% c("Po")] <- "Fa"

training$Electrical[training$Electrical %in% c("FuseP")] <- "FuseF"
testing$Electrical[testing$Electrical %in% c("FuseP")] <- "FuseF"
training$Electrical[training$Electrical %in% c("Mix")] <- "FuseA"
testing$Electrical[testing$Electrical %in% c("Mix")] <- "FuseA"
training$Electrical[is.na(training$Electrical)] = "SBrkr"


training$Functional[training$Functional %in% c("Maj1","Maj2","Sev")] <- "Major"
testing$Functional[testing$Functional %in% c("Maj1","Maj2","Sev")] <- "Major"

training$GarageCond[training$GarageCond %in% c("Po")] <- "Fa"
testing$GarageCond[testing$GarageCond %in% c("Po")] <- "Fa"
training$GarageCond[training$GarageCond %in% c("Ex")] <- "Gd"
testing$GarageCond[testing$GarageCond %in% c("Ex")] <- "Gd"

training$GarageQual[training$GarageQual %in% c("Po")] <- "Fa"
testing$GarageQual[testing$GarageQual %in% c("Po")] <- "Fa"
training$GarageQual[training$GarageQual %in% c("Ex")] <- "Gd"
testing$GarageQual[testing$GarageQual %in% c("Ex")] <- "Gd"

training$HasFence <- 1
training$HasFence[training$Fence %in% ("NoFence")] <- 0
testing$HasFence <- 1
testing$HasFence[testing$Fence %in% c("NoFence")] <- 0
training$HasFence <- as.factor(training$HasFence)
testing$HasFence <- as.factor(testing$HasFence)

  
#plot_box_and_histograms(training)

# Co-relation
numeric_subset = subset(training, select = sapply(training, is.numeric))
numeric_subset$Id <- NULL
numeric_subset$SalePrice <- NULL
corrplot.mixed(cor(numeric_subset), lower="circle", upper="circle", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

# Transformations

training$RegularLotShape[training$LotShape %in% c("Reg")] <- 1
training$RegularLotShape[training$LotShape %in% c("IR1","IR2","IR3")] <- 0
testing$RegularLotShape[testing$LotShape %in% c("Reg")] <- 1
testing$RegularLotShape[testing$LotShape %in% c("IR1","IR2","IR3")] <- 0
training$RegularLotShape <- as.factor(training$RegularLotShape)
testing$RegularLotShape <- as.factor(testing$RegularLotShape)

training$FlatLandContour[training$LandContour %in% c("Lvl")] <- 1
training$FlatLandContour[training$LandContour %in% c("Bnk","HLS","Low")] <- 0
testing$FlatLandContour[testing$LandContour %in% c("Lvl")] <- 1
testing$FlatLandContour[testing$LandContour %in% c("Bnk","HLS","Low")] <- 0
training$FlatLandContour <- as.factor(training$FlatLandContour)
testing$FlatLandContour <- as.factor(testing$FlatLandContour)

training$LandHasSlope[training$LandSlope %in% c("Mod","Sev")] <- 1
training$LandHasSlope[training$LandSlope %in% c("Gtl")] <- 0
testing$LandHasSlope[testing$LandSlope %in% c("Mod","Sev")] <- 1
testing$LandHasSlope[testing$LandSlope %in% c("Gtl")] <- 0
training$LandHasSlope <- as.factor(training$LandHasSlope)
testing$LandHasSlope <- as.factor(testing$LandHasSlope)

training$RoofStyle[training$RoofStyle %in% c("Flat","Gambrel","Mansard","Shed")] <- "Others"
testing$RoofStyle[testing$RoofStyle %in% c("Flat","Gambrel","Mansard","Shed")] <- "Others"

training$WasRemodeled <- 1
training$WasRemodeled[training$YearBuilt == training$YearRemodAdd] <- 0
training$WasRemodeled <- as.factor(training$WasRemodeled)
testing$WasRemodeled <- 1
testing$WasRemodeled[testing$YearBuilt == testing$YearRemodAdd] <-0
testing$WasRemodeled <- as.factor(testing$WasRemodeled)

training$MiscFeature[!training$MiscFeature %in% c("None")] <- 1
training$MiscFeature[training$MiscFeature %in% c("None")] <- 0
testing$MiscFeature[!testing$MiscFeature %in% c("None")] <- 1
testing$MiscFeature[testing$MiscFeature %in% c("None")] <- 0

training$SaleCondition[training$SaleCondition %in% c("AdjLand")] <- "Alloca"
testing$SaleCondition[testing$SaleCondition %in% c("AdjLand")] <- "Alloca"

training$Residential <- 0
training$Residential[training$Condition1 %in% c("Norm")] <- 1
testing$Residential <- 0
testing$Residential[testing$Condition1 %in% c("Norm")] <- 1

training$BsmtExposure[training$BsmtExposure %in% c("NoBasement")] <- "No"
testing$BsmtExposure[testing$BsmtExposure %in% c("NoBasement")] <- "No"

training$HasGarage <- 1
training$HasGarage[training$GarageType %in% c("NoGarage")] <- 0
testing$HasGarage <- 1
testing$HasGarage[testing$GarageType %in% c("NoGarage")] <- 0

training$HasWoodenDeck <- 1
training$HasWoodenDeck[training$WoodDeckSF == 0] <- 0
testing$HasWoodenDeck <- 1
testing$HasWoodenDeck[testing$WoodDeckSF == 0] <- 0

training$HasOpenPorch <- 1
training$HasOpenPorch[training$OpenPorchSF == 0] <- 0
testing$HasOpenPorch <- 1
testing$HasOpenPorch[testing$OpenPorchSF == 0] <- 0

training$HasEnclosedPorch <- 1
training$HasEnclosedPorch[training$EnclosedPorch == 0] <- 0
testing$HasEnclosedPorch <- 1
testing$HasEnclosedPorch[testing$EnclosedPorch == 0] <- 0

training$Has3SNPorch <- 1
training$Has3SNPorch[training$`3SsnPorch` == 0] <- 0
testing$Has3SNPorch <- 1
testing$Has3SNPorch[testing$`3SsnPorch` == 0] <- 0

training$HasScreenPorch <- 1
training$HasScreenPorch[training$ScreenPorch == 0] <- 0
testing$HasScreenPorch <- 1
testing$HasScreenPorch[testing$ScreenPorch == 0] <- 0

training$RichNeighborhood <- 0
training$RichNeighborhood[training$SalePrice >= 214000] <- 1
testing$RichNeighborhood <- 0
testing$RichNeighborhood[testing$SalePrice >= 214000] <- 1

training$Bedrooms[training$BedroomAbvGr %in% c(5,6,8)] <- "5+"
training$Bedrooms[training$BedroomAbvGr %in% c(4)] <- "4"
training$Bedrooms[training$BedroomAbvGr %in% c(3)] <- "3"
training$Bedrooms[training$BedroomAbvGr %in% c(0,1,2)] <- "2AndBelow"
testing$Bedrooms[testing$BedroomAbvGr %in% c(5,6,8)] <- "5+"
testing$Bedrooms[testing$BedroomAbvGr %in% c(4)] <- "4"
testing$Bedrooms[testing$BedroomAbvGr %in% c(3)] <- "3"
testing$Bedrooms[testing$BedroomAbvGr %in% c(0,1,2)] <- "2AndBelow"

training$Kitchen[training$KitchenAbvGr %in% c(1)] <- "1"
training$Kitchen[training$KitchenAbvGr %in% c(0,2,3)] <- "Others"
testing$Kitchen[testing$KitchenAbvGr %in% c(1)] <- "1"
testing$Kitchen[testing$KitchenAbvGr %in% c(0,2,3)] <- "Others"

training$TotalFullBath <- training$FullBath + training$BsmtFullBath
training$TotalFullBath[training$TotalFullBath %in% c(4,6)] <- "4+"
testing$TotalFullBath <- testing$FullBath + testing$BsmtFullBath
testing$TotalFullBath[testing$TotalFullBath %in% c(4,6)] <- "4+"
training$TotalFullBath[training$TotalFullBath == 0] = 1
testing$TotalFullBath[testing$TotalFullBath == 0] = 1

training$TotalHalfBath <- training$HalfBath + training$BsmtHalfBath
training$TotalHalfBath[training$TotalHalfBath %in% c(0,2,3,4)] <- "2+"
testing$TotalHalfBath <- testing$HalfBath + testing$BsmtHalfBath
testing$TotalHalfBath[testing$TotalHalfBath %in% c(2,3,4)] <- "2+"

training$TotalHalfBath[training$TotalHalfBath == 0] = 1
testing$TotalHalfBath[testing$TotalHalfBath == 0] = 1

training$GoodBasementExposure <- 0
training$GoodBasementExposure[training$BsmtExposure %in% c("Gd")] <- 1
testing$GoodBasementExposure <- 0
testing$GoodBasementExposure[testing$BsmtExposure %in% c("Gd")] <- 1

training$PavedDrive[training$PavedDrive %in% c("P")] <- "Y"
testing$PavedDrive[testing$PavedDrive %in% c("P")] <- "Y"

training$HeatingQualAvgOrBetter <- 0
training$HeatingQualAvgOrBetter[training$HeatingQC %in% c("TA","Gd")] <- 1
testing$HeatingQualAvgOrBetter <- 0
testing$HeatingQualAvgOrBetter[testing$HeatingQC %in% c("TA","Gd")] <- 1

training$BuildingTypeShared <- 0
training$BuildingTypeShared[training$BldgType %in% c("Duplex","2fmCon","Twnhs")] <- 1
testing$BuildingTypeShared <- 0
testing$BuildingTypeShared[testing$BldgType %in% c("Duplex","2fmCon","Twnhs")] <- 1

training$MSZoningRM <- 0
training$MSZoningRM[training$MSZoning %in% c("RM")] <- 1
testing$MSZoningRM <- 0
testing$MSZoningRM[testing$MSZoning %in% c("RM")] <- 1

training$SaleInFamily <- 0
training$SaleInFamily[training$SaleCondition %in% c("Family")] <- 1
testing$SaleInFamily <- 0
testing$SaleInFamily[testing$SaleCondition %in% c("Family")] <- 1

training$NewHome <- 0
training$NewHome[training$SaleCondition %in% c("Partial")] <- 1
testing$NewHome <- 0
testing$NewHome[testing$SaleCondition %in% c("Partial")] <- 1

training$NormalCondition <- 0
training$NormalCondition[training$SaleCondition %in% c("Normal","Partial")] <-1
testing$NormalCondition <- 0
testing$NormalCondition[testing$SaleCondition %in% c("Normal","Partial")] <-1

training$AverageFunctional <- 0
training$AverageFunctional[training$Functional %in% c("Typ")] <- 1
testing$AverageFunctional <- 0
testing$AverageFunctional[testing$Functional %in% c("Typ")] <- 1

training$UsableBasement <- 0
training$UsableBasement[training$BsmtQual %in% c("TA","Ex","Gd")] <- 1
testing$UsableBasement <- 0
testing$UsableBasement[testing$BsmtQual %in% c("TA","Ex","Gd")] <- 1

# Check if still any NA values 
sort(sapply(training, function(x){ sum(is.na(x))}), decreasing = TRUE)
sort(sapply(testing, function(x){ sum(is.na(x))}), decreasing = TRUE)

plot_box_and_histograms(training)


