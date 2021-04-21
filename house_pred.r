library(readr)
library(dplyr)
library(ggplot2)
library(imputeMissings)
library(glmnet)
library(corrplot)
library(caret)
library(olsrr)

# Importing data
data_train <- read_csv("/Users/uthresh/Downloads/house-prices-advanced-regression-techniques/train.csv")
data_test <- read_csv("/Users/uthresh/Downloads/house-prices-advanced-regression-techniques/test.csv")

# Count NA
countNA <- function(data, byrow = FALSE){
  if(byrow == FALSE){
    result<-NULL
    for (i in 1:ncol(data)){
      temp<-sum(is.na(data[,i]))
      temp<-as.data.frame(temp)
      temp$cols<-colnames(data)[i]
      colnames(temp)<-c('NAs','cols') 
      result<-rbind(result,temp)
    }
    return(result)
  }
  else{
    result<-NULL
    for (i in 1:nrow(data)){
      temp<-sum(is.na(data[i,]))
      temp<-as.data.frame(temp)
      temp$cols<-rownames(data)[i]
      colnames(temp)<-c('NAs','cols') 
      result<-rbind(result,temp)
      
    }
    return(result) 
  }
}

train_na <- countNA(data_train)
train_na <- train_na %>%
  filter(NAs > 0)
test_na <- countNA(data_test)
test_na <- test_na %>%
  filter(NAs > 0)

# Imputations for data_train
lst <- c('PoolQC', 'MiscFeature', 'Alley', 'Fence','FireplaceQu',
         'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond',
         'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',
         'MasVnrType')
for(lst_name in lst){
  data_train[,lst_name][is.na(data_train[,lst_name])] <- "None"
}

data_train[,'GarageYrBlt'][is.na(data_train[,'GarageYrBlt'])] <- 0
data_train[,'MasVnrArea'][is.na(data_train[,'MasVnrArea'])] <- 0

data_train <- data_train %>%
  select(-c(TotalBsmtSF, GrLivArea, LotFrontage, Id))
  

data_train$LotFrontage[is.na(data_train$LotFrontage == "Blmngtn")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Blmngtn"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "BrkSide")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "BrkSide"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "ClearCr")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "ClearCr"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "CollgCr")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "CollgCr"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Crawfor")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Crawfor"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Edwards")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Edwards"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Gilbert")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Gilbert"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "IDOTRR")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "IDOTRR"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "MeadowV")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "MeadowV"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Mitchel")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Mitchel"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "NAmes")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NAmes"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "NoRidge")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NoRidge"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "NridgHt")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NridgHt"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "NWAmes")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NWAmes"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "OldTown")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "OldTown"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Sawyer")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Sawyer"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "SawyerW")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "SawyerW"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Somerst")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Somerst"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "SWISU")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "SWISU"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Timber")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Timber"], na.rm = TRUE)
data_train$LotFrontage[is.na(data_train$LotFrontage == "Veenker")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Veenker"], na.rm = TRUE)

Mode <- function(x){
  names(which.max(table(x,useNA="no")))
}
data_train[is.na(data_train[,'Electrical']),'Electrical'] <- 
  Mode(data_train[,'Electrical'])

#Imputations for data_test
lst <- c('PoolQC', 'MiscFeature', 'Alley', 'Fence','FireplaceQu',
         'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond',
         'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',
         'MasVnrType')
for(lst_name in lst){
  data_test[,lst_name][is.na(data_test[,lst_name])] <- "None"
}

data_test[,'GarageYrBlt'][is.na(data_test[,'GarageYrBlt'])] <- 0
data_test[,'MasVnrArea'][is.na(data_test[,'MasVnrArea'])] <- 0
data_test[,'BsmtFinSF1'][is.na(data_test[,'BsmtFinSF1'])] <- 0
data_test[,'BsmtFinSF2'][is.na(data_test[,'BsmtFinSF2'])] <- 0
data_test[,'BsmtUnfSF'][is.na(data_test[,'BsmtUnfSF'])] <- 0
data_test[,'TotalBsmtSF'][is.na(data_test[,'TotalBsmtSF'])] <- 0
data_test[,'BsmtFullBath'][is.na(data_test[,'BsmtFullBath'])] <- 0
data_test[,'BsmtHalfBath'][is.na(data_test[,'BsmtHalfBath'])] <- 0
data_test[,'GarageArea'][is.na(data_test[,'GarageArea'])] <- 0
data_test[,'Functional'][is.na(data_test[,'Functional'])] <- "Typ"

temp <- data_test %>%
  filter(GarageType == "Detchd") %>%
  select(GarageCars, GarageType) %>%
  filter(GarageType == "Detchd") 
data_test$GarageCars[is.na(data_test$GarageCars)] <-
  median(temp$GarageCars,na.rm=TRUE)

temp1 <- data_test %>%
  select(Neighborhood, LotFrontage) %>%
  group_by(Neighborhood)

data_test$LotFrontage[is.na(data_test$LotFrontage == "Blmngtn")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Blmngtn"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "BrkSide")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "BrkSide"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "ClearCr")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "ClearCr"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "CollgCr")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "CollgCr"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Crawfor")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Crawfor"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Edwards")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Edwards"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Gilbert")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Gilbert"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "IDOTRR")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "IDOTRR"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "MeadowV")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "MeadowV"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Mitchel")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Mitchel"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "NAmes")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NAmes"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "NoRidge")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NoRidge"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "NridgHt")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NridgHt"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "NWAmes")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "NWAmes"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "OldTown")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "OldTown"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Sawyer")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Sawyer"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "SawyerW")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "SawyerW"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Somerst")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Somerst"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "SWISU")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "SWISU"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Timber")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Timber"], na.rm = TRUE)
data_test$LotFrontage[is.na(data_test$LotFrontage == "Veenker")] <-
  median(temp1$LotFrontage[temp1$Neighborhood == "Veenker"], na.rm = TRUE)

Mode <- function(x){
  names(which.max(table(x,useNA="no")))
}
data_test[is.na(data_test[,'Electrical']),'Electrical'] <- 
  Mode(data_test[,'Electrical'])
data_test[is.na(data_test[,'MSZoning']),'MSZoning'] <- 
  Mode(data_test[,'MSZoning'])
data_test[is.na(data_test[,'Utilities']),'Utilities'] <- 
  Mode(data_test[,'Utilities'])
data_test[is.na(data_test[,'Exterior1st']),'Exterior1st'] <- 
  Mode(data_test[,'Exterior1st'])
data_test[is.na(data_test[,'Exterior2nd']),'Exterior2nd'] <- 
  Mode(data_test[,'Exterior2nd'])
data_test[is.na(data_test[,'KitchenQual']),'KitchenQual'] <- 
  Mode(data_test[,'KitchenQual'])
data_test[is.na(data_test[,'SaleType']),'SaleType'] <-
  Mode(data_test[,'SaleType'])

# EDA
#Sales count
ggplot(data_train, aes(x = SalePrice))+
  geom_histogram(bins = 30, fill = "pink", color = "black")+
  labs(title = "Sales Price Count",
       x = "Sales Price",
       y = "Count")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_train, aes(x = log10(SalePrice)))+
  geom_histogram(bins = 30, fill = "pink", color = "black")+
  labs(title = "Sale Price Count",
       x = "Sale Price",
       y = "Count")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

#Ground living area
ggplot(data_train, aes(x= GrLivArea ,y= SalePrice))+
  geom_point(color = "blue")
data_train <- data_train %>%
  filter(GrLivArea < 4000)
ggplot(data_train, aes(x= GrLivArea ,y= log10(SalePrice)))+
  geom_point(color = "blue")+
  labs(title = "Total sq feet of Ground Living Area and Sale Price comparison",
       x = "Ground Living Area (sqft)",
       y = "Sale Price")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Total basement surface
ggplot(data_train, aes(x=TotalBsmtSF, y=log10(SalePrice)))+
  geom_point(color ="Dark green")+
  labs(title = "Total sq feet of Basement area and Sale Price comparison",
       x = "Total Basement Area (sqft)",
       y = "Sale Price")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# year built
ggplot(data_train,aes(x = as.factor(YearBuilt), y = log10(SalePrice), 
                      fill = as.factor(YearBuilt)))+
  geom_boxplot()+
  labs(title = "Sale Price vs Year Built",
       x = "Year Built",
       y = "Sale Price")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))

# Overall Quality
ggplot(data_train,aes(x = as.factor(OverallQual),y = log10(SalePrice),
                fill = as.factor(OverallQual))) +
  geom_boxplot()+
  labs(title = "Overall Quality of house and Sale Price comparison ",
       x = "Overall Quality",
       y = "Sale Price")+
  theme_minimal()+
  scale_fill_discrete(name = "Overall Quality", 
                      labels = c("Very Poor","Poor","Fair","Below Average",
                                 "Average","Above Average","Good","Very Good",
                                 "Excellent","Very Excellent"))+
  theme(plot.title = element_text(hjust = 0.5))

# Garage Cars
ggplot(data_train,aes(x = as.factor(GarageCars),y = log10(SalePrice))) +
  geom_boxplot(fill = "purple") +
  labs(title = "Size of garage in car capacity and Sale Price comparison",
       x = "Garage cars",
       y="Sale Price")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Plot Full bath
ggplot(data_train,aes(x=as.factor(FullBath),y=log10(SalePrice))) +
  geom_boxplot(fill = "gray") +
  labs(title = "Full Bathrooms and Sale Price comparison",
       x = "Full Bath", 
       y = "Sale Price")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# Converting categorical data to numeric
data_train_numeric <- Filter(is.numeric, data_train)
data_train_cat <- data_train[, sapply(data_train, class) %in% 
                               c('character', 'factor')]
data_train_cat <- as.data.frame(sapply(data_train_cat, 
                                    function(x) as.numeric(as.factor(x))))
cat_train <- cbind(data_train_cat,SalePrice = data_train$SalePrice)
data_train <- cbind(data_train_cat,data_train_numeric)


data_test_numeric <- Filter(is.numeric, data_test)
data_test_cat <- data_test[, sapply(data_test, class) %in% 
                               c('character', 'factor')]
data_test_cat <- as.data.frame(sapply(data_test_cat, 
                                       function(x) as.numeric(as.factor(x))))
data_test <- cbind(data_test_cat,data_test_numeric)


# Correlation Plot
corrplot(cor(data_train), order="hclust",
         tl.col="gray", tl.srt=90, type = "lower")
corrplot(cor(cat_train), order="hclust",
         tl.col="red", tl.srt=90, type = "lower")
corrplot(cor(data_train_numeric), order="hclust",
         tl.col="red", tl.srt=90, type = "lower")

# Linear Model
set.seed(100)
data_train <- data_train %>%
  select(-c(TotalBsmtSF, GrLivArea))
index <- sample(1:nrow(data_train), 0.75*nrow(data_train))
df_train <- data_train[index,]
df_test <- data_train[-index,]
fit <- lm(log10(SalePrice) ~ . , data = df_train)
summary(fit)
t <- summary(fit)$coefficients[,4] < 0.05
names(which(t == 'TRUE')) # 29 variables
pred <- predict(fit, df_test)
result_lm <- data.frame(cbind(Actual_Values = df_test$SalePrice,
                              Predicted_Values = 10^(pred)))
plot(result_lm)
rmse = RMSE(obs=df_test$SalePrice, pred=10^(pred))
mae = MAE(obs=df_test$SalePrice, pred=10^(pred))
error = data.frame('RMSE' = rmse, 'MAE' = mae,
                      'R-Squared' = summary(fit)$r.squared)

# Predictions on test.csv using lm()
train <- data_train
test <- data_test
model <- lm(log10(SalePrice) ~ . , data = train)
summary(model)
prediction <- predict(model, test)
res <- data.frame(cbind(Actual_Values = sample$SalePrice,
                        Predicted_Values = 10^(prediction)))
rmse = summary(model)$r.squared

# Using CV
set.seed(150)
train_control <- trainControl(method="cv", number=10)
model1 <- train(log10(SalePrice)~., data=data_train, 
               trControl=train_control, method="lm")
summary(model1)
t <- summary(model1)$coefficients[,4] < 0.05
names(which(t == 'TRUE')) 
pred1 <- predict(model1, df_test)
resul1 <- data.frame(cbind(Actual = df_test$SalePrice,
                           Predicted = pred1))
rmse = RMSE(obs=df_test$SalePrice, pred=10^(pred1))
mae = MAE(obs=df_test$SalePrice, pred=10^(pred1))
error_cv <- data.frame('RMSE' = rmse, 'MAE' = mae,
                       'R-Squared' = summary(model1)$r.squared)

set.seed(100) 
index = sample(1:nrow(data_train), 0.75*nrow(data_train))
train = data_train[index,]
test = data_train[-index,]

x <- model.matrix(SalePrice~., train)[,-1]
y <- log10(train$SalePrice)

set.seed(2021)
cv_model <- cv.glmnet(x, y, alpha = 1)
lambdaOptimal <- cv_model$lambda.min
lambdaOptimal

model <- glmnet(x, y, alpha = 1, lambda = lambdaOptimal)
coef(model)

y_predicted <- predict(model, s = lambdaOptimal, newx = x)
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq

RMSE(obs = test$SalePrice, pred = y_predicted)

stepwiseModel <- lm(log10(SalePrice) ~ ., data = train)
a <- ols_step_backward_aic(stepwiseModel, progress = T)
b <- ols_step_forward_aic(stepwiseModel, progress = T)



model1 <- b
predict(model1, test)
summary(model1)
plot(b)