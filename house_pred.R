library(readr)
library(dplyr)
library(ggplot2)
library(glmnet)
library(corrplot)
library(caret)
library(MASS)
library(olsrr)
library(modelr)
library(glasso)

# Importing data
data_train <- read_csv('D:/MS DS/Sem 1/DMP/Project/Dataset/train.csv')
data_test <- read_csv('D:/MS DS/Sem 1/DMP/Project/Dataset/test.csv')
sample <- read_csv('D:/MS DS/Sem 1/DMP/Project/Dataset/sample_submission.csv')

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

train_na[,"NAs"] <- sort(train_na[,"NAs"], decreasing = TRUE)
test_na[,"NAs"] <- sort(test_na[,"NAs"], decreasing = TRUE)

ggplot(train_na, aes(x = NAs, y = cols ))+
  geom_bar(stat = "identity", fill = "red")+
  theme_minimal()+
  labs(title = "NA values in train file",
       x = "NA Values",
       y = "Column Names")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = NAs), vjust = 0.5,hjust = -0.2, size = 3.5)

ggplot(test_na, aes(x = NAs, y = cols))+
  geom_bar(stat = "identity", fill = "blue")+
  theme_minimal()+
  labs(title = "NA values in test file",
       x = "NA Values",
       y = "Column Names")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = NAs), vjust = 0.5,hjust = -0.2, size = 3.5)


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

Mode <- function(x){
  names(which.max(table(x,useNA="no")))
}
data_train[is.na(data_train[,'Electrical']),'Electrical'] <- 
  Mode(data_train[,'Electrical'])

data_train <- subset(data_train, select = -c(GrLivArea, TotalBsmtSF, Id,
                                             LotFrontage))

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
  filter(GarageType == "Detchd")
temp <- subset(temp, select = c(GarageCars, GarageType))
temp <- temp %>%
  filter(GarageType == "Detchd") 
data_test$GarageCars[is.na(data_test$GarageCars)] <-
  median(temp$GarageCars,na.rm=TRUE)


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
index <- sample(1:nrow(data_train), 0.75*nrow(data_train))
df_train <- data_train[index,]
df_test <- data_train[-index,]
fit <- lm(log10(SalePrice) ~ . , data = df_train)
summary(fit)
pred <- predict(fit, df_test)
result_lm <- data.frame(cbind(Actual_Values = df_test$SalePrice,
                              Predicted_Values = 10^(pred)))
rmse = sqrt(mean(fit$residuals^2))
aic = AIC(fit)
r2 <- summary(fit)$r.squared
error = data.frame('RMSE' = rmse, 'AIC' = aic,
                   'R-Squared' = summary(fit)$r.squared)
t <- summary(fit)$coefficients[,4] < 0.05
name <- names(which(t == 'TRUE')) 
name <- name[-1]
var <- c("MSZoning","Street","ExterCond","BsmtExposure","BsmtFinType1",
         "HeatingQC","CentralAir","KitchenQual","Functional","PavedDrive",
         "PoolQC","SaleCondition","LotArea","OverallQual","OverallCond",
         "YearBuilt","BsmtFinSF1","BsmtFinSF2","`1stFlrSF`","`2ndFlrSF`",
         "BsmtFullBath","FullBath","HalfBath","KitchenAbvGr","TotRmsAbvGrd",
         "Fireplaces","GarageCars","WoodDeckSF","ScreenPorch","PoolArea",
         "YrSold")  
formula <- as.formula(paste("log10(SalePrice)",
                            paste(var, collapse = "+"),
                            sep = "~"))
fit_name <- lm(formula, data = df_train)
pred_name <- predict(fit_name, df_test)
rmse_p = sqrt(mean(fit_name$residuals^2))
aic = AIC(fit_name)
error_name = data.frame('RMSE' = rmse, 'AIC' = aic,
                   'R-Squared' = summary(fit_name)$r.squared)
result_name <- data.frame(cbind("Actual_Values" = df_test$SalePrice,
                          "Predicted_Values" = 10^(pred_name)))
aic_p <- AIC(fit_name)
r2_p <- summary(fit_name)$r.squared

ggplot(df_test,aes(x = log10(SalePrice), y = 10^(pred)))+
  geom_point()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Actual Vales vs Predicted Values",
       x = "Sale Price",
       y = "Predicted Sale Price")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

ggplot(df_test,aes(x = SalePrice, y = pred))+
  geom_point()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Actual Vales vs Predicted Values",
       x = "Sale Price",
       y = "Predicted Sale Price")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

ggplot(df_test,aes(x = log10(SalePrice), y = 10^(pred_name)))+
  geom_point()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Actual Vales vs Predicted Values",
       x = "Sale Price",
       y = "Predicted Sale Price")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

plot(residuals(fit))
plot(residuals(fit_name))

df_train %>%
  add_residuals(fit, "resid") %>%
  ggplot(aes(sample=resid)) +
  geom_qq() +
  theme_minimal()

df_train %>%
  add_residuals(fit_name, "resid") %>%
  ggplot(aes(sample=resid)) +
  geom_qq() +
  theme_minimal()

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

plot(residuals(model))

# Predictions on test.csv using lm()
train <- data_train
test <- data_test
model <- lm(log10(SalePrice) ~ . , data = train)
summary(model)
prediction <- predict(model, test)
res <- data.frame(cbind(Actual_Values = sample$SalePrice,
                        Predicted_Values = 10^(prediction)))
rmse = summary(model)$r.squared
aic = AIC(model)
ggplot(res,aes(x = Actual_Values, y = Predicted_Values))+
  geom_point()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Actual Vales vs Predicted Values",
       x = "Sale Price",
       y = "Predicted Sale Price")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))



# Lasso
set.seed(100) 
index = sample(1:nrow(data_train), 0.75*nrow(data_train))
df_train = data_train[index,]
df_test = data_train[-index,]
x <- model.matrix(SalePrice~., df_train)[,-1]
y <- log10(df_train$SalePrice)
set.seed(2021)
cv_model <- cv.glmnet(x, y, alpha = 1)
lambdaOptimal <- cv_model$lambda.min
lambdaOptimal
fit1 <- glmnet(x, y, alpha = 1, lambda = lambdaOptimal)
coef(fit1)
y_predicted <- predict(fit1, s = lambdaOptimal, newx = x)
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq
rmse_lasso = sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.min])
rmse
aic_lasso = 

# Stepwise selection
starting.model <- lm(SalePrice ~ ., data = data_train)
simple.model <- lm(SalePrice ~ 1, data = data_train)
stepAIC(starting.model, scope = list(upper = starting.model,lower = simple.model),
        direction = "backward")
stepAIC(starting.model, scope = list(upper = starting.model,lower = simple.model),
        direction = "forward")

model_aic <- lm(log10(SalePrice) ~ ., data = data_train)
#ols_step_both_aic(model_aic)
step_ols <- ols_step_backward_aic(model_aic, details = TRUE)
aic_step <- AIC(step_ols$model)
r2_step <- step_ols$rsq[1]

model <- lm(formula = SalePrice ~ MSZoning + Street + Alley + LotShape + 
              LandContour + Utilities + LotConfig + LandSlope + Neighborhood + 
              Condition1 + Condition2 + BldgType + HouseStyle + RoofStyle + 
              RoofMatl + Exterior1st + Exterior2nd + MasVnrType + ExterQual + 
              ExterCond + Foundation + BsmtQual + BsmtCond + BsmtExposure + 
              BsmtFinType1 + BsmtFinType2 + Heating + HeatingQC + CentralAir + 
              Electrical + KitchenQual + Functional + FireplaceQu + GarageType + 
              GarageFinish + GarageQual + GarageCond + PavedDrive + PoolQC + 
              Fence + MiscFeature + SaleType + SaleCondition + Id + MSSubClass + 
              LotFrontage + LotArea + OverallQual + OverallCond + YearBuilt + 
              YearRemodAdd + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
              TotalBsmtSF + `1stFlrSF` + `2ndFlrSF` + LowQualFinSF + GrLivArea + 
              BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + 
              KitchenAbvGr + TotRmsAbvGrd + Fireplaces + GarageYrBlt + 
              GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + 
              `3SsnPorch` + ScreenPorch + PoolArea + MiscVal + MoSold + 
              YrSold, data = data_train)
summary(model)

for(i in name){
  name[i] <- as.name(name[i])
}

#final table

method = c("LINEAR","P-VALUE","LASSO","STEPWISE_AIC")
rmse_final <- c(rmse, rmse_p, rmse_lasso, "0.060")
aic_final <- c(aic, aic_p, "54.91184",aic_step)
r2_final <- c(r2, r2_p,rsq,r2_step)
result_final <- data.frame('METHOD' = method,'RMSE'=rmse_final,
                           'R-SQUARED' = r2_final)
