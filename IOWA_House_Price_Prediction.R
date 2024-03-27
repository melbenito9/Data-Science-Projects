#Mel Benito

#packages used
library(caret)
library(MASS)
library(tidyverse)
library(mice)
library(dplyr)
library(AppliedPredictiveModeling)
library(car)     
library(ggplot2)
library(pls)
library(glmnet)
library(Metrics)


#House prices data: Exploratory Data Analysis and Visualization

#importing the dataset
hData <- read_csv("~/Documents/housingData.csv")
view(hData)
head(hData)
summary(hData)

#i Overall Quality vs Year Built
ggplot(hData, aes(x = OverallQual, y = YearBuilt)) +
  geom_smooth() 

#ii Masonry Veneer vs Overall Quality
ggplot(hData, aes(x = MasVnrType , y = OverallQual)) +
  geom_violin()

#iii Garage Type vs Garage Area
ggplot(hData, aes(x = GarageType , y = GarageArea)) +
  geom_line()


#iv building type versus neighborhood
ggplot(hData, aes(x = BldgType , y = Neighborhood)) +
  geom_point() 

#v basement condition vs year built
ggplot(hData, aes(x = BsmtCond, y = YearBuilt)) + 
  geom_boxplot()


#Data Pre-processing Steps

#removing columns with 'NA' values > 25%
hData <- hData[ , colSums(is.na(hData))/nrow(hData) < .25] 
hData$Id <- NULL #null the ID values since we do not need these
housingData<- hData
view(hData)


#transforming all text data into numeric data  
char_var <- lapply(hData, class) == "character"
view(char_var)
hData <- as.data.frame(unclass(hData), stringsAsFactors=TRUE)
hData[sapply(hData, is.factor)] <- data.matrix(hData[sapply(hData, is.factor)])
hData[, char_var] <- lapply(hData[, char_var], as.factor)

#replacing numeric and categorical NAs with their mean and mode respectively
fac_mode <- function(x) {
  ij <- unique(x)
  ij[which.max(tabulate(match(x, ij)))]
}

hData <- hData %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))  %>% 
  mutate_if(is.factor, ~replace_na(., fac_mode(na.omit(.))))

view(hData)

#no remaining columns with missing values
colSums(is.na(hData))

##--------------------------------------------------------
# a) OLS Model

#Data Splitting
#Test Set using first 100 observations
test <- hData[1:100,]
view(test)
#Training set using the remaining 900 observations
training <- hData[101:nrow(data),]
view(training)

paste("Test set contains:", nrow(test),"observations with", ncol(test),"variables" )
paste("Training set contains:", nrow(training),"observations with", ncol(training),"variables" )

#--------------------------------------------------------
##creating a linear model with predictors which I believe have an effect on Sale Price
model1 <- lm(log(SalePrice) ~ LotArea + OverallCond + YearBuilt + YearRemodAdd + GrLivArea + KitchenAbvGr + TotRmsAbvGrd + GarageCars, data = training)

#coefficient estimates, p-values, R^2 and adjusted R^2 
summary(model1) 

#AIC, BIC, VIF
AIC(model1)
BIC(model1)
vif(model1)

#using our training model against test data
predict(model1, test)
P1 <- predict(model1, test)

#RMSE
RMSE(P1, log(test$SalePrice))
#----------------------------------------------------------
#ii) Residual Analysis 
plot(model1)

hist(residuals(model1),
     breaks = 20,
     freq = FALSE,
     col = "lightgreen",
     main = "Histogram of residuals",
     xlab = "Residuals")
sdr <- sd(residuals(model1))
curve(dnorm(x, mean = 0, sd = s),
      col = "blue",
      add = TRUE,  
      lwd = 2) 

#EXPLANATION FOR RESIDUAL ANALYSIS
###As the red line is close to the dashed line, we can see that linearity appears to hold here. With high residual values, the points 128, 402, and 875 could be considered outliers.
###The points in the QQ plot aren't quite on a straight line, but the deviations from linearity aren't significant enough to worry us.
###The fitted values don't significantly affect the average magnitude of the standardized residuals, and the red line is roughly horizontal.
###Points with high leverage could be significant if they were removed. The model would change significantly. The grey dotted line in this image represents the Cook's distance, which calculates the impact of eliminating a point. Points outside the dotted line have a significant impact. However, there are no points outside the dotted line in this scenario.
###The residuals tend to follow a normal distribution, as shown by the residual plots and from the histogram.However, there are some extreme outliers that we must consider dealing with before making any more models.


 
#PLS model 
pls<- train(SalePrice~.,
            data<- hData, method<-"pls",
            tuneLength= 20, metric= "RMSE", 
            trControl= trainControl(method = "cv", number = 5))

pls
plot(pls)


res_pls <- pls$results

best_pls <- subset(res_pls, res_pls$RMSE == min(res_pls$RMSE) )
pls_per <- best_pls[,1:4]
RMSE.pls<- min(pls_per$RMSE)
RMSE.pls
Rsquare.pls<- pls_per$Rsquared
Rsquare.pls
pls_per
ncomp.pls<- pls_per$ncomp
ncomp.pls
pls$results


#1 c
# LASSO model 
set.seed(1)
lambda<- 10^seq(-3, 3, length = 50)

#alpha=1 for LASSO model 
lasso_model<- train(SalePrice~.,
                    data<- hData, method<-"glmnet",
                    trControl= trainControl(method = "cv", number = 5),
                    tuneGrid= expand.grid(lambda= seq(0.0001, 1, length= 5), alpha= 1))

lasso_model
lasso_model$results
plot(lasso_model)

#CV RMSE estimate for the final model
res_lasso <- lasso_model$results

best_lasso <- subset(res_lasso, res_lasso$RMSE == min(res_lasso$RMSE) )
lasso_per <- best_lasso[,1:4]
RMSE.lasso<- lasso_per$RMSE
RMSE.lasso
Rsquare.lasso<- lasso_per$Rsquared
Rsquare.lasso
lambda.lasso<- lasso_per$lambda
#best
lasso_per

set.seed(123)
X<- model.matrix(SalePrice~.,
                 data<- hData, method<-"glmnet",
                 trControl= trainControl(method = "cv", number = 5),
                 tuneGrid= expand.grid(lambda= lambda, alpha= 1))

Y<- hData[, "SalePrice"]


# Penalty type - alpha=1 for LASSO 
cv.lambda.lasso<- cv.glmnet(x= X, y= Y,
                            alpha= 1, scale= TRUE)
plot(cv.lambda.lasso)

# Model coefficients
coef(lasso_model$finalModel)


# 1d 
#d 
# Ridge Regression 
# alpha=0 for Ridge Regression 
ridge<- train(SalePrice~.,data<- housingData, 
              method<-"glmnet",
              trControl= trainControl(method = "cv", number = 5),
              tuneGrid= expand.grid(lambda= lambda, alpha= 0))



ridge
ridge$results

#RMSE, Rsquared and lambda values
#-------------------------------------------------
res_ridge <- ridge$results
k<- res_ridge$RMSE == min(res_ridge$RMSE)
best_ridge <- subset(res_ridge, k ) 
ridge_per <- best_ridge[1,1:4]
RMSE.ridge<- ridge_per$RMSE
r.ridge<- ridge_per$Rsquared
lambda.ridge<- ridge_per$lambda

RMSE.ridge
r.ridge
lambda.ridge


#PCR
pcr<- train(SalePrice~., data=housingData, 
            method = "pcr",tuneLength=20, metric="RMSE", 
            trControl=(trainControl(method="cv", number=5)),  
            preProcess=c("center","scale"))
pcr

res_pcr <- pcr$results
k<- res_pcr$RMSE == min(res_pcr$RMSE)
best_pcr <- subset(res_pcr, k ) 
pcr_per <- best_pcr[1,1:3]
RMSE.pcr<- pcr_per$RMSE
r.pcr<- pcr_per$Rsquared
ncomp.pcr<- pcr_per$ncomp

RMSE.ridge
r.ridge
ncomp.pcr


# Elasticnet 
set.seed(123)

elastic<- train(SalePrice~., data = housingData, 
                method = "glmnet",
                trControl = trainControl(method = "cv", number = 5),
                tuneLength = 10)

elastic
elastic$results
# RMSE value for Elasticnet 


#Alpha value 
alpha.elastic<- 0.5
alpha.elastic

res_elast<- res_elast %>% filter(elastic$results$alpha == 0.5)
res_elast
k<- res_elast$RMSE == min(res_elast$RMSE)
best_elast <- subset(res_elast, k ) 
elast_per <- best_elast[1,1:4]
RMSE.elast<- elast_per$RMSE
r.elast<- elast_per$Rsquared
lambda.elast<- elast_per$lambda

RMSE.elast
r.elast
lambda.elast

# Summary Table 
model<- c("OLS", "PLS", "LASSO", "Ridge", "PCR", "ElasticNet")

notes<- c("lm ", "caret", "caret and elasticNet", "caret and elasticNet", "caret", "caret and elasticNet")

hyperparameters<- c("N/A", paste("ncomp= ", ncomp.pls),paste("alpha= ", 1," lambda= ", lambda.lasso), paste("alpha= ", 0, " lambda= ", lambda.ridge), paste("ncomp= ", 4),paste("alpha= ", alpha.elastic, " lambda= ", lambda.elast))

rmse_values<- c(0.1460, RMSE.pls, lasso_per$RMSE, RMSE.ridge, RMSE.pcr, RMSE.elast)

rsquared<- c(0.8302, Rsquare.pls, lasso_per$Rsquared, r.ridge, r.pcr, r.elast)

tab<- data.frame(model)
tab$Notes<- notes
tab$Hyperparameters<- hyperparameters
tab$CV_RMSE<- rmse_values
tab$CV_R2<- rsquared

tab<- tab[order(tab$CV_RMSE), ]
tab





















