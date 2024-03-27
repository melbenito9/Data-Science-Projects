library(mice)
library(dplyr)
library(tidyverse)
library(forcats)
library(caret)
library(glmnet)
library(tree)
library(Metrics)
library(pls)
library(outliers)
library(earth)
library(party)
library(partykit)
library(caTools)
library(magrittr)
library(rpart)
library(e1071)
library(skimr)
library(corrplot)

# Reading train and test data separately
train_data <- read.csv(file = "~/Downloads/Train.csv")
test_data <- read.csv(file = "~/Downloads/Test.csv")

#glimpse(train_data)
# summary(train_data)
skim(train_data)

ggplot(train_data)+geom_point(mapping = aes(y=revenue, x=custId))
ggplot(train_data)+geom_point(mapping = aes(y=revenue, x=medium))
ggplot(train_data)+geom_point(mapping = aes(y=revenue, x=deviceCategory))

######################################## Data Preparation #########################################

######################################### For numerical Data ######################################
# Replacing the blank spaces with NA for train data
train_data[train_data==""] <- NA
# Sometimes, it takes NA in the form of string i.e, "NA". Replacing those NA strings to NA for train data.
train_data[train_data=="NA"] <- NA
# Replacing the blank spaces with NA for test data
test_data[test_data==""] <- NA
# Sometimes, it takes NA in the form of string i.e, "NA". Replacing those NA strings to NA for test data.
test_data[test_data=="NA"] <- NA

# Removing the columns which have more than 80% missing values
train_d <- train_data[ lapply( train_data, function(x) sum(is.na(x)) / length(x) ) < 0.8 ]

sapply(train_d, function(x) sum(is.na(x)))

# Replacing NA with zero for newVists,bounces,pageviews columns for train data
train_d$newVisits[is.na(train_d$newVisits)] <- 0
train_d$bounces[is.na(train_d$bounces)] <- 0
train_d$pageviews[is.na(train_d$pageviews)] <- 0
# Replacing NA with zero for newVists,bounces,pageviews columns for test data
test_data$newVisits[is.na(test_data$newVisits)] <- 0
test_data$bounces[is.na(test_data$bounces)] <- 0
test_data$pageviews[is.na(test_data$pageviews)] <- 0


################################ Factor Lumping for categorical data ###########################################3

# This displays the frequency of unique values for categorical columns
# By checking that data, we decided the n value for fctlump() function


# op_gr <- train_d %>% group_by(operatingSystem) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# 
# view(op_gr)
# 
# continent_gr <- train_d %>% group_by(continent) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(continent_gr)
# 
# subcontinent_gr <- train_d %>% group_by(subContinent) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(subcontinent_gr)
# 
# cont_gr <- train_d %>% group_by(country) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(cont_gr)
# 
# re_gr <- train_d %>% group_by(region) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(re_gr)
# 
# met_gr <- train_d %>% group_by(metro) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(met_gr)
# 
# city_gr <- train_d %>% group_by(city) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(city_gr)
# 
# Nd_gr <- train_d %>% group_by(networkDomain) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(Nd_gr)
# 
# Tl_gr <- train_d %>% group_by(topLevelDomain) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(Tl_gr)
# 
# me_gr <- train_d %>% group_by(medium) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(me_gr)
# rp_gr <- train_d %>% group_by(referralPath) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(rp_gr)

# br_gr <- train_d %>% group_by(browser) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(br_gr)
# 
# sr_gr <- train_d %>% group_by(source) %>%
#   summarise(cnts=n()) %>% arrange(desc(cnts))
# view(sr_gr)
######################################################################
# Dealing with categorical attributes
# Performing factor lumping for train data of categorical type
train_d<- train_d %>% 
  mutate(browser=fct_lump(fct_explicit_na(browser),n=5)) %>%
  mutate(operatingSystem = fct_lump(fct_explicit_na(operatingSystem), n = 6)) %>%
  mutate(continent = fct_lump(fct_explicit_na(continent), n=3 )) %>%
  mutate(subContinent = fct_lump(fct_explicit_na(subContinent), n= 10 )) %>%
  mutate(country = fct_lump(fct_explicit_na(country), n= 10)) %>%
  mutate(region = fct_lump(fct_explicit_na(region), n = 4 )) %>%
  mutate(metro = fct_lump(fct_explicit_na(metro),n= 5)) %>%
  mutate(city = fct_lump(fct_explicit_na(city), n=6 )) %>%
  mutate(networkDomain = fct_lump(fct_explicit_na(networkDomain) , n= 5)) %>%
  mutate(topLevelDomain = fct_lump(fct_explicit_na(topLevelDomain), n=3 )) %>%
  mutate(source = fct_lump(fct_explicit_na(source),n=6)) %>%
  mutate(medium = fct_lump(fct_explicit_na(medium), n= 2)) %>%
  mutate(referralPath = fct_lump(fct_explicit_na(referralPath), n=3 )) 

summary(train_d)
view(train_d)  
# Performing factor lumping for test data of categorical type
test_data<- test_data %>% 
  mutate(browser=fct_lump(fct_explicit_na(browser),n=5)) %>%
  mutate(operatingSystem = fct_lump(fct_explicit_na(operatingSystem), n = 6)) %>%
  mutate(continent = fct_lump(fct_explicit_na(continent), n=3 )) %>%
  mutate(subContinent = fct_lump(fct_explicit_na(subContinent), n= 10 )) %>%
  mutate(country = fct_lump(fct_explicit_na(country), n= 10)) %>%
  mutate(region = fct_lump(fct_explicit_na(region), n = 4 )) %>%
  mutate(metro = fct_lump(fct_explicit_na(metro),n= 5)) %>%
  mutate(city = fct_lump(fct_explicit_na(city), n=6 )) %>%
  mutate(networkDomain = fct_lump(fct_explicit_na(networkDomain) , n= 5)) %>%
  mutate(topLevelDomain = fct_lump(fct_explicit_na(topLevelDomain), n=3 )) %>%
  mutate(source = fct_lump(fct_explicit_na(source),n=6)) %>%
  mutate(medium = fct_lump(fct_explicit_na(medium), n= 2)) %>%
  mutate(referralPath = fct_lump(fct_explicit_na(referralPath), n=3 )) 

# Aggregating the data to customer level for train data i.e, by using group_by and summarise functions
final_train <- train_d %>%
  group_by(custId) %>%
  summarise(numofVisits= n(),
            max_visitNumber = max(visitNumber , na.rm=TRUE),
            browser_first = first(browser),
            browser_count=n_distinct(browser),
            first_operatingSystem = first(operatingSystem),
            ismobile_max = max(isMobile),
            first_deviceCategory = first(deviceCategory),
            first_continent = first(continent),
            first_subContinent = first(subContinent),
            first_country= first(country),
            first_region = first(region),
            first_metro=first(metro),
            first_city = first(city),
            first_networkDomain= first(networkDomain),
            first_topleveldomain = first(topLevelDomain),
            first_source = first(source),
            first_medium = first(medium),
            isTrueDirect_max= max(isTrueDirect),
            first_referralPath = first(referralPath),
            avg_pageViews= mean(pageviews),
            sum_pageviews = sum(pageviews),
            max_pageviews = max(pageviews),
            median_pageviews = median(pageviews),
            sum_bounces= sum(bounces),
            max_bounces = max(bounces),
            first_newVisits = first(newVisits),
            avg_revenue = mean(revenue),
            sum_revenue = sum(revenue),
            max_revenue= max(revenue)
            )


# Aggregating the data to customer level for test data i.e, by using group_by and summarise functions
final_test <- test_data %>%
  group_by(custId) %>%
  summarise(numofVisits= n(),
            max_visitNumber = max(visitNumber , na.rm=TRUE),
            browser_first = first(browser),
            browser_count = n_distinct(browser),
            first_operatingSystem = first(operatingSystem),
            ismobile_max = max(isMobile),
            first_deviceCategory = first(deviceCategory),
            first_continent = first(continent),
            first_subContinent = first(subContinent),
            first_country= first(country),
            country_count = n_distinct(country),
            first_region = first(region),
            first_metro=first(metro),
            first_city = first(city),
            first_networkDomain= first(networkDomain),
            first_topleveldomain = first(topLevelDomain),
            first_source = first(source),
            first_medium = first(medium),
            isTrueDirect_max= max(isTrueDirect),
            first_referralPath = first(referralPath),
            avg_pageViews= mean(pageviews),
            sum_pageviews = sum(pageviews),
            max_pageviews = max(pageviews),
            median_pageviews = median(pageviews),
            sum_bounces= sum(bounces),
            max_bounces = max(bounces),
            first_newVisits = first(newVisits))


############################ OLS Model ##################################################

model1 <- lm(formula = log(sum_revenue+1) ~ 
               log(max_visitNumber +1) + browser_first + first_country +
               first_networkDomain +log(numofVisits +1)+ 
               log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) +
               log(sum_bounces+1)+ first_source , data=final_train)

plot(model1)
anova(model1)
summary(model1)
str(final_test)
str(final_train)

train_predict <- predict(model1,final_test)

sqrt(mean(model1$residuals^2))

predRevenue <- predict(model1,final_test)
predRevenue <- replace(predRevenue, which(predRevenue<0), 0)
predicted_result<-tibble(custId=final_test$custId, predRevenue)
write.csv(predicted_result, 'file.csv', row.names = F)

##################################### PLS Model ###########################33
set.seed(1)
pls_model <- plsr(log(sum_revenue+1)~log(max_visitNumber +1) + browser_first + first_country +
                  first_networkDomain +log(max_bounces+1)+log(numofVisits +1)+
                  log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) + 
                  log(sum_bounces+1)+ first_source , data= final_train,scale=TRUE,validation="CV")

plot(pls_model)
pls_sum <- summary(pls_model)
pls_pred <- predict(pls_model,final_test,ncomp = 8)
sqrt(mean(pls_model$residuals^2))

summary(pls_model)$r.squared
predRevenue <- predict(pls_model,final_test,ncomp = 8)
predRevenue <- replace(predRevenue, which(predRevenue<0), 0)

pls_Result<-tibble(custId=final_test$custId, predRevenue)
write.csv(pls_Result, 'submission_pls.csv', row.names = F)


################################ Lasso Model ###########################

set.seed(1)
trnControl <- trainControl(method = "cv",number = 5)

lasso_model <- train(log(sum_revenue+1)~log(max_visitNumber +1) + browser_first + first_country +
                      first_networkDomain +log(max_bounces+1)+log(numofVisits +1)+
                      log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) + 
                      log(sum_bounces+1)+ first_source ,
                     data = final_train,
                     method = "lasso",
                     trControl = trnControl,
                     tuneLength = 5)

model_grid1 <- expand.grid(lambda=seq(0,0.001,length=10),alpha=1)

lasso_model1 <- train(log(sum_revenue+1)~log(max_visitNumber +1) + browser_first + first_country +
                       first_networkDomain +log(max_bounces+1)+log(numofVisits +1)+
                       log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) + 
                       log(sum_bounces+1)+ first_source ,
                     data = final_train,
                     method = "glmnet",
                     trControl = trnControl,
                     tuneGrid=model_grid1,
                     tuneLength = 5)

summary(lasso_model1)

plot(lasso_model1)
plot(lasso_model1$finalModel, xvar = "lambda", label = T)

Lasso_predict <- predict(lasso_model1,final_test)
Lasso_predict <- replace(Lasso_predict, which(Lasso_predict<0), 0)
rmse(log(final_train$sum_revenue+1), Lasso_predict)

predRevenue <- predict(lasso_model1,final_test)
predRevenue <- replace(predRevenue,which(predRevenue<0),0)
lasso_Result<-tibble(custId=final_test$custId, predRevenue)
write.csv(lasso_Result, 'submission_lasso.csv', row.names = F)

########################## Elastic net ############################

model_grid2 <- expand.grid(lambda=seq(0,0.001,length=10),alpha=seq(0.25,0.99 ,length=10))
eln_model <- train(log(sum_revenue+1)~log(max_visitNumber +1) + browser_first + first_country +
                     first_networkDomain +log(max_bounces+1)+log(numofVisits +1)+
                     log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) + 
                     log(sum_bounces+1)+ first_source ,
                   data = final_train,
                   method = "glmnet",
                   trControl = trnControl,
                   tuneGrid=model_grid2,
                   tuneLength = 5)

plot(eln_model)
plot(eln_model$finalModel, xvar = "lambda", label = T)
eln_predict <- predict(eln_model,final_test)
eln_predict <- replace(eln_predict,which(eln_predict<0),0)
rmse(log(final_train$sum_revenue+1),eln_predict )

predRevenue <- predict(eln_model,final_test)
predRevenue <- replace(predRevenue,which(predRevenue<0),0)
eln_Result<-tibble(custId=final_test$custId, predRevenue)
write.csv(eln_Result, 'submission_en.csv', row.names = F)


###################################### Ridge Regression ########################################3333

set.seed(1)
trncontrol = trainControl(method ="cv", number = 5)

ridge_grid=expand.grid(alpha=0,lambda=seq(0,0.001,length=10))
ridge_model = train(log(sum_revenue+1)~log(max_visitNumber +1) + browser_first + first_country +
                      first_networkDomain +log(numofVisits +1)+
                      log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) +
                      log(sum_bounces+1)+ first_source , data=final_train,
                    method="glmnet",
                    trControl=trncontrol,
                    tuneGrid=ridge_grid,
                    tuneLength=5)
plot(ridge_model)
plot(ridge_model$finalModel, xvar = "lambda", label = T)
ridge_predict <- predict(ridge_model,final_test)
ridge_predict <- replace(ridge_predict,which(ridge_predict<0),0)
rmse(log(final_train$avg_revenue+1),ridge_predict)

predRevenue <- predict(ridge_model,final_test)
predRevenue <- replace(predRevenue,which(predRevenue<0),0)
ridge_Result<-tibble(custId=final_test$custId, predRevenue)
write.csv(ridge_Result, 'submission_ridge.csv', row.names = F)

####################################### Mars #####################################################3

mars <- earth(log(sum_revenue+1)~log(max_visitNumber +1) + browser_first + first_country +
                first_networkDomain +log(sum_bounces+1)+log(numofVisits +1)+
                log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) +
                first_source +log(ismobile_max+1)+
                log(isTrueDirect_max+1), data=final_train,
                degree=3,
                nk=25,
                pmethod="cv",
                nfold=5,
                ncross=5)

plot(mars)
mars_predict <- predict(mars,final_test)
mars_Predict <- replace(mars_predict, which(mars_predict<0), 0)
sqrt(mean(mars$residuals^2))

predRevenue <- predict(mars,final_test)
predRevenue <- replace(predRevenue,which(predRevenue<0),0)
mars_result <- tibble(custId=final_test$custId, predRevenue)
write.csv(mars_result, 'submission_mars.csv', row.names = F)


######################################## decision tree #############################################


fit1 <- rpart(log(sum_revenue+1)~log(max_visitNumber +1) + browser_first + first_country +
                first_networkDomain +log(max_bounces+1)+log(numofVisits +1)+
                log(1+avg_pageViews) + log(1+sum_pageviews) + log(1+max_pageviews) +
                log(sum_bounces+1)+ first_source , data= final_train )
fitTreeParty<-as.party(fit1)
plot(fitTreeParty)
pred = predict(fit1,final_test )
pred <- replace(pred, which(pred<0), 0)
sqrt(mean(fit1$residuals^2))

predRevenue <- predict(fit1,final_test)
predRevenue <- replace(predRevenue,which(predRevenue<0),0)
dt_result <- tibble(custId=final_test$custId, predRevenue)
write.csv(dt_result, 'submission_dt.csv', row.names = F)

########################################## SVM ##################################################

modelsvm = svm(log(sum_revenue+1)~ ., data= final_train)

plot(modelsvm)

svm_predict <- predict(modelsvm,final_test)
svm_predict <- replace(svm_predict, which(svm_predict<0), 0)
sqrt(mean(modelsvm$residuals^2))
rmse(log(final_train$sum_revenue+1), svm_predict)


predRevenue <- predict(modelsvm,final_test)
predRevenue <- replace(predRevenue,which(predRevenue<0),0)
svm_result <- tibble(custId=final_test$custId, predRevenue)
write.csv(svm_result, 'submit_svm.csv', row.names = F)

       