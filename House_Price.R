
#data <- iris
create_report(data)
#Install packages
install.packages("DataExplorer")
#install.packages("readxl")
#install.packages("ggplot2")
install.packages("zipcodeR")
install.packages("esquisse")
library(GGally)

#Load libraries
library(ggplot2)
library(readxl)
library(DataExplorer)
library(zipcodeR)
library(esquisse)
library(corrplot)
library(caTools)
library(GGally)
library(caret)
library(caTools) # for sample.split()

# Set the working directory and check it
setwd("C:/2020/Capstone/House Price")
getwd()
#Import the data
#House_1 <- read_excel("C:/2020/Capstone/House Price/innercity.xls")
House_1 <- read.csv("C:/2020/Capstone/House Price/innercity.csv")
attach(House_1)
nrow(House_1)
names(House_1)
str(House_1)
summary(House_1)
View(House_1)

#Examine the data
#create_report(House_1)

#Using datset with zipcode mapped to city
?zipcode
data(zip_code_db)
zip_code_db
attach(zip_code_db)

#Merging House Dataset and Zipcode databse
export_tab <- merge(x=House_1,y=zip_code_db,by.x=c("zipcode"),by.y=c("zipcode"), all.x=TRUE)
View(export_tab)
names(export_tab)

House_2 <- export_tab[,c(1:23,25)]
View(House_2)

# Missing value Treatment
#Checking for Missing values(NA) in each of the columns
apply(House_2,2,function(x) sum(is.na(x)))
sum(is.na(House_2))
5/21000

#Checking NA's
apply(House_1,2,function(x) sum(is.na(x)))
sum(apply(House_1,2,function(x) sum(is.na(x))))
apply(House_2,2,function(x) sum(is.na(x)))

#Converting nulls & dollars to NA and removing NA
House_2[House_2 == ""]<-NA
House_2[House_2 == "$"]<-NA
House_2 <- na.omit(House_2)

View(House_2)

#Extracting Sold Year (Variable Transformation)
House_2$yr_sold = substr(House_2$dayhours,1,4)
str(House_2)

#Converting the variables

House_2$yr_built = as.character(House_2$yr_built)
House_2$yr_built = as.numeric(House_2$yr_built)
House_2$yr_sold = as.numeric(House_2$yr_sold)

#Create new variable Age
House_2$house_age = (House_2$yr_sold - House_2$yr_built)
View(House_2)

#Create a cat variable for Year renovated
House_2$renovated = ifelse(House_2$yr_renovated==0,0,1)

# Converting negative house age to zero
House_2$house_age[House_2$house_age < 0] <- 0

# House_2$total_area = as.numeric(House_2$total_area)
# House_2$furnished = as.numeric(House_2$furnished)
# House_2$long = as.numeric(House_2$long)
# House_2$coast = as.numeric(House_2$coast)
# House_2$condition = as.numeric(House_2$condition)
# House_2$ceil = as.numeric(House_2$ceil)

View(House_2)
names(House_2)
str(House_2)

#Creating new variable perc_carpert_area
House_2$perc_carpet_area = round(House_2$living_measure15/House_2$lot_measure15,2)

# Changing yr_renovated to Renovated - Yes/No (1/0)- variable Transformation
#House_2$renovated = House_2$yr_renovated

House_3 <- House_2[-c(17)]
View(House_3)
str(House_3)

#Basement Treatment - Variable Transformation
mean(House_3$basement) # 290, 
# Average without zeroes -> 750
median(House_3$basement)
quantile(House_3$basement)
quantile(House_3$basement, c(.50, .75, .90)) 
perc95
# 0 -> Basement = 0  --> No basement
# 1 -> Basement > 0 & Basement < 750 ---> Below Avg basement
# 2 -> Basement > 750 --> Above Avg Basement

House_3$basement_new = ifelse(House_3$basement == 0,0,ifelse(House_3$basement < 750,1,2))  
View(House_3)
names(House_3)

House_3 <- House_3[-c(15)]
House_4 <- House_3
str(House_4)
#Variable type conversion
House_4$room_bed = as.factor(House_4$room_bed)
House_4$room_bath = as.factor(House_4$room_bath)
House_4$sight = as.factor(House_4$sight)
House_4$quality = as.factor(House_4$quality)
House_4$furnished = as.factor(House_4$furnished)
House_4$basement_new = as.factor(House_4$basement_new)
House_4$renovated = as.factor(House_4$renovated)

House_4$total_area = as.numeric(House_4$total_area)
House_4$long = as.numeric(House_4$long)

View(House_4)
# Variables for Outlier Treatment

#length(boxplot.stats(House_3$room_bed)$out)
#length(boxplot.stats(House_3$room_bath)$out)
#length(boxplot.stats(House_3$living_measure)$out)
#length(boxplot.stats(House_3$lot_measure)$out)
#length(boxplot.stats(House_3$basement)$out)
length(boxplot.stats(House_4$price)$out)
length(boxplot.stats(House_4$ceil_measure)$out)
length(boxplot.stats(House_4$living_measure15)$out)
length(boxplot.stats(House_4$lot_measure15)$out)
length(boxplot.stats(House_4$perc_carpet_area)$out)

#For missing values that lie outside the 1.5 * IQR limits, we could cap it by replacing those observations outside the lower limit with the value of 5th %ile and those that lie 
#above the upper limit, with the value of 95th %ile. Below is a sample code that achieves this.

##Outlier Treatment for price variable
qnt <- quantile(House_4$price, probs=c(.25, .75), na.rm = T)
caps <- quantile(House_4$price, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(House_4$price, na.rm = T)

House_4$price[House_4$price < (qnt[1] - H)] <- caps[1]
House_4$price[House_4$price > (qnt[2] + H)] <- caps[2]
summary(House_4$price)
length(boxplot.stats(House_4$price)$out)

##Outlier Treatment for ceil_measure variable
qnt <- quantile(House_4$ceil_measure, probs=c(.25, .75), na.rm = T)
caps <- quantile(House_4$ceil_measure, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(House_4$ceil_measure, na.rm = T)

House_4$ceil_measure[House_4$ceil_measure < (qnt[1] - H)] <- caps[1]
House_4$ceil_measure[House_4$ceil_measure > (qnt[2] + H)] <- caps[2]
summary(House_4$ceil_measure)
length(boxplot.stats(House_4$ceil_measure)$out)

##Outlier Treatment for living_measure15 variable
qnt <- quantile(House_4$living_measure15, probs=c(.25, .75), na.rm = T)
caps <- quantile(House_4$living_measure15, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(House_4$living_measure15, na.rm = T)

House_4$living_measure15[House_4$living_measure15 < (qnt[1] - H)] <- caps[1]
House_4$living_measure15[House_4$living_measure15 > (qnt[2] + H)] <- caps[2]
summary(House_4$living_measure15)
length(boxplot.stats(House_4$living_measure15)$out)

##Outlier Treatment for lot_measure15 variable
qnt <- quantile(House_4$lot_measure15, probs=c(.25, .75), na.rm = T)
caps <- quantile(House_4$lot_measure15, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(House_4$lot_measure15, na.rm = T)

House_4$lot_measure15[House_4$lot_measure15 < (qnt[1] - H)] <- caps[1]
House_4$lot_measure15[House_4$lot_measure15 > (qnt[2] + H)] <- caps[2]
summary(House_4$lot_measure15)
length(boxplot.stats(House_4$lot_measure15)$out)

##Outlier Treatment for perc_carpet_area variable
qnt <- quantile(House_4$perc_carpet_area, probs=c(.25, .75), na.rm = T)
caps <- quantile(House_4$perc_carpet_area, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(House_4$perc_carpet_area, na.rm = T)

House_4$perc_carpet_area[House_4$perc_carpet_area < (qnt[1] - H)] <- caps[1]
House_4$perc_carpet_area[House_4$perc_carpet_area > (qnt[2] + H)] <- caps[2]
summary(House_4$perc_carpet_area)
length(boxplot.stats(House_4$perc_carpet_area)$out)
############################################

#Examine the data after missing value and outlier treatment
# create_report(House_3)
# create_report(House_3,y="price")
# 
# 
# View(House_3)
# write.csv(House_3,file="innercity_3.csv", col.names=TRUE)
# 
# create_report(House_4,y="price")
# write.csv(House_4,file="innercity_4.csv", col.names=TRUE, row.names = FALSE)
#############################################
# Variables that will be omitted:
#zipcode"          "cid"              "dayhours"  
#"basement"         "yr_built"         "lat.x"            "long"
#"yr_sold"
# Final List of Variables:
names(House_3)
#House_4 <- House_3[,c(4:6,9:14,19:23,25:28)]
# names(House_4)
House_4 <- House_4[,c(4:6,9:14,18:22,24:27)]
names(House_4)
str(House_4)

#Examine the variables
create_report(House_4, y="price")
#############################################
# ggplots - Uni Variate, Bi Variate
esquisser(data=House_4)


library(ggplot2)

#Bedroom
ggplot(House_4) +
  aes(x = room_bed, y = price) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(x = "No: of Rooms", y = "Price", title = "No: of Rooms vs Price Boxplot") +
  theme_minimal()

#Bathroom
ggplot(House_4) +
  aes(x = room_bath, y = price) +
  geom_boxplot(fill = "#ed7953") +
  labs(x = "No: of bathrooms", y = "Price", title = "No: of bath rooms Vs Price") +
  theme_minimal()

#Ceil
ggplot(House_4) +
  aes(x = ceil, y = price) +
  geom_boxplot(fill = "#6dcd59") +
  labs(x = "No: of Ceilings", y = "Price", title = "No: of ceilings vs Price") +
  theme_minimal()

#Coast
ggplot(House_4) +
  aes(x = coast, y = price) +
  geom_boxplot(fill = "#000000") +
  labs(x = "Coast", y = "Price", title = "Coast vs price", subtitle = "Coast Yes=1, No=0") +
  theme_minimal()

#Sight
ggplot(House_4) +
  aes(x = sight, y = price) +
  geom_boxplot(fill = "#7301a8") +
  labs(x = "No: of Sights", y = "Price", title = "No: of Sights vs Price") +
  theme_minimal()

#Condition
ggplot(House_4) +
  aes(x = condition, y = price) +
  geom_boxplot(fill = "#a50f15") +
  labs(x = "Condition", y = "Price", title = "Hosue Condition Vs Price") +
  theme_minimal()

#Quality
ggplot(House_4) +
  aes(x = quality, y = price) +
  geom_boxplot(fill = "#f0f921") +
  labs(x = "House_Quality", y = "Price", title = "House Quality Vs Price") +
  theme_minimal()

#Furnished
ggplot(House_4) +
  aes(x = furnished, y = price) +
  geom_boxplot(fill = "#737373") +
  labs(x = "House Furnished ", y = "Price", title = "House Furnished Vs Price", subtitle = "Furnished Yes=1, No=0") +
  theme_minimal()

#Basement_new
ggplot(House_4) +
  aes(x = basement_new, y = price) +
  geom_boxplot(fill = "#4292c6") +
  labs(x = "Basement Area category", y = "Price", title = "Basement Vs Price", subtitle = "No basement area =0, Area below avg=1, Area above avg=2") +
  theme_minimal()

#Living Measure
ggplot(House_4) +
  aes(x = living_measure15, y = price) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  theme_minimal() +
  facet_wrap(vars(major_city))

#Lot Measure
ggplot(House_4) +
  aes(x = lot_measure15, y = price) +
  geom_point(size = 1L, colour = "#d8576b") +
  theme_minimal() +
  facet_wrap(vars(major_city))

#Ceil Measure
ggplot(House_4) +
  aes(x = ceil_measure, y = price) +
  geom_point(size = 1L, colour = "#006d2c") +
  theme_minimal() +
  facet_wrap(vars(major_city))

#House Age
ggplot(House_4) +
  aes(x = house_age, y = price) +
  geom_point(size = 1L, colour = "#fa9e3b") +
  theme_minimal() +
  facet_wrap(vars(major_city))

#Tot Area
ggplot(House_4) +
  aes(x = total_area, y = price) +
  geom_point(size = 1L, colour = "#6dcd59") +
  theme_minimal() +
  facet_wrap(vars(major_city))

#Perc Carpet Area
ggplot(House_4) +
  aes(x = perc_carpet_area, y = price) +
  geom_point(size = 1L, colour = "#bd3786") +
  theme_minimal() +
  facet_wrap(vars(major_city))

#Histogram

ggplot(House_4) +
  aes(x = perc_carpet_area) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

ggplot(House_4) +
  aes(x = ceil_measure) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Price", y = "Frequency", title = "Histogram of Price by City") +
  theme_minimal() +
  facet_wrap(vars(major_city))

ggplot(House_4) +
  aes(x = basement_new, weight = price) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "basement_new Vs price") +
  theme_minimal()
#####################################################
#Removing unwanted variables
# names(House_4)
# 
# House_5 <- House_4[,c(4:6,9:14,18:21,24:27)]
# View(House_5)
# str(House_5)

#Converting all factor variables back to numeric and create a new data set--> House_5
House_5 <- House_4
House_5$room_bed = as.numeric(House_4$room_bed)
House_5$room_bath = as.numeric(House_4$room_bath)
House_5$ceil = as.numeric(House_4$ceil)
House_5$coast = as.numeric(House_4$coast)
House_5$sight = as.numeric(House_4$sight)
House_5$condition = as.numeric(House_4$condition)
House_5$quality = as.numeric(House_4$quality)
House_5$furnished = as.numeric(House_4$furnished)
House_5$renovated = as.numeric(House_4$renovated)
House_5$basement_new = as.numeric(House_4$basement_new)

str(House_5)
create_report(House_5)
create_report(House_5,y="price")
#write.csv(House_5,"House_5.csv")
House_5 <- read.csv("C:/2020/Capstone/House Price/House_5.csv")
House_5 <- House_5[,-1]
##########################################################
####################
#Multi-collinearity Check
install.packages("usdm")
library(usdm)
usdm::vifcor(House_4)
vifcor(House_5[-c(1,14)])

#Model Building- using Multi Linear Regression
library(caret)
#Corrplot
# cor_data=data.frame(train_data[,2:17])
# correlation=cor(cor_data)
# par(mfrow=c(1, 1))
# corrplot(correlation,method="color")
# 
# ggpairs(train_data, columns= 2:ncol(train_data))
# ggpairs(House_5[,c(2:13,15:18)])
#Splitting data into test and train- House_4
 set.seed(123)   #  set seed to ensure you always have same random numbers generated
 sample = sample.split(House_5,SplitRatio = 0.8) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
 train_data =subset(House_5,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
 test_data=subset(House_5, sample==FALSE)
 dim(train_data)
 dim(test_data)
#######################
###################

###################
#LINEAR REGRESSION
###################
LR_fit_1 <- train(price ~ .,data = train_data,method = "lm",preProcess = c("scale", "center"),
                trControl = trainControl(method = "cv",number = 5))

LR_fit_2 <- train(price ~ .,data = train_data,method = "lm",
                  trControl = trainControl(method = "cv",number = 5))


#LR_fit_2
LR_fit_1
summary(LR_fit_1)

#Variable Importance - LR
varImp(LR_fit_1)
attributes(LR_fit_1)
LR_fit_1$finalModel

LR_fit_2=lm(data=train_data_1,price~.)
summary(LR_fit_2)
LR_pred_train_1 <- predict(LR_fit_1, train_data)
LR_pred_test_1 <- predict(LR_fit_1, test_data)
# LR_pred_train_2 <- predict(LR_fit_2, train_data)
# LR_pred_test_2 <- predict(LR_fit_2, test_data)

install.packages("Metrics")
library(Metrics)
mape(train_data$price,LR_pred_train_1)
mape(test_data$price,LR_pred_test_1)
mae(test_data$price,LR_pred_test_1)
rmse(test_data$price,LR_pred_test_1)
R2_LR <- 1 - (sum((test_data$price-LR_pred_test_1)^2)/sum((test_data$price-mean(test_data$price))^2))
R2_LR
# mape(train_data$price,LR_pred_train_2)
# mape(test_data$price,LR_pred_test_2)

##############
#RIDGE REGRESSION
##############
# glmnet() runs the model many times for different values of lambda.
library(glmnet)
ridge_fit_1 <- caret::train(
  form = price ~ .,
  data = train_data,
  method = "glmnet", # method for fit
  tuneGrid = expand.grid(alpha = 0,
  lambda = 2^seq(from = -4, to = 10)), # plug in different values of lambda to see how test RMSE changes),
  trControl = trainControl(method = "cv",number = 5))

ridge_fit_1
ridge_fit_1$bestTune                           
ridge_fit_1$bestTune$lambda
summary(ridge_fit_1)
varImp(ridge_fit_1)

Ridge_pred_train_1 <- predict(ridge_fit_1,newdata= train_data)
Ridge_pred_test_1 <- predict(ridge_fit_1,newdata= test_data)

mape(train_data$price,Ridge_pred_train_1)
mape(test_data$price,Ridge_pred_test_1)

mae(test_data$price,Ridge_pred_test_1)
rmse(test_data$price,Ridge_pred_test_1)
R2_Ridge <- 1 - (sum((test_data$price-Ridge_pred_test_1)^2)/sum((test_data$price-mean(test_data$price))^2))
R2_Ridge
#################
#DECISION TREE
#################
library(rpart)
library(rpart.plot)

dt_fit_1 <- rpart(
  formula = price ~ .,
  data    = train_data,
  method  = "anova"
)

dt_fit_1
rpart.plot(dt_fit_1, cex=0.5)
plotcp(dt_fit_1)
dt_fit_1$cptable

DT_pred_train_1 <- predict(dt_fit_1,newdata= train_data)
DT_pred_test_1 <- predict(dt_fit_1,newdata= test_data)

mape(train_data$price,DT_pred_train_1)
mape(test_data$price,DT_pred_test_1)

rmse(train_data$price,DT_pred_train_1)
rmse(test_data$price,DT_pred_test_1)

mae(train_data$price,DT_pred_train_1)
mae(test_data$price,DT_pred_test_1)

R2_DT_train <- 1 - (sum((train_data$price-DT_pred_train_1)^2)/sum((train_data$price-mean(train_data$price))^2))
R2_DT_test <-  1 - (sum((test_data$price-DT_pred_test_1)^2)/sum((test_data$price-mean(test_data$price))^2))

# R2_DT_train
# R2_DT_test
# m2 <- rpart(
#   formula = price ~ .,
#   data    = train_data,
#   method  = "anova",
#   control = list(minsplit = 20, maxdepth = 10, xval = 10,
#                  cp=0)
# )
# m2$cptable
# pred_train_dt_2 <- predict(m2,newdata= train_data)
# rmse(train_data$price,pred_train_dt_2)
# mape(train_data$price,pred_train_dt_2)
######################
###################
#KNN REGRESSION
###################
knn_fit_1 <- train(price ~ .,data = train_data,method = "knn",preProcess = c("scale", "center"),
                 trControl = trainControl(method = "cv",number = 5))


knn_fit
KNN_pred_train_1

KNN_pred_train_1 <- predict(knn_fit_1, train_data)
KNN_pred_test_1 <- predict(knn_fit_1, test_data)

mape(train_data$price,KNN_pred_train_1)
mape(test_data$price,KNN_pred_test_1)

rmse(test_data$price,KNN_pred_test_1)
mae(test_data$price,KNN_pred_test_1)
R2_KNN_test <-  1 - (sum((test_data$price-KNN_pred_test_1)^2)/sum((test_data$price-mean(test_data$price))^2))
R2_KNN_test

varImp(knn_fit_1)
attributes(knn_fit_1)
knn_fit_1$finalModel
# KNN
# library(dplyr)
# knn_val_pred <- knn_fit$pred %>%
#   dplyr::filter(k == knn_fit$bestTune$k) %>%
#   arrange(rowIndex) %>%
#   pull(pred)
##############

# lm_fit_3 <- train(price ~ .,
#                 data = train_data_1, 
#                 method = "lm")
# 
# lm_fit_3
# bh_pred <- predict(lm_fit_3, train_data_1)
# bh_pred_test <- predict(lm_fit_3, test_data_1)
# install.packages("Metrics")
# library(Metrics)
# mape(train_data_1$price,bh_pred)
# mape(test_data_1$price,bh_pred_test)
# 
# #######################
# install.packages("mlbench")
# library(mlbench)
# postResample(pred = bh_pred, obs = train_data_1$price)
########################

#################################
#Random Forest
#################################
RF_fit_1 <- randomForest(price ~ ., data = train_data)

RF_fit_1

RF_pred_train_1 <- predict(RF_fit_1, train_data)
RF_pred_test_1 <- predict(RF_fit_1, test_data)

mape(train_data$price,RF_pred_train_1)
mape(test_data$price,RF_pred_test_1)

rmse(train_data$price,RF_pred_train_1)
rmse(test_data$price,RF_pred_test_1)

mae(train_data$price,RF_pred_train_1)
mae(test_data$price,RF_pred_test_1)

R2_RF_train <- 1 - (sum((train_data$price-RF_pred_train_1)^2)/sum((train_data$price-mean(train_data$price))^2))
R2_RF_test <-  1 - (sum((test_data$price-RF_pred_test_1)^2)/sum((test_data$price-mean(test_data$price))^2))
R2_RF_train
R2_RF_test

varImp(RF_fit_1)
install.packages("reprtree")
# library(reprtree)
# reprtree:::plot.getTree(RF_fit_1)
# library(randomForest)
# fit3 <- randomForest(price ~ ., data = train_data)
# print(fit3)
# importance(fit3)
# varImpPlot(fit3)
# summary(fit3)
#Tuning the random Forest Model for optimal parameters
tune.RF <- tuneRF(train_data[,-1],train_data[,1],stepFactor=2, improve=0.05,
       trace=TRUE, plot=TRUE)
       
RF_fit_tuned <- randomForest(price ~ ., data = train_data,mtry=10)

RF_fit_tuned
varImp(RF_fit_tuned)
RF_pred_train_tuned <- predict(RF_fit_tuned, train_data)
RF_pred_test_tuned <- predict(RF_fit_tuned, test_data)

mape(train_data$price,RF_pred_train_tuned)
mape(test_data$price,RF_pred_test_tuned)

rmse(train_data$price,RF_pred_train_tuned)
rmse(test_data$price,RF_pred_test_tuned)

mae(train_data$price,RF_pred_train_tuned)
mae(test_data$price,RF_pred_test_tuned)

R2_RF_train_tuned <- 1 - (sum((train_data$price-RF_pred_train_tuned)^2)/sum((train_data$price-mean(train_data$price))^2))
R2_RF_test_tuned <-  1 - (sum((test_data$price-RF_pred_test_tuned)^2)/sum((test_data$price-mean(test_data$price))^2))
R2_RF_train_tuned
R2_RF_test_tuned
##########################
# library("Metrics")
# pred_train_rf <- predict(fit3, newdata= train_data)
# rmse(train_data$price,pred_train_rf)
# R2 <- 1 - (sum((train_data$price-pred_train_rf)^2)/sum((train_data$price-mean(train_data$price))^2))
# #R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
# mae(train_data$price,pred_train_rf)
# mape(train_data$price,pred_train_rf)
# 

# pred_test_rf <- predict(fit3, newdata= test_data)
# mape(test_data$price,pred_test_rf)
# 
########################
#STACKING
########################
#stacking the train output from all models along with train actuals
stacked_train <- cbind(LR_pred_train_1,Ridge_pred_train_1,DT_pred_train_1,KNN_pred_train_1,train_data$price)
stacked_train <- data.frame(stacked_train)
stacked_train["price"] <- stacked_train["V5"]
stacked_train <- stacked_train[,-5]
stacked_train["LR_pred"] <- stacked_train["LR_pred_train_1"]
stacked_train["Ridge_pred"] <- stacked_train["Ridge_pred_train_1"]
stacked_train["DT_pred"] <- stacked_train["DT_pred_train_1"]
stacked_train["KNN_pred"] <- stacked_train["KNN_pred_train_1"]
stacked_train <- stacked_train[,-c(1:4)]
head(stacked_train)
View(stacked_train)
#############
#stacking the test output from all models along with test actuals
stacked_test <- cbind(LR_pred_test_1,Ridge_pred_test_1,DT_pred_test_1,KNN_pred_test_1,test_data$price)
stacked_test <- data.frame(stacked_test)
stacked_test["price"] <- stacked_test["V5"]
stacked_test <- stacked_test[,-5]
stacked_test["LR_pred"] <- stacked_test["LR_pred_test_1"]
stacked_test["Ridge_pred"] <- stacked_test["Ridge_pred_test_1"]
stacked_test["DT_pred"] <- stacked_test["DT_pred_test_1"]
stacked_test["KNN_pred"] <- stacked_test["KNN_pred_test_1"]
stacked_test <- stacked_test[,-c(1:4)]
head(stacked_test)
View(stacked_test)
#############
knn_fit_stack <- train(price ~ .,data = stacked_train,method = "knn",preProcess = c("scale", "center"),
                 trControl = trainControl(method = "cv",number = 5))

knn_fit_stack
varImp(knn_fit_stack)

pred_train_knn_stack <- predict(knn_fit_stack, stacked_train)
pred_test_knn_stack <- predict(knn_fit_stack, stacked_test)

mape(stacked_train$price,pred_train_knn_stack)
mape(stacked_test$price,pred_test_knn_stack)

mae(stacked_test$price,pred_test_knn_stack)
rmse(stacked_test$price,pred_test_knn_stack)
R2_KNN_stack_test <-  1 - (sum((stacked_test$price-pred_test_knn_stack)^2)/sum((stacked_test$price-mean(stacked_test$price))^2))
R2_KNN_stack_test

