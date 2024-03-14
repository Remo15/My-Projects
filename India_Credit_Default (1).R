##########################################
############India Credit Default##########
##########################################

library(ggplot2)
library(readxl)
library(car)
library(corrplot)
library(caTools)
library(ROCR)
library(ineq)
library(caret)
library(blorr)

getwd()
setwd("C:/2020/Finance_Risk_Analytics/Assignment")
# Import the Train dataset, we will convert empty cells as NA while importing
data1 <- read_excel("raw-data.xlsx")
View(data1)
str(data1)
dim(data1)
head(data1)
tail(data1)
names(data1)
#Change NA to resembele blank-NA
data1[data1=="NA"] <- NA
sapply(data1, function(x) {sum(is.na(x))})

str(data1)


# Remove Deposits as it is empty and Num columns as it is just serial numbers
data1 <- data1[c(-1,-22)]

# Convert the below  8 character variables to numeric
#Creditors turnover, Debtors turnover, Finished goods turnover, WIP turnover, Raw material turnover , 
#Shares outstanding,Equity face value, PE on BSE
data1$`Creditors turnover`<- as.numeric(data1$`Creditors turnover`)
data1$`Debtors turnover` <- as.numeric(data1$`Debtors turnover`)
data1$`Finished goods turnover`<- as.numeric(data1$`Finished goods turnover`)
data1$`WIP turnover` <- as.numeric(data1$`WIP turnover`)
data1$`Raw material turnover`<- as.numeric(data1$`Raw material turnover`)
data1$`Shares outstanding` <- as.numeric(data1$`Shares outstanding`)
data1$`Equity face value`<- as.numeric(data1$`Equity face value`)
data1$`PE on BSE`<- as.numeric(data1$`PE on BSE`)

#Checking the structure of variables again
str(data1)

#Creating the Target variable from Networth Next Year
data1$Default <- ifelse(data1$`Networth Next Year` >= 0, 0, 1)
data1$Default<- as.factor(data1$Default)
View(data1)
names(data1)

#Check data summary
attach(data1)
summary(data1)
#Checking for Outliers
boxplot(data1)
sapply(data1[c(-1,-51)], function(x) {length(boxplot.stats(x)$out)})

# Perform Outlier Treatment
# Outlier function

Outlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

# Apply Outlier functions to all columns
data1$`Total assets` <- Outlier(data1$`Total assets`)  
data1$`Net worth` <- Outlier(data1$`Net worth`) 
data1$`Total income`<- Outlier(data1$`Total income`) 
data1$`Change in stock`<- Outlier(data1$`Change in stock`) 
data1$`Total expenses`<- Outlier(data1$`Total expenses`) 
data1$`Profit after tax`<- Outlier(data1$`Profit after tax`)             
data1$PBDITA<- Outlier(data1$PBDITA)             
data1$PBT<- Outlier(data1$PBT) 
data1$`Cash profit`<- Outlier(data1$`Cash profit`)   
data1$`PBDITA as % of total income`<- Outlier(data1$`PBDITA as % of total income`) 
data1$`PBT as % of total income`<- Outlier(data1$`PBT as % of total income`) 
data1$`PAT as % of total income`<- Outlier(data1$`PAT as % of total income`) 
data1$`Cash profit as % of total income`<- Outlier(data1$`Cash profit as % of total income`) 
data1$`PAT as % of net worth`<- Outlier(data1$`PAT as % of net worth`) 
data1$Sales<- Outlier(data1$Sales) 
data1$`Income from financial services`<- Outlier(data1$`Income from financial services`) 
data1$`Other income`<- Outlier(data1$`Other income`) 
data1$`Total capital`<- Outlier(data1$`Total capital`) 
data1$`Reserves and funds`<- Outlier(data1$`Reserves and funds`) 
data1$Borrowings<- Outlier(data1$Borrowings) 
data1$`Current liabilities & provisions`<- Outlier(data1$`Current liabilities & provisions`)
data1$`Deferred tax liability`<- Outlier(data1$`Deferred tax liability`) 
data1$`Shareholders funds`<- Outlier(data1$`Shareholders funds`) 
data1$`Cumulative retained profits`<- Outlier(data1$`Cumulative retained profits`) 
data1$`Capital employed`<- Outlier(data1$`Capital employed`) 
data1$`TOL/TNW`<- Outlier(data1$`TOL/TNW`) 
data1$`Total term liabilities / tangible net worth`<- Outlier(data1$`Total term liabilities / tangible net worth`) 
data1$`Contingent liabilities / Net worth (%)`<- Outlier(data1$`Contingent liabilities / Net worth (%)`) 
data1$`Contingent liabilities`<- Outlier(data1$`Contingent liabilities`) 
data1$`Net fixed assets`<- Outlier(data1$`Net fixed assets`) 
data1$Investments<- Outlier(data1$Investments) 
data1$`Current assets`<- Outlier(data1$`Current assets`) 
data1$`Net working capital`<- Outlier(data1$`Net working capital`) 
data1$`Quick ratio (times)`<- Outlier(data1$`Quick ratio (times)`) 
data1$`Current ratio (times)`<- Outlier(data1$`Current ratio (times)`) 
data1$`Debt to equity ratio (times)`<- Outlier(data1$`Debt to equity ratio (times)`) 
data1$`Cash to current liabilities (times)`<- Outlier(data1$`Cash to current liabilities (times)`) 
data1$`Cash to average cost of sales per day`<- Outlier(data1$`Cash to average cost of sales per day`) 
data1$`Creditors turnover`<- Outlier(data1$`Creditors turnover`) 
data1$`Debtors turnover`<- Outlier(data1$`Debtors turnover`) 
data1$`Finished goods turnover`<- Outlier(data1$`Finished goods turnover`) 
data1$`WIP turnover`<- Outlier(data1$`WIP turnover`) 
data1$`Raw material turnover`<- Outlier(data1$`Raw material turnover`) 
data1$`Shares outstanding`<- Outlier(data1$`Shares outstanding`) 
data1$`Equity face value`<- Outlier(data1$`Equity face value`) 
data1$`EPS`<- Outlier(data1$`EPS`) 
data1$`Adjusted EPS`<- Outlier(data1$`Adjusted EPS`) 
data1$`Total liabilities`<- Outlier(data1$`Total liabilities`) 
data1$`PE on BSE`<- Outlier(data1$`PE on BSE`)
#######################################################

boxplot.stats(data1$`Change in stock`)$out
boxplot(data1$`Change in stock`)
boxplot(data1$`PE on BSE`)

##########################################################
#Remove Next Worth Next Year Column
data1 <- data1[-1]
names(data1)

#Perform Missing Value Treatment
#Check for Missing Values (NA)
sapply(data1,function(x) sum(is.na(x)))

# Creating a for loop to change all NA values to the mean value of that column
# for(i in 1:49)
#   {
#   data1[is.na(data1[,i]), i] <- mean(data1[,i], na.rm = TRUE)
# }

#Missing Value treatment by changing NA to mean of the data
data1$`Change in stock`[which(is.na(data1$`Change in stock`))]  <- mean(data1$`Change in stock`,na.rm=T)
data1$`Total income`[which(is.na(data1$`Total income`))]  <- mean(data1$`Total income`,na.rm=T)

data1$`Change in stock`[which(is.na(data1$`Change in stock`))]  <- mean(data1$`Change in stock`,na.rm=T)

data1$`Total expenses`[which(is.na(data1$`Total expenses`))]  <- mean(data1$`Total expenses`,na.rm=T)

data1$`Profit after tax`[which(is.na(data1$`Profit after tax`))]  <- mean(data1$`Profit after tax`,na.rm=T)

data1$PBDITA[which(is.na(data1$PBDITA))]  <- mean(data1$PBDITA,na.rm=T)

data1$PBT[which(is.na(data1$PBT))]  <- mean(data1$PBT,na.rm=T)

data1$`Cash profit`[which(is.na(data1$`Cash profit`))]  <- mean(data1$`Cash profit`,na.rm=T)

data1$`PBDITA as % of total income`[which(is.na(data1$`PBDITA as % of total income`))]  <- mean(data1$`PBDITA as % of total income`,na.rm=T)

data1$`PBT as % of total income`[which(is.na(data1$`PBT as % of total income`))]  <- mean(data1$`PBT as % of total income`,na.rm=T)

data1$`PAT as % of total income`[which(is.na(data1$`PAT as % of total income`))]  <- mean(data1$`PAT as % of total income`,na.rm=T)

data1$`Cash profit as % of total income`[which(is.na(data1$`Cash profit as % of total income`))]  <- mean(data1$`Cash profit as % of total income`,na.rm=T)

data1$Sales[which(is.na(data1$Sales))]  <- mean(data1$Sales,na.rm=T)

data1$`Income from financial services`[which(is.na(data1$`Income from financial services`))]  <- mean(data1$`Income from financial services`,na.rm=T)

data1$`Other income`[which(is.na(data1$`Other income`))]  <- mean(data1$`Other income`,na.rm=T)

data1$`Total capital`[which(is.na(data1$`Total capital`))]  <- mean(data1$`Total capital`,na.rm=T)

data1$`Reserves and funds`[which(is.na(data1$`Reserves and funds`))]  <- mean(data1$`Reserves and funds`,na.rm=T)

data1$Borrowings[which(is.na(data1$Borrowings))]  <- mean(data1$Borrowings,na.rm=T)

data1$`Current liabilities & provisions`[which(is.na(data1$`Current liabilities & provisions`))]  <- mean(data1$`Current liabilities & provisions`,na.rm=T)

data1$`Deferred tax liability`[which(is.na(data1$`Deferred tax liability`))]  <- mean(data1$`Deferred tax liability`,na.rm=T)

data1$`Cumulative retained profits`[which(is.na(data1$`Cumulative retained profits`))]  <- mean(data1$`Cumulative retained profits`,na.rm=T)

data1$`Contingent liabilities`[which(is.na(data1$`Contingent liabilities`))]  <- mean(data1$`Contingent liabilities`,na.rm=T)

data1$`Net fixed assets`[which(is.na(data1$`Net fixed assets`))]  <- mean(data1$`Net fixed assets`,na.rm=T)

data1$Investments[which(is.na(data1$Investments))]  <- mean(data1$Investments,na.rm=T)

data1$`Current assets`[which(is.na(data1$`Current assets`))]  <- mean(data1$`Current assets`,na.rm=T)

data1$`Net working capital`[which(is.na(data1$`Net working capital`))]  <- mean(data1$`Net working capital`,na.rm=T)

data1$`Quick ratio (times)`[which(is.na(data1$`Quick ratio (times)`))]  <- mean(data1$`Quick ratio (times)`,na.rm=T)

data1$`Current ratio (times)`[which(is.na(data1$`Current ratio (times)`))]  <- mean(data1$`Current ratio (times)`,na.rm=T)

data1$`Cash to current liabilities (times)`[which(is.na(data1$`Cash to current liabilities (times)`))]  <- mean(data1$`Cash to current liabilities (times)`,na.rm=T)

data1$`Cash to average cost of sales per day`[which(is.na(data1$`Cash to average cost of sales per day`))]  <- mean(data1$`Cash to average cost of sales per day`,na.rm=T)

data1$`Creditors turnover`[which(is.na(data1$`Creditors turnover`))]  <- mean(data1$`Creditors turnover`,na.rm=T)

data1$`Debtors turnover`[which(is.na(data1$`Debtors turnover`))]  <- mean(data1$`Debtors turnover`,na.rm=T)

data1$`Finished goods turnover`[which(is.na(data1$`Finished goods turnover`))]  <- mean(data1$`Finished goods turnover`,na.rm=T)

data1$`WIP turnover`[which(is.na(data1$`WIP turnover`))]  <- mean(data1$`WIP turnover`,na.rm=T)

data1$`Raw material turnover`[which(is.na(data1$`Raw material turnover`))]  <- mean(data1$`Raw material turnover`,na.rm=T)

data1$`Equity face value`[which(is.na(data1$`Equity face value`))]  <- mean(data1$`Equity face value`,na.rm=T)

data1$`Adjusted EPS`[which(is.na(data1$`Adjusted EPS`))]  <- mean(data1$`Adjusted EPS`,na.rm=T)

data1$`PE on BSE`[which(is.na(data1$`PE on BSE`))]  <- mean(data1$`PE on BSE`,na.rm=T)

data1$`Shares outstanding`[which(is.na(data1$`Shares outstanding`))]  <- mean(data1$`Shares outstanding`,na.rm=T)
#Check again for Missing values
sapply(data1,function(x) sum(is.na(x)))


#New Variables Creation (One ration for profitability, leverage, liquidity and company's size each )

#Liquidity - Current Ratio = Current Assets/Current Liabilities
data1$`Current Ratio` = data1$`Current assets`/data1$`Current liabilities & provisions`

#Profitability - Profit Margin = Profit after tax /sales
data1$`Profit Margin` = data1$`Profit after tax`/data1$Sales

#Leverage - Borrowings Ratio = Borrowings/Total Liabilities
data1$`Borrowings Ratio` = data1$Borrowings/data1$`Total liabilities`

#Company Size - Net Fixed Assets to Net worth ratio = Fixed Assets/Net Worth
data1$`Assets_Worth Ratio` = data1$`Net fixed assets`/data1$`Net worth`

#Converting Asset Worth Ratio of  values -zero & Inf to the median value of the group
data1$`Assets_Worth Ratio`[data1$`Assets_Worth Ratio`==0] <- median(data1$`Assets_Worth Ratio`)
data1$`Assets_Worth Ratio`[data1$`Assets_Worth Ratio`==Inf] <- median(data1$`Assets_Worth Ratio`)

sum(is.nan(data1$`Assets_Worth Ratio`))
names(data1)
head(data1)
str(data1)

#Check for multicollinearity
cor(data1[,-c(50)])
corrplot(cor(data1[,-c(50)]))
#correlation=corrplot(cor(train[,-c(1,3)]),method = "circle",type = "upper")
corrplot(cor(data1[,-c(50)]),method = "circle",type = "upper")

# data1$`Assets_Worth Ratio`[data1$`Assets_Worth Ratio`==0]
# min(data1$`Assets_Worth Ratio`)

#Uni-Variate Analysis
#####################################
library(purrr)
library(tidyr)
library(ggplot2)

#Histogram
data1 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
####################################
#Boxplot
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot()
###################################
## Bi-Variate
#Below are the significant variables that we got from LR o/p for which we will be doing bi-variate analysis

#Change in stock`,`Cash profit as % of total income`,`PAT as % of net worth`,
#`Total capital`,`Cumulative retained profits`,`TOL/TNW`+`Contingent liabilities`,
#Investments,`Current ratio (times)`,`Cash to current liabilities (times)`,`Adjusted EPS`,
#`PE on BSE` , `Assets_Worth Ratio`    

#ggplot(data2, aes(x=Default), y=(data2$`Shares outstanding`)))
#geom_bar(stat="identity", width=0.5)

ggplot(data1, aes(`Change in stock`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Cash profit as % of total income`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`PAT as % of net worth`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Total capital`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Cumulative retained profits`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`TOL/TNW`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Contingent liabilities`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Investments`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Current ratio (times)`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Cash to current liabilities (times)`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Adjusted EPS`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`PE on BSE`, fill= Default)) + geom_density(alpha=0.5)
ggplot(data1, aes(`Assets_Worth Ratio`, fill= Default)) + geom_density()+ scale_x_continuous(limits = c(0, 125)) 


#####################################
#Build Logistic regression Model
###################################
# Remove Variable - Total Liabilities as it is same as Total assets
data1 <- data1[, -48]
TrainModel=glm(Default~ .,data=data1,family = binomial)
summary(TrainModel)
install.packages("car")
library(car)
library(caret)

vif(TrainModel)

#Variables that are significant
#`Change in stock`,  PBDITA, `Cash profit`, `PBDITA as % of total income`,`Cash profit as % of total income`,
#`PAT as % of net worth`, `Income from financial services`, `Other income`,
#`Total capital`, `Reserves and funds` Borrowings, `Current liabilities & provisions`,  `Deferred tax liability`,
#`Cumulative retained profits`, `TOL/TNW`, `Total term liabilities / tangible net worth`,
#`Contingent liabilities / Net worth (%)`, `Contingent liabilities`, `Net fixed assets` , Investments,
#`Net working capital` ,`Quick ratio (times)`, `Current ratio (times)` , `Cash to current liabilities (times)` ,
#`Cash to average cost of sales per day`,`Creditors turnover`, `Debtors turnover` , `Finished goods turnover`,
#`WIP turnover`,`Raw material turnover` ,`Shares outstanding` , `Equity face value`,`Adjusted EPS`,`PE on BSE` 
# `Current Ratio`,`Profit Margin` , `Borrowings Ratio`,`Assets_Worth Ratio` 

TrainModel_2 = glm(Default~ `Change in stock`+PBDITA+`Cash profit`+`PBDITA as % of total income`+
`Cash profit as % of total income`+`PAT as % of net worth`+`Income from financial services`+`Other income`+
`Total capital`+`Reserves and funds`+Borrowings+`Current liabilities & provisions`+`Deferred tax liability`+
`Cumulative retained profits`+`TOL/TNW`+`Total term liabilities / tangible net worth`+
`Contingent liabilities / Net worth (%)`+`Contingent liabilities`+`Net fixed assets`+Investments+
`Net working capital`+`Quick ratio (times)`+`Current ratio (times)`+`Cash to current liabilities (times)`+
`Cash to average cost of sales per day`+`Creditors turnover`+`Debtors turnover`+`Finished goods turnover`+
`WIP turnover`+`Raw material turnover`+`Shares outstanding`+`Equity face value`+`Adjusted EPS`+`PE on BSE`+ 
`Current Ratio`+`Profit Margin`+`Borrowings Ratio`+`Assets_Worth Ratio`,
data=data1,family = binomial)

summary(TrainModel_2)

#Iteration3 with the below Significant Variables
#`Change in stock`, `Cash profit as % of total income`, `PAT as % of net worth` , `Total capital` ,
#`Cumulative retained profits`  , `TOL/TNW`  ,`Contingent liabilities` , Investments , `Current ratio (times)`  
#`Cash to current liabilities (times)`  , `Adjusted EPS` , `PE on BSE` , `Assets_Worth Ratio` 
TrainModel_3 = glm(Default~`Change in stock`+`Cash profit as % of total income`+`PAT as % of net worth`+
                    `Total capital`+`Cumulative retained profits`+`TOL/TNW`+`Contingent liabilities`+
                    Investments+`Current ratio (times)`+`Cash to current liabilities (times)`+`Adjusted EPS`+
                    `PE on BSE` + `Assets_Worth Ratio`,
                  data=data1,family = binomial)
summary(TrainModel_3)
vif(TrainModel_3)

##Model Performance
#Checking the train dataset distribution of Default & Non-default
summary(data1$Default)
#Making Predictions with train data-
pred_train = predict(TrainModel_3, newdata= data1, type="response")
table(pred_train>0.5,data1$Default)

#Accuracy = (3269+114)/(3269+120+38+114) = 95.5%
#Sensitivity = (114)/(120+114)=48.7%
#Specificity = (3269)/(3269+38) = 98.8%

library(ROCR)
library(blorr)
ROCRpred = prediction(pred_train, data1$Default)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf,col="black",lty=2, lwd=2)
plot(perf,lwd=3,colorize = TRUE)

data1$PredScore<- predict(TrainModel_3,
                              newdata=data1, type = "response")

View(data1)

data1$PredClass = ifelse(data1$PredScore>0.5,1,0)

#Plotting ROC Curve- 1
pred_ratio_train <- prediction(data1$PredScore, data1$Default)
perf_ratio_train <- performance(pred_ratio_train, "tpr", "fpr")
plot(perf_ratio_train,main = "ROC curve")

##KS
KS_train = max(perf_ratio_train@y.values[[1]]-perf_ratio_train@x.values[[1]])
KS_train

k <- blr_gains_table(TrainModel_3)
plot(k)

blr_confusion_matrix(TrainModel_3, data = data1)

blr_gini_index(TrainModel_3, data = data1)

blr_ks_chart(k, title = "KS Chart",
             yaxis_title = " ",xaxis_title = "Cumulative Population %",
             ks_line_color = "black")

blr_decile_lift_chart(k, xaxis_title = "Decile",
                      yaxis_title = "Decile Mean / Global Mean",
                      title = "Decile Lift Chart",
                      bar_color = "blue", text_size = 3.5,
                      text_vjust = -0.3)

blr_decile_capture_rate(k, xaxis_title = "Decile",
                        yaxis_title = "Capture Rate",
                        title = "Capture Rate by Decile",
                        bar_color = "blue", text_size = 3.5,
                        text_vjust =-0.3)


blr_roc_curve(k, title = "ROC Curve",
              xaxis_title = "1 - Specificity",
              yaxis_title = "Sensitivity",roc_curve_col = "blue",
              diag_line_col = "red", point_shape = 18,
              point_fill = "blue", point_color = "blue",
              plot_title_justify = 0.5)  

# blr_rsq_mcfadden(LRmodel4)
# blr_rsq_mcfadden_adj(LRmodel4)
# 
# 
# blr_plot_difchisq_fitted(LRmodel4, point_color = "blue",
#                          title = "Delta Chi Square vs Fitted Values Plot",
#                          xaxis_title = "Fitted Values",
#                          yaxis_title = "Delta Chi Square")

###############################################################################
#Making Predictions with validate/test data-

setwd("C:/2020/Finance_Risk_Analytics/Assignment")
# Import the Train dataset, we will convert empty cells as NA while importing
data2 <- read_excel("validation_data.xlsx")
View(data2)
str(data2)
dim(data2)
head(data2)
tail(data2)
names(data2)

#Convert all variables except target variable to numeric
data2$`Creditors turnover`<- as.numeric(data2$`Creditors turnover`)
data2$`Debtors turnover`<- as.numeric(data2$`Debtors turnover`)
data2$`Finished goods turnover`<- as.numeric(data2$`Finished goods turnover`)
data2$`WIP turnover`<- as.numeric(data2$`WIP turnover`)
data2$`Raw material turnover`<- as.numeric(data2$`Raw material turnover`)
data2$`Shares outstanding`<- as.numeric(data2$`Shares outstanding`)
data2$`Equity face value`<- as.numeric(data2$`Equity face value`)
data2$`PE on BSE`<- as.numeric(data2$`PE on BSE`)

str(data2)
#Convert Default variable to a factor
data2$`Default - 1`<- as.factor(data2$`Default - 1`)

#Create a new variable - `Assets_Worth Ratio` 
#Company Size - Net Fixed Assets to Net worth ratio = Fixed Assets/Net Worth
data2$`Assets_Worth Ratio` = data2$`Net fixed assets`/data2$`Net worth`

#Converting Asset Worth Ratio of  values -zero & Inf to the median value of the group
data2$`Assets_Worth Ratio`[data2$`Assets_Worth Ratio`==0] <- median(data2$`Assets_Worth Ratio`)
data2$`Assets_Worth Ratio`[data2$`Assets_Worth Ratio`==Inf] <- median(data2$`Assets_Worth Ratio`)

#Create the dataset with variables that we selected for train dataset
data_test <- data2[c("Default - 1" ,"Cash profit as % of total income","PAT as % of net worth", "Total capital", 
           "Cumulative retained profits", "Contingent liabilities", "Current ratio (times)", 
           "Adjusted EPS", "PE on BSE","Change in stock","TOL/TNW","Investments",
          "Cash to current liabilities (times)", "Assets_Worth Ratio")]

names(data2)
names(data_test)
#Check for NA's
sapply(data_test, function(x) {sum(is.na(x))})
#Change NA to resembele blank-NA
data2[data2=="NA"] <- NA

#Convert NA's to mean value
data_test$`Cash profit as % of total income`[which(is.na(data_test$`Cash profit as % of total income`))]  <- mean(data_test$`Cash profit as % of total income`,na.rm=T)
data_test$`Total capital`[which(is.na(data_test$`Total capital`))]  <- mean(data_test$`Total capital`,na.rm=T)
data_test$`Cumulative retained profits`[which(is.na(data_test$`Cumulative retained profits`))]  <- mean(data_test$`Cumulative retained profits`,na.rm=T)
data_test$`Contingent liabilities`[which(is.na(data_test$`Contingent liabilities`))]  <- mean(data_test$`Contingent liabilities`,na.rm=T)
data_test$`Current ratio (times)`[which(is.na(data_test$`Current ratio (times)`))]  <- mean(data_test$`Current ratio (times)`,na.rm=T)
data_test$`PE on BSE`[which(is.na(data_test$`PE on BSE`))]  <- mean(data_test$`PE on BSE`,na.rm=T)
data_test$`Change in stock`[which(is.na(data_test$`Change in stock`))]  <- mean(data_test$`Change in stock`,na.rm=T)
data_test$`Investments`[which(is.na(data_test$`Investments`))]  <- mean(data_test$`Investments`,na.rm=T)
data_test$`Cash to current liabilities (times)`[which(is.na(data_test$`Cash to current liabilities (times)`))]  <- mean(data_test$`Cash to current liabilities (times)`,na.rm=T)
data_test$`Assets_Worth Ratio`[which(is.na(data_test$`Assets_Worth Ratio`))]  <- mean(data_test$`Assets_Worth Ratio`,na.rm=T)


sapply(data_test, function(x) {sum(is.na(x))})

str(data_test)

#Predictions for validatipn dataset
# we get probablity using predict function
data_test$PredScore<- predict(TrainModel_3,
                                  newdata=data_test, type = "response")

View(data_test)

data_test$PredClass = ifelse(data_test$PredScore>0.5,1,0)

pred_ratio_test <- prediction(data_test$PredScore, data_test$`Default - 1`)
perf_ratio_test <- performance(pred_ratio_test, "tpr", "fpr")
plot(perf_ratio_test,main = "ROC curve")

confusionMatrix(data_test$PredClass, data_test$`Default - 1`)
#Confusion Matrix
table(data_test$PredClass, data_test$`Default - 1`)
#Accuracy = (625+39)/(625+15+36+39) = 92.8%
#Sensitivity = (39)/(39+15)=72.2%
#Specificity = (625)/(625+36) = 94.5%

library(ineq)
##KS

KS_test = max(perf_ratio_test@y.values[[1]]-perf_ratio_test@x.values[[1]])

KS_test

auc_test = performance(pred_ratio_test,"auc"); 
auc_test = as.numeric(auc_test@y.values)
auc_test

##Gini Score
gini_test = ineq(data_test$PredClass, type="Gini")

gini_test
############################
#Creating a decile
library(dplyr)
#### decile rank of the column in descending order

data_decile = mutate(data_test, decile_rank = ntile(desc(data_test$PredScore),10))View(data_decile)


# ggplot(data_decile, aes(as.factor(decile_rank),PredScore)) + 
#   geom_col(position = 'dodge')



ggplot(data_decile, aes(decile_rank, PredScore, colour=`Default - 1`, group=`Default - 1`)) +
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.y=mean, geom="point") +
  expand_limits(y=0) +
  theme_bw() +
  scale_x_continuous(limits = c(1, 10,1))
