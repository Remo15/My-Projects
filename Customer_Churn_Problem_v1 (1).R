
######################################################################
###########################CUSTOMER CHURN SOLUTION########################
######################################################################

#Install packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("caTools")
install.packages("ROCR")
install.packages("ineq")
install.packages("caret")



#Load libraries
library(ggplot2)
library(readxl)
library(corrplot)
library(caTools)
library(caret)
library(blorr)

# Set the working directory and check it
setwd("C:/2020/RProgramming/Predictive_Modeling")
getwd()

#Import the data
Cust_Churn <- read_excel("C:/2020/RProgramming/Predictive_Modeling/Cellphone.xlsx", sheet=2)
View(Cust_Churn)

##Check the dimension or shape of the data
dim(Cust_Churn)
str(Cust_Churn)

##Convert all variables into factors where necessary. 
#library(dplyr) 
Cust_Churn$Churn <- as.factor(Cust_Churn$Churn)
Cust_Churn$ContractRenewal  <- as.factor(Cust_Churn$ContractRenewal)
Cust_Churn$DataPlan  <- as.factor(Cust_Churn$DataPlan)

str(Cust_Churn)
#Perform basic summary 
head(Cust_Churn)
tail(Cust_Churn)
summary(Cust_Churn)

##View top 5 rows
Cust_Churn[1:5,]

##Lets see the variable list in the data and then make changes to the col names
names(Cust_Churn)


#Checking for Missing values9NA) in each of the columns
apply(Cust_Churn,2,function(x) sum(is.na(x)))
sum(is.na(Cust_Churn))

str(Cust_Churn)
## list out all the numeric and categorical variables and their levels
split(names(Cust_Churn),sapply(Cust_Churn, function(x) paste(class(x), collapse=" ")))

##Summary Statistics Measure of central tendency and dispersion (Univariate Analysis)
##(count,missing value,mean,0.01,0.05,0.10,0.25,Median,0.75,0.90,0.95,0.99,min,max,range,skew,kurtosis,
#SD,IQR) for continous variable
names(Cust_Churn)
boxplot(Cust_Churn$AccountWeeks)
boxplot(Cust_Churn$DataUsage)
boxplot(Cust_Churn$DayMins)
boxplot(Cust_Churn$DayCalls)
boxplot(Cust_Churn$MonthlyCharge)
boxplot(Cust_Churn$OverageFee)
boxplot(Cust_Churn$RoamMins)

length(boxplot.stats(Cust_Churn$AccountWeeks)$out)
length(boxplot.stats(Cust_Churn$DataUsage)$out)
length(boxplot.stats(Cust_Churn$DayMins)$out)
length(boxplot.stats(Cust_Churn$DayCalls)$out)
length(boxplot.stats(Cust_Churn$MonthlyCharge)$out)
length(boxplot.stats(Cust_Churn$OverageFee)$out)
length(boxplot.stats(Cust_Churn$RoamMins)$out)

#For missing values that lie outside the 1.5 * IQR limits, we could cap it by replacing those observations outside the lower limit with the value of 5th %ile and those that lie 
#above the upper limit, with the value of 95th %ile. Below is a sample code that achieves this.
##Outlier Treatment for AccountWeeks variable

qnt <- quantile(Cust_Churn$AccountWeeks, probs=c(.25, .75), na.rm = T)
caps <- quantile(Cust_Churn$AccountWeeks, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Cust_Churn$AccountWeeks, na.rm = T)

Cust_Churn$AccountWeeks[Cust_Churn$AccountWeeks < (qnt[1] - H)] <- caps[1]
Cust_Churn$AccountWeeks[Cust_Churn$AccountWeeks > (qnt[2] + H)] <- caps[2]
summary(Cust_Churn$AccountWeeks)
length(boxplot.stats(Cust_Churn$AccountWeeks)$out)

############################################
## Outlier Treatment for DataUsage variable
qnt <- quantile(Cust_Churn$DataUsage, probs=c(.25, .75), na.rm = T)
caps <- quantile(Cust_Churn$DataUsage, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Cust_Churn$DataUsage, na.rm = T)

Cust_Churn$DataUsage[Cust_Churn$DataUsage < (qnt[1] - H)] <- caps[1]
Cust_Churn$DataUsage[Cust_Churn$DataUsage > (qnt[2] + H)] <- caps[2]
summary(Cust_Churn$DataUsage)
length(boxplot.stats(Cust_Churn$DataUsage)$out)

## Outlier Treatment for DayMins variable
qnt <- quantile(Cust_Churn$DayMins, probs=c(.25, .75), na.rm = T)
caps <- quantile(Cust_Churn$DayMins, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Cust_Churn$DayMins, na.rm = T)

Cust_Churn$DayMins[Cust_Churn$DayMins < (qnt[1] - H)] <- caps[1]
Cust_Churn$DayMins[Cust_Churn$DayMins > (qnt[2] + H)] <- caps[2]
summary(Cust_Churn$DayMins)
length(boxplot.stats(Cust_Churn$DayMins)$out)

## Outlier Treatment for DayCalls variable
qnt <- quantile(Cust_Churn$DayCalls, probs=c(.25, .75), na.rm = T)
caps <- quantile(Cust_Churn$DayCalls, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Cust_Churn$DayCalls, na.rm = T)

Cust_Churn$DayCalls[Cust_Churn$DayCalls < (qnt[1] - H)] <- caps[1]
Cust_Churn$DayCalls[Cust_Churn$DayCalls > (qnt[2] + H)] <- caps[2]
summary(Cust_Churn$DayCalls)
length(boxplot.stats(Cust_Churn$DayCalls)$out)

## Outlier Treatment for MonthlyCharge variable
qnt <- quantile(Cust_Churn$MonthlyCharge, probs=c(.25, .75), na.rm = T)
caps <- quantile(Cust_Churn$MonthlyCharge, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Cust_Churn$MonthlyCharge, na.rm = T)

Cust_Churn$MonthlyCharge[Cust_Churn$MonthlyCharge < (qnt[1] - H)] <- caps[1]
Cust_Churn$MonthlyCharge[Cust_Churn$MonthlyCharge > (qnt[2] + H)] <- caps[2]
summary(Cust_Churn$MonthlyCharge)
length(boxplot.stats(Cust_Churn$MonthlyCharge)$out)

## Outlier Treatment for OverageFee variable
qnt <- quantile(Cust_Churn$OverageFee, probs=c(.25, .75), na.rm = T)
caps <- quantile(Cust_Churn$OverageFee, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Cust_Churn$OverageFee, na.rm = T)

Cust_Churn$OverageFee[Cust_Churn$OverageFee < (qnt[1] - H)] <- caps[1]
Cust_Churn$OverageFee[Cust_Churn$OverageFee > (qnt[2] + H)] <- caps[2]
summary(Cust_Churn$OverageFee)
length(boxplot.stats(Cust_Churn$OverageFee)$out)

## Outlier Treatment for RoamMins variable
qnt <- quantile(Cust_Churn$RoamMins, probs=c(.25, .75), na.rm = T)
caps <- quantile(Cust_Churn$RoamMins, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Cust_Churn$RoamMins, na.rm = T)

Cust_Churn$RoamMins[Cust_Churn$RoamMins < (qnt[1] - H)] <- caps[1]
Cust_Churn$RoamMins[Cust_Churn$RoamMins > (qnt[2] + H)] <- caps[2]
summary(Cust_Churn$RoamMins)
length(boxplot.stats(Cust_Churn$RoamMins)$out)

# Checking the Summary and Structure of dataset after making doing the treatments
summary(Cust_Churn)
str(Cust_Churn)

## Univariate Analysis
plot_intro(Cust_Churn)
plot_histogram(Cust_Churn)
plot_bar(Cust_Churn, title="Categorical variables in Cust_Churn Dataset")
## Bivariate Analysis

#Boxplots 
names(Cust_Churn)

### Few boxplots
boxplot(Cust_Churn[c(2,5,6:11)],
        las=1,
        horizontal = TRUE,
        cex= 0.8,
        par(cex.axis = 0.8),
        col=brewer.pal(8,"Set1"),
        main = "Boxplots of continous variables")

boxplot(Cust_Churn$AccountWeeks, main="Boxplot of AccountWeeks")
boxplot(Cust_Churn$DataUsage, main="Boxplot of DataUsage")
boxplot(Cust_Churn$CustServCalls, main="Boxplot of CustServCalls")
boxplot(Cust_Churn$DayMins, main="Boxplot of DayMins")
boxplot(Cust_Churn$DayCalls, main="Boxplot of DayCalls")
boxplot(Cust_Churn$MonthlyCharge, main="Boxplot of MonthlyCharge")
boxplot(Cust_Churn$OverageFee, main="Boxplot of OverageFee")
boxplot(Cust_Churn$RoamMins, main="Boxplot of RoamMins")

ggplot(Cust_Churn, aes(AccountWeeks, fill= Churn)) + geom_density(alpha=0.5)
ggplot(Cust_Churn, aes(DataUsage, fill= Churn)) + geom_density(alpha=0.5)
ggplot(Cust_Churn, aes(CustServCalls, fill= Churn)) + geom_density(alpha=0.5)
ggplot(Cust_Churn, aes(DayMins, fill= Churn)) + geom_density(alpha=0.5)
ggplot(Cust_Churn, aes(DayCalls, fill= Churn)) + geom_density(alpha=0.5)
ggplot(Cust_Churn, aes(MonthlyCharge, fill= Churn)) + geom_density(alpha=0.5)
ggplot(Cust_Churn, aes(OverageFee, fill= Churn)) + geom_density(alpha=0.5)
ggplot(Cust_Churn, aes(RoamMins, fill= Churn)) + geom_density(alpha=0.5)


#Multi Boxplot
plot_boxplot(Cust_Churn[c(-3,-4)], by="Churn")

#Scatter Plots
plot(Cust_Churn$DataUsage, Cust_Churn$RoamMins)
plot(Cust_Churn$DataUsage, Cust_Churn$MonthlyCharge)
plot(Cust_Churn$DayMins, Cust_Churn$MonthlyCharge)
plot(Cust_Churn$OverageFee, Cust_Churn$MonthlyCharge)

#Check for Multicollinearity
pairs(Cust_Churn[c(-1,-3,-4)], pch=15, col=c("darkblue","magenta","dark green"))
##correlation between continous variables (Multi variate Analysis)
plot_correlation(Cust_Churn[ ,c(2,5:11)])
##correlation between continous variables 
cor(Cust_Churn[ ,c(2,5:11)])

# Treatment for Multicollinearity
set.seed(300)
LRmodel_mc = glm(Churn ~ ., data = Cust_Churn, family= binomial)
summary(LRmodel_mc)
car::vif(LRmodel_mc)
#Treatment - We will remove the variables which has a VIF > 5 . These are MonthyCharge,
#DataUsage,DayMins,DataPlan,OverageFee
LRmodel_mc_treated = glm(Churn ~ AccountWeeks+ContractRenewal+CustServCalls+DayCalls+RoamMins  , data = Cust_Churn, family= binomial)
summary(LRmodel_mc_treated)
car::vif(LRmodel_mc_treated)
##################################################################################################



#################################################################################
#### build univariate logistic regression models and check results
mod.num1 <- glm(Churn~AccountWeeks, data = Cust_Churn, family = binomial)
summary(mod.num1)
# AccountWeeks is NOT a significant variable

mod.num2 <- glm(Churn~ContractRenewal , data = Cust_Churn, family = binomial)
summary(mod.num2)
# ContractRenewal  is a significant variable

mod.num3 <- glm(Churn~CustServCalls  , data = Cust_Churn, family = binomial)
summary(mod.num3)
# CustServCalls   is a significant variable

mod.num4 <- glm(Churn~DayMins   , data = Cust_Churn, family = binomial)
summary(mod.num4)
# DayMins is a significant variable

mod.num5 <- glm(Churn~DayCalls    , data = Cust_Churn, family = binomial)
summary(mod.num5)
# DayCalls  is NOT a significant variable

mod.num6 <- glm(Churn~MonthlyCharge     , data = Cust_Churn, family = binomial)
summary(mod.num6)
# MonthlyCharge is  a significant variable

mod.num7 <- glm(Churn~OverageFee      , data = Cust_Churn, family = binomial)
summary(mod.num7)
# OverageFee       is  a significant variable

mod.num8 <- glm(Churn~RoamMins , data = Cust_Churn, family = binomial)
summary(mod.num8)
# RoamMins       is  a significant variable

mod.fact1 <- glm(Churn~ContractRenewal , data = Cust_Churn, family = binomial)
summary(mod.fact1)
# ContractRenewal      is  a significant variable

mod.fact2 <- glm(Churn~DataPlan , data = Cust_Churn, family = binomial)
summary(mod.fact2)
# DataPlan      is  a significant variable
###########################################################################################

# Now we can build the full model

# Split data into test and train datasets
set.seed(300)
library(caTools)
spl = sample.split(Cust_Churn$Churn, SplitRatio=0.65)
train = subset(Cust_Churn, spl ==T)
test = subset(Cust_Churn, spl==F)

dim(train)
dim(test)
## Check split consistency
#sum(as.integer(as.character(train$TenYearCHD))) / nrow(train)
sum(as.integer(as.character(train$Churn))) / nrow(train)
sum(as.integer(as.character(test$Churn))) / nrow(test)
sum(as.integer(as.character(Cust_Churn$Churn))) / nrow(Cust_Churn)

# build the model with all variables
set.seed(300)
LRmodel1 = glm(Churn ~ ., data = train, family= binomial)
summary(LRmodel1)

predTest1 = predict(LRmodel1, newdata= test, type="response")
table(test$Churn, predTest1>0.5)
(977+23)/nrow(na.omit(test)) #Accuracy= 85.68%

# build the model after omitting variables with high VIF's -MonthlyCharge,DataUsage,DayMins,DataPlan,OverageFee
set.seed(301)
LRmodel2 = glm(Churn ~ AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayCalls, data = train, family= binomial)
summary(LRmodel2)

predTest2 = predict(LRmodel2, newdata= test, type="response")
table(test$Churn, predTest2>0.5)
(982+21)/nrow(na.omit(test)) #= 85.95%

# From output we see that AccountWeeks & DayCalls are not significant variables.So re running
#the model after removing the insignificant variables.
LRmodel3 = glm(Churn ~ ContractRenewal+CustServCalls+RoamMins, data = train, family= binomial)
summary(LRmodel3)

predTest3 = predict(LRmodel3, newdata= test, type="response")
table(test$Churn, predTest3>0.5)
(983+21)/nrow(na.omit(test)) #= 86.03%

## Finally we use library blorr to build and validate binary logistic models
library(blorr)
#We pass the output of model where all variables were taken into account to blr_step_aic_both() function
blr_step_aic_both(LRmodel1, details = FALSE)
#Based on the output from blorr, we run the LR model for the 4th time
set.seed(400)
LRmodel4 <- glm(Churn~ContractRenewal+CustServCalls+DataPlan+MonthlyCharge+RoamMins+DataUsage, data = Cust_Churn, family = binomial)
summary(LRmodel4)
predTest4 = predict(LRmodel4, newdata= test, type="response")
table(test$Churn, predTest4>0.5)
#(976+26)/nrow(na.omit(test)) = 85.86%

###############################################################################################
#Of all the iterations that we performed, the accuracy was best for the model where we removed 
#multicollinear and insignificant variables.However the difference was very marginal and also 
#since we know that Multicollinearity is not a parameter that really affects Accuracy, we
#will take that model output that was given by blorr() function (LRmodel4) and
#perform other model performance measures on it for the test dataset.
###############################################################################################
library(ROCR)
ROCRpred = prediction(predTest4, test$Churn)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf,col="black",lty=2, lwd=2)
plot(perf,lwd=3,colorize = TRUE)

k <- blr_gains_table(LRmodel4)
plot(k)

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

blr_confusion_matrix(LRmodel4, data = test)

blr_gini_index(LRmodel4, data = Cust_Churn)

blr_roc_curve(k, title = "ROC Curve",
              xaxis_title = "1 - Specificity",
              yaxis_title = "Sensitivity",roc_curve_col = "blue",
              diag_line_col = "red", point_shape = 18,
              point_fill = "blue", point_color = "blue",
              plot_title_justify = 0.5)  

blr_rsq_mcfadden(LRmodel4)
blr_rsq_mcfadden_adj(LRmodel4)


blr_plot_difchisq_fitted(LRmodel4, point_color = "blue",
                         title = "Delta Chi Square vs Fitted Values Plot",
                         xaxis_title = "Fitted Values",
                         yaxis_title = "Delta Chi Square")

###############################################################################
###########################KNN MODEL###########################################
###############################################################################

#normalizing the data 
norm = function(x) { (x- min(x))/(max(x) - min(x)) }
norm.data = as.data.frame(lapply(Cust_Churn[,c(-1,-3,-4)], norm))
View(Cust_Churn)
View(norm.data)
usable.data = cbind(Cust_Churn[,c(1,3,4)], norm.data)
str(usable.data)
head(usable.data)
# Data partitioning
library(caTools)
#Use KNN Classifier 
# Split data into test and train datasets
set.seed(300)
library(caTools)
spl2 = sample.split(Cust_Churn$Churn, SplitRatio=0.65)
train.knn = subset(Cust_Churn, spl2 ==T)
test.knn = subset(Cust_Churn, spl2==F)
dim(train.knn)
dim(test.knn)

library(class)
#Removing churn from first and second param and adding it to the third.
#Trying with k =sqrt(2166+1167) = 58
pred = knn(train.knn[-1], test.knn[-1], train.knn$Churn, k = 58) 
table.knn = table(test.knn$Churn, pred)
sum(diag(table.knn)/sum(table.knn)) 

#Finding the optimum value of K
library(caret)
set.seed(401)
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
knnFit <- train(Churn ~ ., data = train.knn, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit
plot(knnFit)
dim(train)
dim(test)
pred.knn = knn(train.knn[-1], test.knn[-1], train.knn$Churn, k = 5)
table.knn = table(test.knn$Churn, pred.knn)
table.knn
sum(diag(table.knn)/sum(table.knn)) # 
#(971+43)/(976+91+22+78) Accuracy = 86.88%

knnPredict <- predict(knnFit,newdata = test.knn )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, test.knn$Churn )

################################################################################
############################NAIVE BAYES MODEL###################################
################################################################################
names(Cust_Churn)
head(Cust_Churn)
head(train)
head(test)
library(e1071)
# Since Naive Bayes is impacted by multicollinearity, we will remove high vif variables value from the dataset.
# Variables impacted by Multi collinearity - MonthyCharge,DataUsage,DayMins,DataPlan,OverageFee
# Split data into test and train datasets
set.seed(300)
library(caTools)
spl3 = sample.split(Cust_Churn$Churn, SplitRatio=0.65)
train.NB = subset(Cust_Churn, spl3 ==T)
test.NB = subset(Cust_Churn, spl3==F)
dim(train.NB)
dim(test.NB)

NB = naiveBayes(Churn ~AccountWeeks+ContractRenewal+CustServCalls+DayCalls+RoamMins, data = train.NB)
predNB = predict(NB, test.NB, type = "class")
tab.NB = table(predNB,test.NB$Churn)
tab.NB
sum(diag(tab.NB)/sum(tab.NB)) #Accuracy = 86.63

#We will also check the Accuracy had we included all the variables
NB_All = naiveBayes(Churn ~ ., data = train.NB)
predNB_All = predict(NB_All, test.NB, type = "class")
tab.NB_All = table(predNB_All,test.NB$Churn)
tab.NB_All
sum(diag(tab.NB_All)/sum(tab.NB_All)) #Accuracy = 86.80

#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(predNB, test.NB$Churn )
