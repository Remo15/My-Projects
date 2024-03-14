
######################################################################
###########################THERA BANK SOLUTION########################
######################################################################

#Install packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("corrplot")
install.packages("caTools")
install.packages("rpart")
install.packages("ROCR")
install.packages("rattle")
install.packages("data.tables")
install.packages("scales")
install.packages("InformationValue")
install.packages("ineq")

#Load libraries
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(readxl)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(ROCR)
library(data.tables)
library(scales)
library(InformationValue)
library(ineq)

# Set the working directory and check it
setwd("C:/2020/RProgramming/DataMining")
getwd()

#Import the data
Thera_Bank <- read_excel("C:/2020/RProgramming/DataMining/Thera Bank_dataset.xlsx", sheet=2)
View(Thera_Bank)

##Check the dimension or shape of the data
dim(Thera_Bank)

#Perform basic summary 
head(Thera_Bank)
tail(Thera_Bank)

##View top 5 rows
Thera_Bank[1:5,]

##Lets see the variable list in the data and then make changes to the col names
names(Thera_Bank)
names(Thera_Bank)[names(Thera_Bank)== "Age (in years)"] <- "Age"
names(Thera_Bank)[names(Thera_Bank)== "Experience (in years)"] <- "Experience_In_Years"
names(Thera_Bank)[names(Thera_Bank)== "Income (in K/year)"] <- "Income_In_K_Peryear"
names(Thera_Bank)[names(Thera_Bank)== "ZIP Code"] <- "ZIP_Code"
names(Thera_Bank)[names(Thera_Bank)== "Family members"] <- "Family_Members"
names(Thera_Bank)[names(Thera_Bank)== "Personal Loan"] <- "Personal_Loan"
names(Thera_Bank)[names(Thera_Bank)== "Securities Account"] <- "Securities_Account"
names(Thera_Bank)[names(Thera_Bank)== "CD Account"] <- "CD_Account"

names(Thera_Bank)


##Lets see the datatype of the data set. Can see several factor variables as factors
str(Thera_Bank)
summary(Thera_Bank)
Thera_Bank$Experience_In_Years = abs(Thera_Bank$Experience_In_Years)
sum(is.na(Thera_Bank))
#Replacing NA values in the dataset with zeroes
Thera_Bank[is.na(Thera_Bank)] = 0
any(is.na(Thera_Bank))

##Convert all variables into factors where necessary. 
#library(dplyr) 
Thera_Bank$Personal_Loan <- as.factor(Thera_Bank$Personal_Loan)
Thera_Bank$Securities_Account <- as.factor(Thera_Bank$Securities_Account)
Thera_Bank$CD_Account <- as.factor(Thera_Bank$CD_Account)
Thera_Bank$Online <- as.factor(Thera_Bank$Online)                                  
Thera_Bank$CreditCard <- as.factor(Thera_Bank$CreditCard) 
Thera_Bank$Family_Members <- as.factor(Thera_Bank$Family_Members) 
# For education, we convert it into an Ordered factor such that 1 < 2 < 3
Thera_Bank$Education <- factor(Thera_Bank$Education,levels=c('1','2','3'),ordered = TRUE)

str(Thera_Bank)

##Summary Statistics Measure of central tendency and dispersion (Univariate Analysis)

summary(Thera_Bank)
##(count,missing value,mean,0.01,0.05,0.10,0.25,Median,0.75,0.90,0.95,0.99,min,max,range,skew,kurtosis,
#SD,IQR) for continous variable


names(Thera_Bank)
boxplot(Thera_Bank$Income_In_K_Peryear)
boxplot(Thera_Bank$CCAvg)
boxplot(Thera_Bank$Mortgage)

boxplot.stats(Thera_Bank$Income_In_K_Peryear)$out
summary(Thera_Bank$Income_In_K_Peryear)

#For missing values that lie outside the 1.5 * IQR limits, we could cap it by replacing those observations outside the lower limit with the value of 5th %ile and those that lie 
#above the upper limit, with the value of 95th %ile. Below is a sample code that achieves this.
#Thera_Bank$Income_In_K_Peryear <- Thera_Bank$Income_In_K_Peryear

##Outlier Treatment for Income_In_K_Peryear variable
boxplot(Thera_Bank$Income_In_K_Peryear)
qnt <- quantile(Thera_Bank$Income_In_K_Peryear, probs=c(.25, .75), na.rm = T)
caps <- quantile(Thera_Bank$Income_In_K_Peryear, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Thera_Bank$Income_In_K_Peryear, na.rm = T)
Thera_Bank$Income_In_K_Peryear[Thera_Bank$Income_In_K_Peryear < (qnt[1] - H)] <- caps[1]
Thera_Bank$Income_In_K_Peryear[Thera_Bank$Income_In_K_Peryear > (qnt[2] + H)] <- caps[2]

summary(Thera_Bank$Income_In_K_Peryear)
boxplot(Thera_Bank$Income_In_K_Peryear)
boxplot.stats(Thera_Bank$Income_In_K_Peryear)$out

## Outlier Treatment for CCAvg variable
qnt <- quantile(Thera_Bank$CCAvg, probs=c(.25, .75), na.rm = T)
caps <- quantile(Thera_Bank$CCAvg, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Thera_Bank$CCAvg, na.rm = T)
Thera_Bank$CCAvg[Thera_Bank$CCAvg < (qnt[1] - H)] <- caps[1]
Thera_Bank$CCAvg[Thera_Bank$CCAvg > (qnt[2] + H)] <- caps[2]

summary(Thera_Bank$CCAvg)
boxplot(Thera_Bank$CCAvg)
boxplot.stats(Thera_Bank$CCAvg)$out
names(Thera_Bank)

## Outlier Treatment for Mortgage variable
qnt <- quantile(Thera_Bank$Mortgage, probs=c(.25, .75), na.rm = T)
caps <- quantile(Thera_Bank$Mortgage, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(Thera_Bank$Mortgage, na.rm = T)
Thera_Bank$Mortgage[Thera_Bank$Mortgage < (qnt[1] - H)] <- caps[1]
Thera_Bank$Mortgage[Thera_Bank$Mortgage > (qnt[2] + H)] <- caps[2]

summary(Thera_Bank$Mortgage)
str(Thera_Bank)
boxplot(Thera_Bank$Mortgage)
boxplot.stats(Thera_Bank$Mortgage)$out

# Checking the Summary and Structure of dataset after making doing the treatments
summary(Thera_Bank)
str(Thera_Bank)




#Thera_Bank_Cat_Var = Thera_Bank[c(5,6,8:14)]
#Thera_Bank_Cont_Var = Thera_Bank[c(2,3,4,7)]
#Thera_Bank_Cat_Var 
#Thera_Bank_Cont_Var

## Univariate Analysis

plot_intro(Thera_Bank)
plot_histogram(Thera_Bank)
plot_bar(Thera_Bank, title="Categorical variables")
## Bivariate Analysis

#Boxplots
plot_boxplot(Thera_Bank, by = "Education", geom_boxplot_args = list("outlier.color" = "blue"))
plot_boxplot(Thera_Bank, by = "Personal_Loan", geom_boxplot_args = list("outlier.color" = "blue"))
plot_boxplot(Thera_Bank, by = "Family_Members", geom_boxplot_args = list("outlier.color" = "blue"))


ggplot(Thera_Bank, aes(Age, fill= Personal_Loan)) + geom_density(alpha=0.5)
ggplot(Thera_Bank, aes(Experience_In_Years, fill= Personal_Loan)) + geom_density(alpha=0.5)
ggplot(Thera_Bank, aes(Income_In_K_Peryear, fill= Personal_Loan)) + geom_density(alpha=0.5)
ggplot(Thera_Bank, aes(CCAvg, fill= Personal_Loan)) + geom_density(alpha=0.5)
ggplot(Thera_Bank, aes(Mortgage, fill= Personal_Loan)) + geom_density(alpha=0.5)

ggplot(Thera_Bank, aes(Income_In_K_Peryear, Mortgage, color = Personal_Loan)) + geom_point(alpha = 0.5)
ggplot(Thera_Bank, aes(Income_In_K_Peryear, CCAvg, color = Personal_Loan)) + geom_point(alpha = 0.5)
ggplot(Thera_Bank, aes(CCAvg, Mortgage, color = Personal_Loan)) + geom_point(alpha = 0.5)

counts <- table(Thera_Bank$Family_Members,Thera_Bank$Personal_Loan)
counts
barplot(counts, main="Family members vs Personal Loan",
        xlab="Personal Loan No vs Yes",ylab="No: of Observations", col=c("darkblue","red","green","yellow","black"),
        legend = rownames(counts), beside=TRUE)

counts <- table(Thera_Bank$Education,Thera_Bank$Personal_Loan)
counts
barplot(counts, main="Education vs Personal Loan",
        xlab="Personal Loan No vs Yes",ylab="No: of Observations", col=c("darkblue","red","green"),
        legend = rownames(counts), beside=TRUE)
##################################################################################################
##correlation between continous variables (Multi variate Analysis)
plot_correlation(Thera_Bank[,c(2,4,5,7,9)])
##correlation between categorical variables 
cor(Thera_Bank[,c(3,6,8,10,11,12,13,14)])
#corrplot(Thera_Bank[c(2,3,4,7,9)],method="number")

#********************************************************
summary(Thera_Bank$Mortgage)
names(Thera_Bank)
#length(Thera_Bank$ZIP_Code)
#length(unique(Thera_Bank$ZIP_Code))

########################################################################################################################
##Build a decision tree using CART technique
########################################################################################################################

## Spliting the dataset into train and test for development and out of sample testing respectively
library(caTools)
set.seed(100)
names(Thera_Bank)
#Removing the id column
Thera_Bank = Thera_Bank[-1]
names(Thera_Bank)

#we are splitting the data such that we have 70% of the data is Train Data and 30% of the data is my Test Data
split1 <- sample.split(Thera_Bank$Personal_Loan, SplitRatio = 0.7)
View(split1)
#Use subset function to get the data that is TRUE for the training data set
CARTtrain<- subset(Thera_Bank, split1 == TRUE)
#Use subset function to get the data that is FALSE for the training data set
CARTtest<- subset(Thera_Bank,  split1 == FALSE)
#Dimensions of test and train data
dim(CARTtrain)
dim(CARTtest)
#View the train and test data sets
View(CARTtrain)
View(CARTtest)
## Calculate the response rate. To see number of actual employees that have left organization.
table(CARTtrain$Personal_Loan)
sum(CARTtrain$Personal_Loan == "1")/nrow(CARTtrain)

##Import rpart and rpart.plot library for creating CART model
library(rpart)
library(rpart.plot)

#Define the parameters
#Hence, minsplit helps us avoid overfitting by pre-pruning the tree before we test our model.
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)

#Building the CART model using the rpart function and the pre defined parameters
m1 <- rpart(formula = Personal_Loan~., data = CARTtrain, method = "class", control = r.ctrl)
m1
#Displaying the decision tree
library(rattle)
fancyRpartPlot(m1)

#Examine the complexity plot
printcp(m1)
plotcp(m1)

#Prune the tree at cp=0.002 and view it
m1_pruned <- prune(m1, cp = 0.024)
m1_pruned
names(Thera_Bank)
#Display the new pruned tree
fancyRpartPlot(m1_pruned)



###############################################################


CARTtest$CART.Pred = predict(m1_pruned,CARTtest,type="class")
CARTtest$CART.Score = predict(m1_pruned,CARTtest,type="prob")


View(CARTtrain)
View(CARTtest)
class(CARTtrain)
class(CARTtest)
names(Thera_Bank)
########################################################################################################################
##Build a Random Forest model
########################################################################################################################

## Spliting the dataset into train and test for development and out of sample testing respectively
set.seed(100)
#we are splitting the data such that we have 70% of the data is Train Data and 30% of the data is my Test Data
split2 <- sample.split(Thera_Bank$Personal_Loan, SplitRatio = 0.7)
View(split1)
#Use subset function to get the data that is TRUE for the training data set
RFtrain<- subset(Thera_Bank, split2 == TRUE)

#Use subset function to get the data that is FALSE for the testing data set
RFtest<- subset(Thera_Bank,  split2 == FALSE)

dim(RFtrain)
dim(RFtest)

##import randomForest library for building random forest model
library(randomForest)

## set a seed to start the randomness
seed = 1000
set.seed(seed)

##Build the first RF model
#Node size shall be ~2% of the population. This is similar to minbucket paraeter in CART
#0.02*3500 = 70
# mtry can be taken as sqrt(Variables) = 4

Rforest = randomForest(Personal_Loan~.,data=RFtrain,ntree=300,mtry=10,nodesize=50,importance=TRUE)

##Print the model to see the OOB and error rate
print(Rforest)

# Running RF again with ntree=101
Rforest2 = randomForest(Personal_Loan~.,data=RFtrain,ntree=100,mtry=10,nodesize=40,importance=TRUE)
##Print the model to see the OOB and error rate
print(Rforest2)
##Plot the RF to know the optimum number of trees
plot(Rforest2) 



##The above plot shows - Misclassification/error rate as a function of trees grown
#The black line represents the entire sample, 
#the green line represents the error rate where Y = 0 
#and the red line represents the error rate when Y = 1.

View(RFtrain)
##Tune up the RF model to find out the best mtry which reduces the Out of bag error estimate
#If doBest is set as TRUE, a random forest object is returned with the optimal obtained mtry
set.seed(seed)
tRforest = tuneRF(x=RFtrain[,-c(9)],y=RFtrain$Personal_Loan,mtrystart = 10,stepfactor=1.5,ntree=100,improve=0.0001,
                  nodesize=10,trace=TRUE,plot=TRUE,doBest=TRUE,importance=TRUE)



##Build the refined RF model
Rforest3 = randomForest(Personal_Loan~.,data=RFtrain,ntree=100,mtry=6,nodesize=10,importance=TRUE)
print(Rforest3)
plot(tRforest)
##Identify the importance of the variables
importance(Rforest3)
varImpPlot(Rforest3,type=2)

#################################################################################################
#########################################################################################

##Use this tree to do the prediction on train as well as test data set
RFtrain$RF.Pred = predict(Rforest3,data=RFtrain,type="class")
RFtrain$RF.Score = predict(Rforest3,data=RFtrain,type="prob")

RFtest$RF.Pred = predict(Rforest3,RFtest,type="class")
RFtest$RF.Score = predict(Rforest3,RFtest,type="prob")

View(RFtrain)
View(RFtest)


########################################################################################################################
##Assess the Models Performance - CART
########################################################################################################################
## Let's use rattle to see various model evaluation measures
##rattle()
library(rattle)

## Scoring syntax for train data set
##Use this pruned tree to do the prediction on train as well as test data set
CARTtrain$CART.Pred = predict(m1_pruned,data=CARTtrain,type="class")
CARTtrain$CART.Score = predict(m1_pruned,data=CARTtrain, type="prob")

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}
class(CARTtrain$CART.Score)
## deciling
CARTtrain$deciles <- decile(CARTtrain$CART.Score[,2])
View(CARTtrain)

## Ranking code
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)
tmp_Train = data.table(CARTtrain)
rank <- tmp_Train[, list(
  cnt = length(Personal_Loan), 
  cnt_resp = sum(Personal_Loan == 1), 
  cnt_non_resp = sum(Personal_Loan == 0)) , 
  by=deciles][order(-deciles)]


rank$rrate <- round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),4);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp) * 100;
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

## Scoring syntax for test data set
##Use this pruned tree to do the prediction on train as well as test data set
CARTtest$CART.Pred = predict(m1_pruned,data=CARTtest,type="class")
CARTtest$CART.Score = predict(m1_pruned,data=CARTtest, type="prob")

## deciling code


class(CARTtest$CART.Score)
## deciling
CARTtrain$deciles <- decile(CARTtrain$CART.Score[,2])
View(CARTtrain)

## Ranking code
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)
tmp_Train = data.table(CARTtrain)
rank <- tmp_Train[, list(
  cnt = length(Personal_Loan), 
  cnt_resp = sum(Personal_Loan == 1), 
  cnt_non_resp = sum(Personal_Loan == 0)) , 
  by=deciles][order(-deciles)]


rank$rrate <- round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),4);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp) * 100;
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

#Confusion matrix
## CART Model Confusion Metrix
CART_CM_train = table(CARTtrain$Personal_Loan,CARTtrain$CART.Pred)
CART_CM_train

## Misclassification Rate
(CART_CM_train[1,2]+CART_CM_train[2,1])/nrow(CARTtrain) #Misclassified in train

##Accuracy
(CART_CM_train[1,1]+CART_CM_train[2,2])/nrow(CARTtrain) #Correct classification in train

##Specificity(True Negative Rate)
(CART_CM_train[1,1])/(CART_CM_train[1,1]+CART_CM_train[1,2])

##Recall or Sensitivity
(CART_CM_train[2,2])/(CART_CM_train[2,1]+CART_CM_train[2,2])

##Precision
(CART_CM_train[2,2])/(CART_CM_train[1,2]+CART_CM_train[2,2])
  
#Area under the ROC curve
install.packages("ROCR")
library(ROCR)
pred_dtrain <- prediction(CARTtrain$CART.Score[,2], CARTtrain$Personal_Loan)
perf_dtrain <- performance(pred_dtrain, "tpr", "fpr")
plot(perf_dtrain,main = "ROC curve")

#Check area under the ROC curve
auc_train_dt <- performance(pred_dtrain,"auc")
auc_train_dt <- as.numeric(auc_train_dt@y.values)
auc_train_dt

#Gain chart
library(gains)
gain_dtrain <- performance(pred_dtrain, "tpr", "rpp")
plot(gain_dtrain, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#Kolmogorov-Smirnov (KS) statistic and plot
ks_dtrain <- max(perf_dtrain@y.values[[1]]- perf_dtrain@x.values[[1]])
plot(perf_dtrain,main=paste0('KS=',round(ks_dtrain*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

#Gini
install.packages("ineq")
library(ineq)
ineq(CARTtrain$CART.Score[,2],"gini")

#Concordance
install.packages("InformationValue")
library(InformationValue)
Concordance(actuals=CARTtrain$Personal_Loan,predictedScores = CARTtrain$CART.Score[,2])

# Lift chart
install.packages("lift")
library(lift)

plotLift(CARTtrain$CART.Score[,2],CARTtrain$Personal_Loan,cumulative = TRUE)

###############################################################################
################################Model Performance for CART Test dataset###################

## Scoring syntax for test data set
##Use this pruned tree to do the prediction on train as well as test data set

CARTtest$CART.Pred = predict(m1_pruned,CARTtest,type="class")
CARTtest$CART.Score = predict(m1_pruned,CARTtest,type="prob")

## deciling code


class(CARTtest$CART.Score)
## deciling
CARTtest$deciles <- decile(CARTtest$CART.Score[,2])
View(CARTtest)

## Ranking code
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)
tmp_Test = data.table(CARTtest)

rank_test <- tmp_Test[, list(
  cnt = length(Personal_Loan), 
  cnt_resp = sum(Personal_Loan == 1), 
  cnt_non_resp = sum(Personal_Loan == 0)) , 
  by=deciles][order(-deciles)]


rank_test$rrate <- round(rank_test$cnt_resp / rank_test$cnt,4);
rank_test$cum_resp <- cumsum(rank_test$cnt_resp)
rank_test$cum_non_resp <- cumsum(rank_test$cnt_non_resp)
rank_test$cum_rel_resp <- round(rank_test$cum_resp / sum(rank_test$cnt_resp),4);
rank_test$cum_rel_non_resp <- round(rank_test$cum_non_resp / sum(rank_test$cnt_non_resp),4);
rank_test$ks <- abs(rank_test$cum_rel_resp - rank_test$cum_rel_non_resp) * 100;
rank_test$rrate <- percent(rank_test$rrate)
rank_test$cum_rel_resp <- percent(rank_test$cum_rel_resp)
rank_test$cum_rel_non_resp <- percent(rank_test$cum_rel_non_resp)

View(rank_test)

#Confusion matrix
## CART Model Confusion Metrix for test data
CART_CM_test = table(CARTtest$Personal_Loan,CARTtest$CART.Pred)
CART_CM_test

## Misclassification Rate
(CART_CM_test[1,2]+CART_CM_test[2,1])/nrow(CARTtest) #Misclassified in test

##Accuracy
(CART_CM_test[1,1]+CART_CM_test[2,2])/nrow(CARTtest) #Correct classification in test

##Specificity(True Negative Rate)
(CART_CM_test[1,1])/(CART_CM_test[1,1]+CART_CM_test[1,2])

##Recall or Sensitivity
(CART_CM_test[2,2])/(CART_CM_test[2,1]+CART_CM_test[2,2])

##Precision
(CART_CM_test[2,2])/(CART_CM_test[1,2]+CART_CM_test[2,2])
View(CARTtest)
#Area under the ROC curve
install.packages("ROCR")
library(ROCR)
pred_test_cart <- prediction(CARTtest$CART.Score[,2], CARTtest$Personal_Loan)
perf_test_cart <- performance(pred_test_cart, "tpr", "fpr")
plot(perf_test_cart,main = "ROC curve")

#Check area under the ROC curve
auc_test_cart <- performance(pred_test_cart,"auc")
auc_test_cart <- as.numeric(auc_test_cart@y.values)
auc_test_cart
#Gain chart
library(gains)
gain_test_cart <- performance(pred_test_cart, "tpr", "rpp")
plot(gain_test_cart, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#Kolmogorov-Smirnov (KS) statistic and plot
ks_test_cart <- max(perf_test_cart@y.values[[1]]- perf_test_cart@x.values[[1]])
plot(perf_test_cart,main=paste0('KS=',round(ks_test_cart*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

#Gini
install.packages("ineq")
library(ineq)
ineq(CARTtest$CART.Score[,2],"gini")


#Concordance
install.packages("InformationValue")
library(InformationValue)
Concordance(actuals=CARTtest$Personal_Loan,predictedScores = CARTtest$CART.Score[,2])


# Lift chart
install.packages("lift")
library(lift)

plotLift(CARTtest$CART.Score[,2],CARTtest$Personal_Loan,cumulative = TRUE)

##########################################################################
##########################################################################
###########################################################################
##Assess the Models Performance - Random Forest
##########################################################################
##########################################################################
##########################################################################
#Let's use rattle to see various model evaluation measures for train data set
##rattle()


RFtrain$RF.Pred = predict(Rforest3,RFtrain,type="class")
RFtrain$RF.Score = predict(Rforest3,RFtrain,type="prob")


View(RFtrain)
## deciling
RFtrain$deciles <- decile(RFtrain$RF.Score[,2])
View(RFtrain)

## Ranking code
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)
tmp_Train_RF = data.table(RFtrain)

rank_train_RF <- tmp_Train_RF[, list(
  cnt = length(Personal_Loan), 
  cnt_resp = sum(Personal_Loan == 1), 
  cnt_non_resp = sum(Personal_Loan == 0)) , 
  by=deciles][order(-deciles)]


rank_train_RF$rrate <- round(rank_train_RF$cnt_resp / rank_train_RF$cnt,4);
rank_train_RF$cum_resp <- cumsum(rank_train_RF$cnt_resp)
rank_train_RF$cum_non_resp <- cumsum(rank_train_RF$cnt_non_resp)
rank_train_RF$cum_rel_resp <- round(rank_train_RF$cum_resp / sum(rank_train_RF$cnt_resp),4);
rank_train_RF$cum_rel_non_resp <- round(rank_train_RF$cum_non_resp / sum(rank_train_RF$cnt_non_resp),4);
rank_train_RF$ks <- abs(rank_train_RF$cum_rel_resp - rank_train_RF$cum_rel_non_resp) * 100;
rank_train_RF$rrate <- percent(rank_train_RF$rrate)
rank_train_RF$cum_rel_resp <- percent(rank_train_RF$cum_rel_resp)
rank_train_RF$cum_rel_non_resp <- percent(rank_train_RF$cum_rel_non_resp)

View(rank_train_RF)

#Confusion matrix
## RF  Model Confusion Metrix for train data
RF_CM_train = table(RFtrain$Personal_Loan,RFtrain$RF.Pred)
RF_CM_train
## Misclassification Rate
(RF_CM_train[1,2]+RF_CM_train[2,1])/nrow(RFtrain) #Misclassified in train

##Accuracy
(RF_CM_train[1,1]+RF_CM_train[2,2])/nrow(RFtrain) #Correct classification in train

##Specificity(True Negative Rate)
(RF_CM_train[1,1])/(RF_CM_train[1,1]+RF_CM_train[1,2])

##Recall or Sensitivity
(RF_CM_train[2,2])/(RF_CM_train[2,1]+RF_CM_train[2,2])

##Precision
(RF_CM_train[2,2])/(RF_CM_train[1,2]+RF_CM_train[2,2])

#Check performance. Plot ROC curve for train
library(ROCR)
pred_rftrain <- prediction(RFtrain$RF.Score[,2], RFtrain$Personal_Loan)
perf_rftrain <- performance(pred_rftrain, "tpr", "fpr")
plot(perf_rftrain,main = "ROC curve")

#Check area under the ROC curve
auc_train_rf <- performance(pred_rftrain,"auc"); 
auc_train_rf <- as.numeric(auc_train_rf@y.values)
auc_train_rf 
#Plot ROC curve for test
#pred_rftest <- prediction(RFtest$RF.Score, RFtest$Personal_Loan)
#perf_rftest <- performance(pred_rftest, "tpr", "fpr")
#plot(perf_rftest,main = "ROC curve")

#Check area under the ROC curve for test
#auc_test_rf <- performance(pred_rftest,"auc"); 
#auc_test_rf <- as.numeric(auc_test_rf@y.values)
#auc_test_rf
## List the importance of the variables.


#Gain chart
gain_rftrain <- performance(pred_rftrain, "tpr", "rpp")
plot(gain_rftrain, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#gain_rftest <- performance(pred_rftest, "tpr", "rpp")
#plot(gain_rftest, col="orange", lwd=2)
#lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#Kolmogorov-Smirnov (KS) statistic and plot
ks_rftrain <- max(perf_rftrain@y.values[[1]]- perf_rftrain@x.values[[1]])
plot(perf_rftrain,main=paste0('KS=',round(ks_rftrain*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

#ks_rftest <- max(perf_rftest@y.values[[1]]- perf_rftest@x.values[[1]])
#plot(perf_rftest,main=paste0('KS=',round(ks_rftest*100,1),'%'))
#lines(x = c(0,1),y=c(0,1))

#Gini
#install.packages("ineq")
library(ineq)
ineq(RFtrain$RF.Score[,2],"gini")
#ineq(RFtest$RF.Score,"gini")

#Concordance
#install.packages("InformationValue")
library(InformationValue)
Concordance(actuals=RFtrain$Personal_Loan,predictedScores = RFtrain$RF.Score[,2])
#Concordance(actuals=RFtest$Personal_Loan,predictedScores = RFtest$RF.Score)

# Lift chart
install.packages("lift")
library(lift)

plotLift(RFtrain$RF.Score[,2],RFtrain$Personal_Loan,cumulative = TRUE)
############################################################################
#Let's  see various model evaluation measures for test data set
############################################################################
View(RFtest)
## deciling
RFtest$deciles <- decile(RFtest$RF.Score[,2])
View(RFtest)

## Ranking code
##install.packages("data.table")
##install.packages("scales")
library(data.table)
library(scales)
tmp_Test_RF = data.table(RFtest)

rank_test_RF <- tmp_Test_RF[, list(
  cnt = length(Personal_Loan), 
  cnt_resp = sum(Personal_Loan == 1), 
  cnt_non_resp = sum(Personal_Loan == 0)) , 
  by=deciles][order(-deciles)]


rank_test_RF$rrate <- round(rank_test_RF$cnt_resp / rank_test_RF$cnt,4);
rank_test_RF$cum_resp <- cumsum(rank_test_RF$cnt_resp)
rank_test_RF$cum_non_resp <- cumsum(rank_test_RF$cnt_non_resp)
rank_test_RF$cum_rel_resp <- round(rank_test_RF$cum_resp / sum(rank_test_RF$cnt_resp),4);
rank_test_RF$cum_rel_non_resp <- round(rank_test_RF$cum_non_resp / sum(rank_test_RF$cnt_non_resp),4);
rank_test_RF$ks <- abs(rank_test_RF$cum_rel_resp - rank_test_RF$cum_rel_non_resp) * 100;
rank_test_RF$rrate <- percent(rank_test_RF$rrate)
rank_test_RF$cum_rel_resp <- percent(rank_test_RF$cum_rel_resp)
rank_test_RF$cum_rel_non_resp <- percent(rank_test_RF$cum_rel_non_resp)

View(rank_test_RF)
#Confusion matrix
## RF  Model Confusion Metrix for test data

RF_CM_test = table(RFtest$Personal_Loan,RFtest$RF.Pred)
RF_CM_test
#RF_CM_test = table(RFtest$Personal_Loan,RFtest$RF.Pred)

## Misclassification Rate
(RF_CM_test[1,2]+RF_CM_test[2,1])/nrow(RFtest) #Misclassified in test

##Accuracy
(RF_CM_test[1,1]+RF_CM_test[2,2])/nrow(RFtest) #Correct classification in test

##Specificity(True Negative Rate)
(RF_CM_test[1,1])/(RF_CM_test[1,1]+RF_CM_test[1,2])

##Recall or Sensitivity
(RF_CM_test[2,2])/(RF_CM_test[2,1]+RF_CM_test[2,2])

##Precision
(RF_CM_test[2,2])/(RF_CM_test[1,2]+RF_CM_test[2,2])


library(ROCR)
pred_rftest <- prediction(RFtest$RF.Score[,2], RFtest$Personal_Loan)
perf_rftest <- performance(pred_rftest, "tpr", "fpr")
plot(perf_rftest,main = "ROC curve")

#Check area under the ROC curve

#Plot ROC curve for test
pred_rftest <- prediction(RFtest$RF.Score[,2], RFtest$Personal_Loan)
perf_rftest <- performance(pred_rftest, "tpr", "fpr")
plot(perf_rftest,main = "ROC curve")

#Check area under the ROC curve for test
auc_test_rf <- performance(pred_rftest,"auc"); 
auc_test_rf <- as.numeric(auc_test_rf@y.values)
auc_test_rf

#Gain chart
gain_rftest <- performance(pred_rftest, "tpr", "rpp")
plot(gain_rftest, col="orange", lwd=2)
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)


#Kolmogorov-Smirnov (KS) statistic and plot

ks_rftest <- max(perf_rftest@y.values[[1]]- perf_rftest@x.values[[1]])
plot(perf_rftest,main=paste0('KS=',round(ks_rftest*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

#Gini
#install.packages("ineq")
library(ineq)
ineq(RFtest$RF.Score[,2],"gini")

#Concordance
#install.packages("InformationValue")
library(InformationValue)
Concordance(actuals=RFtest$Personal_Loan,predictedScores = RFtest$RF.Score[,2])


# Lift chart
install.packages("lift")
library(lift)

plotLift(RFtest$RF.Score[,2],RFtest$Personal_Loan,cumulative = TRUE)


###################################THE END #######################################


