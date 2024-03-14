###########################################################
##################Car-Transport-Problem####################
###########################################################
library(tidyverse)
setwd("C:/2020/RProgramming/Machine_Learning") 
cars  = read.csv("Cars_edited.csv")   
View(cars)
dim(cars)
str(cars) 
head(cars)
tail(cars)
summary(cars) 
names(cars)

#Converting numeric variables to factors as appropriately
cars$Engineer <- as.factor(cars$Engineer)
cars$MBA <- as.factor(cars$MBA)
cars$license <- as.factor(cars$license)

str(cars) 
summary(cars)

#Checking for Na-s in the datset
sum(is.na(cars))
sapply(cars, function(y) {sum(is.na(y))})
subset(cars,is.na(MBA))

#Changing the NA value row to 0
cars[is.na(cars)] <- 0
sum(is.na(cars))

#Checking for outliers
names(cars)
boxplot(cars)
boxplot(cars$Age)
boxplot(cars$Work.Exp)
boxplot(cars$Salary)
boxplot(cars$Distance)

summary(cars$Age)
summary(cars$Work.Exp)
summary(cars$Salary)
summary(cars$Distance)

length(boxplot.stats(cars$Age)$out)
length(boxplot.stats(cars$Work.Exp)$out)
length(boxplot.stats(cars$Salary)$out)
length(boxplot.stats(cars$Distance)$out)

#Outlier Treatment- Capping
#For missing values that lie outside the 1.5 * IQR limits, we could cap it by replacing those observations outside the lower limit with the value of 5th %ile and those that lie 
#above the upper limit, with the value of 95th %ile. Below is a sample code that achieves this.

# We are not performing any Outlier for this dataset.


library(ggplot2)

#plot_intro(cars)

#Univariate analysis: Numeric variables
colnames(cars)
colnames(cars[,sapply(cars, is.numeric)]) 
colnames(cars[,sapply(cars, is.factor)]) 

#ggplot(data, aes(x = cont.variable)) + geom_histogram()
#ggplot(data, aes(y = cont.variable)) + geom_boxplot()
#ggplot(data, aes(x = cont.variable)) + geom_density()
ggplot(cars, aes(x = Age)) + geom_histogram(bins = 30, fill = "lightblue", col = "blue")
ggplot(cars, aes(x = Work.Exp)) + geom_histogram(bins = 30, fill = "darkgreen", col = "lightgreen")
ggplot(cars, aes(x = Salary)) + geom_histogram(bins = 30, fill = "red", col = "pink")
ggplot(cars, aes(x = Distance)) + geom_histogram(bins = 30, fill = "yellow", col = "orange")

#ggplot(data, aes(x = previous)) + geom_boxplot()
#ggplot(data, aes(x = emp.var.rate)) + geom_histogram(bins=30)
ggplot(cars, aes(x = Salary)) + geom_density(col = "blue")
ggplot(cars, aes(x = Distance)) + geom_density(col = "red")

#ggplot(cars, aes(y = cons.conf.idx)) + geom_boxplot(fill = "lightblue", col = "blue")

#Univariate analysis: Categorical variables
colnames(cars[,sapply(data, is.factor)]) 

#ggplot(data, aes(x = cat.variable)) + geom_bar()
ggplot(cars, aes(x = Gender, fill=Gender)) + geom_bar() 
ggplot(cars, aes(x = Engineer, fill=Engineer)) + geom_bar() 
ggplot(cars, aes(x = MBA, fill=MBA)) + geom_bar() 
ggplot(cars, aes(x = license, fill=license)) + geom_bar() 
ggplot(cars, aes(x = Transport, fill=Transport)) + geom_bar() 

#Bivariate Analysis: Fixing one variable as dependent variable and pairing it with other variables one at a time.
#Type 1: One numeric one categorical
#ggplot(data, aes(x = cat.variable, y = cont.variable, fill = y)) + geom_boxplot()
ggplot(cars, aes(x = Transport, y = Age, fill=Transport)) + geom_boxplot()
ggplot(cars, aes(x = Transport, y = Work.Exp, fill=Transport)) + geom_boxplot()
ggplot(cars, aes(x = Transport, y = Salary, fill=Transport)) + geom_boxplot()
ggplot(cars, aes(x = Transport, y = Distance, fill=Transport)) + geom_boxplot()

ggplot(cars, aes(x=Age, fill= Transport)) + geom_density(alpha=0.5)
ggplot(cars, aes(x=Work.Exp, fill= Transport)) + geom_density(alpha=0.5)
ggplot(cars, aes(x=Salary, fill= Transport)) + geom_density(alpha=0.5)
ggplot(cars, aes(x=Distance, fill= Transport)) + geom_density(alpha=0.5)


#Type 2: Both categorical
#ggplot(data, aes(x = x-variable)) + geom_bar(aes(fill = y-variable))
#ggplot(data, aes(x = x-variable)) + geom_bar(aes(fill = y-variable), position = "dodge")
ggplot(cars, aes(x = Transport)) + geom_bar(aes(fill = Gender), position = "dodge")
ggplot(cars, aes(x = Transport)) + geom_bar(aes(fill = Engineer), position = "dodge")
ggplot(cars, aes(x = Transport)) + geom_bar(aes(fill = MBA), position = "dodge")
ggplot(cars, aes(x = Transport)) + geom_bar(aes(fill = license), position = "dodge")

#Type 2: Both continuous?
#ggplot(data, aes(x = x-variable, y = y-variable)) + geom_point()
ggplot(cars, aes(x = Age, y = Work.Exp)) + geom_point()+ stat_smooth(method = "lm")
ggplot(cars, aes(x = Age, y = Salary)) + geom_point() + stat_smooth(method = "lm")
ggplot(cars, aes(x = Age, y = Distance)) + geom_point() + stat_smooth(method = "lm")
ggplot(cars, aes(x = Work.Exp, y = Salary)) + geom_point() + stat_smooth(method = "lm")
ggplot(cars, aes(x = Work.Exp, y = Distance)) + geom_point() + stat_smooth(method = "lm")
ggplot(cars, aes(x = Salary, y = Distance)) + geom_point() + stat_smooth(method = "lm")

ggplot(cars, aes(x = Age, y = Work.Exp, col = Transport)) + geom_point() + stat_smooth(method = "lm", col = "black") + facet_grid(~Transport)
ggplot(cars, aes(x = Age, y = Salary, col = Transport)) + geom_point() + stat_smooth(method = "lm", col = "black") + facet_grid(~Transport)
ggplot(cars, aes(x = Age, y = Distance, col = Transport)) + geom_point() + stat_smooth(method = "lm", col = "black") + facet_grid(~Transport)
ggplot(cars, aes(x = Work.Exp, y = Salary, col = Transport)) + geom_point() + stat_smooth(method = "lm", col = "black") + facet_grid(~Transport)
ggplot(cars, aes(x = Work.Exp, y = Distance, col = Transport)) + geom_point() + stat_smooth(method = "lm", col = "black") + facet_grid(~Transport)
ggplot(cars, aes(x = Salary, y = Distance, col = Transport)) + geom_point() + stat_smooth(method = "lm", col = "black") + facet_grid(~Transport)

#STDHA ggplot


#Merging the levels to Yes ( for Cars) and No (for Others)
#install.packages("forcats")
library(forcats)
cars$Transport = fct_collapse(cars$Transport, no = c("Public Transport","2Wheeler"), yes = "Car")
summary(cars)
#Splitting the dataset into numeric and categorical
colnames(cars[,sapply(cars, is.numeric)])
colnames(cars[,sapply(cars, is.factor)])

cars.num = cars[, sapply(cars, is.numeric)]
cars.cat = cars[, sapply(cars, is.factor)]

summary(cars.num)
summary(cars.cat)
dim(cars.num)
dim(cars.cat)

#Scaling the dataset before checking for multicollinearity
cars.num.sc = as.data.frame(scale(cars.num))
names(cars.num.sc)
#Checking for Multicollinearity
pairs(cars.num.sc, pch=15, col=c("darkblue","magenta","dark green"))
cormatrix = cor(cars.num.sc)
#install.packages("corrplot")
library(corrplot)
corrplot(cormatrix, method = "number", type="lower")

#install.packages("psych")
library(psych)
cortest.bartlett(cormatrix)
#KMO Test
KMO(cormatrix)
# PCA to find no: of factors
A = eigen(cormatrix)
ev = A$values
ev

plot(ev, xlab = "Factors", ylab="Eigen Value", pch=20, col="blue")
lines(ev, col="red")
#Scree plot says the elbow happens when no: of factors=2

# FA Analysis for the variables
eFactors1 = fa(cars.num.sc, nfactors=2, rotate="none")
eFactors2 = fa(cars.num.sc, nfactors=2, rotate="varimax")
eFactors3 = fa(cars.num.sc, nfactors=2, rotate="none", fm = "minres")
# wE WILL SELECT VARIMAX ROTATION

eFactors1
eFactors2
eFactors3

fa.diagram(eFactors1)
fa.diagram(eFactors2)
fa.diagram(eFactors3)

attributes(eFactors2)

#Factor_Scores
cars_fa <- eFactors2$scores


#Dataframe with new column names
cars_new <- data.frame(cars_fa)
head(cars_new)

colnames(cars_new) <- c("Emp.Profile","Distance")
names(cars_new)
dim(cars_new)

#Checking for Multicollinearity Again after FA
#cormatrix2 = cor(cars_new)
corrplot(cormatrix2, method = "number", type="lower")

#Merging the categorical variable to the new dataset
cars_all <- cbind(cars_new,cars.cat)
head(cars_all)
names(cars_all)
dim(cars_all)
str(cars_all)

#Data Preparation (SMOTE)
#install.packages('DMwR')
library(DMwR)
#table(cars_all$Transport)
# Split data into test and train datasets
set.seed(300)
library(caTools)
split = sample.split(cars_all, SplitRatio=0.80)
train_smote = subset(cars_all, split ==T)
test_smote = subset(cars_all, split==F)

dim(train_smote)
dim(test_smote)

table(train_smote$Transport)#46/270+46=0.14
table(test_smote$Transport)#15/113+15= 0.11

bal_cars_train <- SMOTE(Transport ~., train_smote, perc.over = 400, k = 5)

table(bal_cars_train$Transport) # 230/368+230=0.38

# we dont really need to use the perc.under here
#in SMOTE we have to define our equation
#perc.over means that 1 minority class will be added for every value of perc.over
#now we have increased the minority class. We are adding 4 for every minority class sample. - perc.over
# 46 + 46*4 = 230
#We are subtracting 10 for every 100 - perc.under. We are taking out of the majority class as well.

View(bal_cars_train)
#Applying Logistic Regression & Interpret results
#Build a logistic regression model on balanced data
LR.smote = glm(Transport ~., data = bal_cars_train, family = binomial(link="logit"))
summary(LR.smote)

#Remove insignificant variables
library(dplyr)
bal_cars_train_sub = bal_cars_train %>% select(-c("Gender", "Engineer", "MBA"))

cars_test_sub = test_smote %>% select(-c("Gender", "Engineer", "MBA"))


LR.smote.sub = glm(Transport ~ ., data = bal_cars_train_sub, family = binomial(link="logit"))
summary(LR.smote.sub)

cars_test_sub$log.pred = predict(LR.smote.sub, cars_test_sub[1:3], type = 'response')
#in the above line of code we have noted the results of the above two lines in the column called log.pred

table(cars_test_sub$log.pred>0.5,cars_test_sub$Transport)
#we are comapring the predicted values and given values. Anything above 0.5 will be a yes from the above code
#Confusion Matrix
library(caret)
cars_test_sub$pred <- ifelse(cars_test_sub$log.pred>0.5,"yes","no")
cars_test_sub$pred <- as.factor(cars_test_sub$pred)
confusionMatrix(cars_test_sub$pred,cars_test_sub$Transport)
###################################################################################

#Apply KNN  only on numeric variables
library(class)
predKNNmodel = knn(train = bal_cars_train[,c(1,2)], test = test_smote[,c(1,2)], cl = bal_cars_train[,7], k = 5, prob=TRUE)
tabKNN = table(predKNNmodel,test_smote[,7])
tabKNN 
#Confusion Matrix
confusionMatrix(predKNNmodel,test_smote[,7])

#Apply Naive bayes
library(e1071)
NBmodel = naiveBayes(Transport ~., data = bal_cars_train)
NBpredTest = predict(NBmodel, newdata = test_smote)
tabNB = table(NBpredTest,test_smote$Transport)
tabNB
#Confusion Matrix
confusionMatrix(NBpredTest,test_smote[,7])

# Remarks on Model validation exercise <Which model performed the best> ?
#Answer: Naive Bayes

#loading a few libraries
install.packages('gbm')
library(gbm)          # basic implementation using AdaBoost
    # a faster implementation of a gbm
install.packages('caret')
library(caret)        # an aggregator package for performing many machine learning models


## Applying Bagging

library(ipred)
library(rpart)

#we can modify the maxdepth and minsplit if needed
#r doc, https://www.rdocumentation.org/packages/ipred/versions/0.4-0/topics/bagging

cars.bagging <- bagging(Transport ~.,
data=bal_cars_train,
control=rpart.control(maxdepth=4, minsplit=4))

test_smote$pred.bag.class <- predict(cars.bagging, test_smote)

#test_smote$pred.class<- ifelse(test_smote$pred.class<0.5,0,1)
#confusionMatrix(data=factor(gd_test$pred.class),
               #reference=factor(gd_test$Class),
                #positive='1')

table(test_smote$pred.bag.class,test_smote$Transport)#we are comapring our class with our predicted values

confusionMatrix(test_smote$pred.bag.class,test_smote$Transport)
#This is a much better model than the model that we had built with KNN.
#Bagging can help us only so much when we are using a data set that is such imbalanced.

# Applying Boosting
install.packages('xgboost')
library(xgboost)  

cars_train_boost <- bal_cars_train
#Converting Traget variable to 1 & 0 to suit for bernouillis methos
cars_train_boost$Transport <- ifelse(cars_train_boost$Transport== "yes",1,0)

View(cars_train_boost)

gbm.fit <- gbm(
  formula = Transport ~ .,
  distribution = "bernoulli",#we are using bernoulli because we are doing a logistic and want probabilities
  data = cars_train_boost,
  n.trees = 6500, #these are the number of stumps
  interaction.depth = 1,#number of splits it has to perform on a tree (starting from a single node)
  shrinkage = 0.001,#shrinkage is used for reducing, or shrinking the impact of each additional fitted base-learner(tree)
  cv.folds = 5,#cross validation folds
  n.cores = NULL, # will use all cores by default
  verbose = FALSE#after every tree/stump it is going to show the error and how it is changing
) 

cars_test_boost <- test_smote[,-8]
head(cars_test_boost)
cars_test_boost$Transport <- ifelse(cars_test_boost$Transport== "yes",1,0)

View(cars_test_boost)

cars_test_boost$pred.class <- predict(gbm.fit, cars_test_boost, type = "response")

table(cars_test_boost$pred.class>0.5,cars_test_boost$Transport)

#cars_test_boost$Transport <- ifelse(cars_test_boost$Transport== "yes",1,0)
cars_test_boost$pred.class <- ifelse(cars_test_boost$pred.class >0.5,1,0)
confusionMatrix(as.factor(cars_test_boost$pred.class),as.factor(cars_test_boost$Transport))


